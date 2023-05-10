/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "ResultSet.h"

#include "eckit/exception/Exceptions.h"

#include <algorithm>
#include <string>
#include <iostream>

#include "Constants.h"
#include "VectorMath.h"


namespace Ingester {
namespace bufr {

    typedef SubsetLookupTable Frame;
    typedef std::vector<Frame> Frames;

    struct PipelineData
    {
    };


    class Step
    {
        void run();
    };

    typedef std::vector<std::shared_ptr<Step>> Steps;

    // Preprocessing Pipeline Steps

    struct PreprocessingData : public PipelineData
    {
        Target& target;
        Frame& frame;
    };

    class ResolveDims : public Step
    {
        void run(PipelineData& data);
    };

    class CheckJagged : public Step
    {
     public:
        void run(PipelineData& data);
        bool isJagged() { return jagged_; }

     private:
        bool jagged_;
    };

    class CollectDims : public Step
    {
        void run(PipelineData& data);
    };

    class CollectMissingFrames : public Step
    {
        CollectMissingFrames(size_t size) { missing_.resize(size); }

        void run(PipelineData& data);

     private:
        std::vector<char> missing_;

    };

    class ResolveTypeInfo : public Step
    {
        void run(PipelineData& data);
    };

    class ResolveDimPath : public Step
    {
        void run(PipelineData& data);
    };

    // Assembly Pipeline Steps

    class ResultPipeline

    {
    public:
        ResultPipeline(const Frames& frames, const Steps& steps) :
            frames_(frames),
            steps_(steps)
        {

        }

        void execute(PipelineData& data)
        {
            for (const auto& frame : frames_)
            {
                for (auto& step : steps_)
                {
                    step->run(data);
                }
            }
        }

    private:
        const Frames& frames_;
        const Steps steps_;
    };

s

    ResultSet::ResultSet()
    {
    }

    ResultSet::~ResultSet()
    {
    }

    std::shared_ptr<Ingester::DataObjectBase>
        ResultSet::get(const std::string& fieldName,
                       const std::string& groupByFieldName,
                       const std::string& overrideType) const
    {




        // Pre-Processing
        auto resolveDims = std::make_shared<ResolveDims>();
        auto checkJagged = std::make_shared<CheckJagged>();
        auto collectMissingFrames = std::make_shared<CollectMissingFrames>(frames_.size());
        auto resolveTypeInfo = std::make_shared<ResolveTypeInfo>();
        auto resolveDimPaths = std::make_shared<ResolveDimPath>();

        auto processingPipeline = ResultPipeline({ resolveDims,
                                             checkJagged,
                                             collectMissingFrames,
                                             resolveTypeInfo,
                                             resolveDimPaths });

        processingPipeline.execute(pipelineData);


//        // Assemble Result Data
//        auto assembleSteps = std::vector<std::shared_ptr<Step>>{};
//        if (checkJagged->isJagged())
//        {
//            assembleSteps.push_back(std::make_shared<AssembleJaggedData>());
//        }
//        else
//        {
//            assembleSteps.push_back(std::make_shared<AssembleData>());
//        }
//
//        assembleSteps.push_back(std::make_shared<ApplyQueryFilters>());
//        assembleSteps.push_back(std::make_shared<ApplyGroupBy>());
//
//        auto assemblePipeline = ResultPipeline(assembleSteps);
//        assemblePipeline.execute(pipelineData);




//        std::vector<double> data;
//        std::vector<int> dims;
//        std::vector<Query> dimPaths;
//        TypeInfo info;

//        getRawValues(fieldName,
//                     groupByFieldName,
//                     data,
//                     dims,
//                     dimPaths,
//                     info);

        auto object = makeDataObject(fieldName,
                                     groupByFieldName,
                                     resolveTypeInfo.getInfo(),
                                     overrideType,
                                     pipelineData.data,
                                     resolveDims.getDims(),
                                     resolveDimPaths.getDimPaths());

        return object;
    }

    void ResultSet::getRawValues(const std::string& fieldName,
                                 const std::string& groupByField,
                                 std::vector<double>& data,
                                 std::vector<int>& dims,
                                 std::vector<Query>& dimPaths,
                                 TypeInfo& info) const
    {
        // Make sure we have accumulated frames
        if (frames_.size() == 0)
        {
            throw eckit::BadValue("Result has no data.");
        }

        // The indices for targets are the same in all the frames. Get them, so we can quickly index
        // into the target arrays for all the frames.
        const bool hasGroupBy = !groupByField.empty();
        const size_t targetIdx = frames_.front().getTargetIdx(fieldName);
        const size_t groupByIdx = hasGroupBy ? frames_.front().getTargetIdx(groupByField) : 0;

        // Loop through the frames to determine the overall parameters for the result data. We will
        // want to find the dimension information and determine if the array could be jagged which
        // means we will need to do extra work later (otherwise we can quickly copy the data).
        bool jagged = false;
        dims = {0};  // Includes dims from binary repeats
        std::vector<char> missingFrame(frames_.size(), false);

        size_t frameIdx = 0;
        for (const auto& frame : frames_)
        {
            const auto &target = frame.targetAtIdx(targetIdx);

            // Resize the dims if necessary
            if (target->exportDimIdxs.size() > dims.size())
            {
                dims.resize(target->exportDimIdxs.size());

                if (&frame != &frames_.front())
                {
                    jagged = true;
                }
            }

            // Capture the dimensional information
            auto pathIdx = 0;
            auto exportIdxIdx = 0;
            for (auto p = target->path.begin(); p != target->path.end() - 1; ++p)
            {
                if (frame[p->nodeId].counts.empty())
                {
                    missingFrame[frameIdx] = true;
                    break;
                }

                if (target->exportDimIdxs[exportIdxIdx] != pathIdx)
                {
                    ++pathIdx;
                    continue;
                }

                const auto newDimVal = std::max(dims[exportIdxIdx], max(frame[p->nodeId].counts));

                if (!jagged)
                {
                    jagged = !allEqual(frame[p->nodeId].counts);

                    if (!jagged && dims[exportIdxIdx] != 0)
                    {
                        jagged = (dims[exportIdxIdx] != newDimVal);
                    }
                }

                dims[exportIdxIdx] = newDimVal;
                pathIdx++;
                exportIdxIdx++;
            }

            if (missingFrame[frameIdx]) { continue; }

            // Fill in the type information
            info.reference = std::min(info.reference, target->typeInfo.reference);
            info.bits = std::max(info.bits, target->typeInfo.bits);

            if (std::abs(target->typeInfo.scale) > info.scale)
            {
                info.scale = target->typeInfo.scale;
            }

            if (info.unit.empty()) info.unit = target->typeInfo.unit;


            // Fill in the dimPaths data
            if (!target->dimPaths.empty() &&
                dimPaths.size() < target->dimPaths.size())
            {
                dimPaths = target->dimPaths;
            }

            ++frameIdx;
        }

//        // print dims to std::cout
//        std::cout << "dims: ";
//        for (auto& d : dims)
//        {
//            std::cout << d << " ";
//        }
//        std::cout << std::endl;




//        // If groupByTarget exists check that is shares a common path with the target
//        if (groupByTarget)
//        {
//            auto groupByPathComps = splitPath(groupByTarget->dimPaths.back().str());
//            auto targetPathComps = splitPath(target->dimPaths.back().str());
//
//            for (size_t i = 1;
//                 i < std::min(groupByPathComps.size(), targetPathComps.size());
//                 i++)
//            {
//                if (targetPathComps[i] != groupByPathComps[i])
//                {
//                    std::ostringstream errStr;
//                    errStr << "The groupByField " << groupByField << " and the targetField "
//                           << fieldName << " do not share a common path. The groupByField path is "
//                           << groupByTarget->dimPaths.back().str() << " and the targetField path is"
//                           << " " << target->dimPaths.back().str();
//                    throw eckit::BadValue(errStr.str());
//                }
//            }
//        }
//
//        auto allDims = dimsList;
//        dims = dimsList;
//
//        // If there is absolutely no data for a field you will have the problem were the
//        // size of some dimensions are zero. We need to have at least 1 element in each
//        // dimension to make room for the missing value. This if statement makes sure there
//        // is at least 1 element in each dimension.
//        for (size_t dimIdx = 0; dimIdx < allDims.size(); ++dimIdx)
//        {
//            if (allDims[dimIdx] == 0)
//            {
//                allDims[dimIdx] = 1;
//            }
//        }
//
//
        int rowLength = 1;
        for (size_t dimIdx = 1; dimIdx < dims.size(); ++dimIdx)
        {
            rowLength *= dims[dimIdx];
        }

        // Allocate the output data
        auto totalRows = frames_.size();
        data.resize(totalRows * rowLength, MissingValue);

        // Copy the data fragments from the frames into the output data
        std::vector<std::vector<int>> inserts(dims.size());
        for (size_t frameIdx=0; frameIdx < frames_.size(); ++frameIdx)
        {
            const auto& frame = frames_[frameIdx];
            const auto& target = frame.targetAtIdx(targetIdx);
            const auto& fragment = frame[target->nodeIdx].data;

            if (jagged)
            {
                std::cout << "Found Jagged Array " << std::endl;

                std::vector<size_t> idxs(fragment.size());
                for (size_t i = 0; i < idxs.size(); ++i)
                {
                    idxs[i] = i;
                }

                for (size_t i = 0; i < dims.size(); ++i) { inserts[i] = {0}; }

                // Compute insert array
                for (size_t repIdx = 0;
                     repIdx < std::min(dims.size(), target->path.size());
                     ++repIdx)
                {
                    inserts[repIdx] = product<int>(dims.begin() + repIdx, dims.end()) -
                                      frame[target->path[repIdx].nodeId].counts *
                                      product<int>(dims.begin() + repIdx + 1, dims.end());
                }

                // Inflate the data, compute the idxs for each data element in the result array
                for (int dim_idx = dims.size() - 1; dim_idx >= 0; --dim_idx)
                {
                    for (size_t insert_idx = 0; insert_idx < inserts[dim_idx].size(); ++insert_idx)
                    {
                        size_t num_inserts = inserts[dim_idx][insert_idx];
                        if (num_inserts > 0)
                        {
                            int data_idx = product<int>(dims.begin() + dim_idx, dims.end()) *
                                           insert_idx + product<int>(dims.begin() + dim_idx, dims.end())
                                           - num_inserts - 1;

                            for (size_t i = 0; i < idxs.size(); ++i)
                            {
                                if (static_cast<int>(idxs[i]) > data_idx)
                                {
                                    idxs[i] += num_inserts;
                                }
                            }
                        }
                    }
                }

                for (size_t i = 0; i < idxs.size(); ++i)
                {
                    data[idxs[i] + frameIdx * rowLength] = fragment[i];
                }
            }
            else
            {
                std::copy(fragment.begin(), fragment.end(), data.begin() + frameIdx * rowLength);
            }
        }

        // Convert dims per data frame to dims for all the collected data.
        dims[0] = totalRows;
    }


    void ResultSet::padJaggedArray(std::shared_ptr<Target> target,
                                    std::vector<double>& data,
                                    size_t rowLength) const
    {
//        std::vector<size_t> idxs(targetField.data.size());
//        for (size_t i = 0; i < idxs.size(); ++i)
//        {
//            idxs[i] = i;
//        }
    }

    std::string ResultSet::unit(const std::string& fieldName) const
    {
//        const auto& targets = frames_[0].getTargets();
//        if (targets->find(fieldName) != targets->end())
//        {
//            return targets->at(fieldName)->unit;
//        }
//        else
//        {
//            throw eckit::BadParameter("Target not found for field \"" + fieldName + "\"");
//        }
    }

    std::shared_ptr<DataObjectBase> ResultSet::makeDataObject(
                                const std::string& fieldName,
                                const std::string& groupByFieldName,
                                TypeInfo& info,
                                const std::string& overrideType,
                                const std::vector<double> data,
                                const std::vector<int> dims,
                                const std::vector<Query> dimPaths) const
    {
        std::shared_ptr<DataObjectBase> object;
        if (overrideType.empty())
        {
            object = objectByTypeInfo(info);
        }
        else
        {
            object = objectByType(overrideType);

            if ((overrideType == "string" && !info.isString()) ||
                (overrideType != "string" && info.isString()))
            {
                std::ostringstream errMsg;
                errMsg << "Conversions between numbers and strings are not currently supported. ";
                errMsg << "See the export definition for \"" << fieldName << "\".";
                throw eckit::BadParameter(errMsg.str());
            }
        }

        object->setData(data, 10e10);
        object->setDims(dims);
        object->setFieldName(fieldName);
        object->setGroupByFieldName(groupByFieldName);
        object->setDimPaths(dimPaths);

        return object;
    }

    std::shared_ptr<DataObjectBase> ResultSet::objectByTypeInfo(TypeInfo &info) const
    {
        std::shared_ptr<DataObjectBase> object;

        if (info.isString())
        {
            object = std::make_shared<DataObject<std::string>>();
        }
        else if (info.isInteger())
        {
            if (info.isSigned())
            {
                if (info.is64Bit())
                {
                    object = std::make_shared<DataObject<int64_t>>();
                }
                else
                {
                    object = std::make_shared<DataObject<int32_t>>();
                }
            }
            else
            {
                if (info.is64Bit())
                {
                    object = std::make_shared<DataObject<uint64_t>>();
                }
                else
                {
                    object = std::make_shared<DataObject<uint32_t>>();
                }
            }
        }
        else
        {
            if (info.is64Bit())
            {
                object = std::make_shared<DataObject<double>>();
            }
            else
            {
                object = std::make_shared<DataObject<float>>();
            }
        }

        return object;
    }

    std::shared_ptr<DataObjectBase> ResultSet::objectByType(const std::string& overrideType) const
    {
        std::shared_ptr<DataObjectBase> object;

        if (overrideType == "int" || overrideType == "int32")
        {
            object = std::make_shared<DataObject<int32_t>>();
        }
        else if (overrideType == "float")
        {
            object = std::make_shared<DataObject<float>>();
        }
        else if (overrideType == "double")
        {
            object = std::make_shared<DataObject<double>>();
        }
        else if (overrideType == "string")
        {
            object = std::make_shared<DataObject<std::string>>();
        }
        else if (overrideType == "int64")
        {
            object = std::make_shared<DataObject<int64_t>>();
        }
        else
        {
            std::ostringstream errMsg;
            errMsg << "Unknown or unsupported type " << overrideType << ".";
            throw eckit::BadParameter(errMsg.str());
        }

        return object;
    }

    std::vector<std::string> ResultSet::splitPath(const std::string& path)
    {
        std::vector<std::string> components;
        std::string::size_type start = 0;
        std::string::size_type end = 0;

        while ((end = path.find('/', start)) != std::string::npos)
        {
            if (end != start)
            {
                components.push_back(path.substr(start, end - start));
            }

            start = end + 1;
        }

        if (start < path.size())
        {
            components.push_back(path.substr(start));
        }

        return components;
    }
}  // namespace bufr
}  // namespace Ingester
