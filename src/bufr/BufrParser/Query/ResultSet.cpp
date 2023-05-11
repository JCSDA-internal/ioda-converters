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

    std::shared_ptr<Ingester::DataObjectBase>
        ResultSet::get(const std::string& fieldName,
                       const std::string& groupByFieldName,
                       const std::string& overrideType) const
    {
        // Make sure we have accumulated frames otherwise somethings wrong.
        if (frames_.size() == 0)
        {
            throw eckit::BadValue("Result has no data.");
        }

        // Get the metadata for the targets
        auto targetMetaData = analyzeTarget(fieldName);

        // Assemble Result Data
        const auto data = assembleData(targetMetaData);


        auto object = makeDataObject(fieldName,
                                     groupByFieldName,
                                     targetMetaData.typeInfo,
                                     overrideType,
                                     data,
                                     targetMetaData.dims,
                                     targetMetaData.dimPaths);

        return object;
    }

    details::TargetMetaData ResultSet::analyzeTarget(const std::string& name) const
    {
        details::TargetMetaData metaData;
        metaData.targetIdx = frames_.front().getTargetIdx(name);
        metaData.missingFrames.resize(frames_.size(), false);

        // Loop through the frames to determine the overall parameters for the result data. We will
        // want to find the dimension information and determine if the array could be jagged which
        // means we will need to do extra work later (otherwise we can quickly copy the data).
        size_t frameIdx = 0;
        for (const auto& frame : frames_)
        {
            const auto &target = frame.targetAtIdx(metaData.targetIdx);

            // Resize the dims if necessary
            // Jagged if the dims need a resize (skip first one)
            if (target->exportDimIdxs.size() > metaData.dims.size())
            {
                metaData.dims.resize(target->exportDimIdxs.size());

                if (&frame != &frames_.front())
                {
                    metaData.jagged = true;
                }
            }

            // Capture the dimensional information
            auto pathIdx = 0;
            auto exportIdxIdx = 0;
            for (auto p = target->path.begin(); p != target->path.end() - 1; ++p)
            {
                if (frame[p->nodeId].counts.empty())
                {
                    metaData.missingFrames[frameIdx] = true;
                    break;
                }

                if (target->exportDimIdxs[exportIdxIdx] != pathIdx)
                {
                    ++pathIdx;
                    continue;
                }

                const auto newDimVal = std::max(metaData.dims[exportIdxIdx], max(frame[p->nodeId].counts));

                if (!metaData.jagged)
                {
                    metaData.jagged = !allEqual(frame[p->nodeId].counts);

                    if (!metaData.jagged && metaData.dims[exportIdxIdx] != 0)
                    {
                        metaData.jagged = (metaData.dims[exportIdxIdx] != newDimVal);
                    }
                }

                metaData.dims[exportIdxIdx] = newDimVal;
                pathIdx++;
                exportIdxIdx++;
            }

            if (metaData.missingFrames[frameIdx]) { continue; }

            // Fill in the type information
            metaData.typeInfo.reference =
                std::min(metaData.typeInfo.reference, target->typeInfo.reference);
            metaData.typeInfo.bits = std::max(metaData.typeInfo.bits, target->typeInfo.bits);

            if (std::abs(target->typeInfo.scale) > metaData.typeInfo.scale)
            {
                metaData.typeInfo.scale = target->typeInfo.scale;
            }

            if (metaData.typeInfo.unit.empty()) metaData.typeInfo.unit = target->typeInfo.unit;


            // Fill in the dimPaths data
            if (!target->dimPaths.empty() &&
                metaData.dimPaths.size() < target->dimPaths.size())
            {
                metaData.dimPaths = target->dimPaths;
            }

            ++frameIdx;
        }

        return metaData;
    }

    details::Data ResultSet::assembleData(details::TargetMetaData& metaData) const
    {
        int rowLength = 1;
        for (size_t dimIdx = 1; dimIdx < metaData.dims.size(); ++dimIdx)
        {
            rowLength *= metaData.dims[dimIdx];
        }

        // Allocate the output data
        auto totalRows = frames_.size();
        auto data = details::Data(totalRows * rowLength, MissingValue);

        // Copy the data fragments from the frames into the output data
        std::vector<std::vector<int>> inserts(metaData.dims.size());
        for (size_t frameIdx=0; frameIdx < frames_.size(); ++frameIdx)
        {
            const auto& frame = frames_[frameIdx];
            const auto& target = frame.targetAtIdx(metaData.targetIdx);
            const auto& fragment = frame[target->nodeIdx].data;

            if (metaData.jagged)
            {
                std::cout << "Found Jagged Array " << std::endl;

                std::vector<size_t> idxs(fragment.size());
                for (size_t i = 0; i < idxs.size(); ++i)
                {
                    idxs[i] = i;
                }

                for (size_t i = 0; i < metaData.dims.size(); ++i) { inserts[i] = {0}; }

                // Compute insert array
                for (size_t repIdx = 0;
                     repIdx < std::min(metaData.dims.size(), target->path.size());
                     ++repIdx)
                {
                    inserts[repIdx] = product<int>(metaData.dims.begin() + repIdx, metaData.dims.end()) -
                                      frame[target->path[repIdx].nodeId].counts *
                                      product<int>(metaData.dims.begin() + repIdx + 1, metaData.dims.end());
                }

                // Inflate the data, compute the idxs for each data element in the result array
                for (int dim_idx = metaData.dims.size() - 1; dim_idx >= 0; --dim_idx)
                {
                    for (size_t insert_idx = 0; insert_idx < inserts[dim_idx].size(); ++insert_idx)
                    {
                        size_t num_inserts = inserts[dim_idx][insert_idx];
                        if (num_inserts > 0)
                        {
                            int data_idx = product<int>(metaData.dims.begin() + dim_idx, metaData.dims.end()) *
                                           insert_idx + product<int>(metaData.dims.begin() + dim_idx,metaData.dims.end())
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

        // Update the dims to reflect the actual size of the data
        metaData.dims[0] = totalRows;

        return data;
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
