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
        const auto targetMetaData = analyzeTarget(fieldName);

        // Assemble Result Data
        const auto data = assembleData(targetMetaData);


        auto object = makeDataObject(fieldName,
                                     groupByFieldName,
                                     targetMetaData->typeInfo,
                                     overrideType,
                                     data.buffer,
                                     data.dims,
                                     targetMetaData->dimPaths);

        return object;
    }

    details::TargetMetaDataPtr ResultSet::analyzeTarget(const std::string& name) const
    {
        auto metaData = std::make_shared<details::TargetMetaData>();
        metaData->targetIdx = frames_.front().getTargetIdx(name);
        metaData->missingFrames.resize(frames_.size(), false);

        // Loop through the frames to determine the overall parameters for the result data. We will
        // want to find the dimension information and determine if the array could be jagged which
        // means we will need to do extra work later (otherwise we can quickly copy the data).
        size_t frameIdx = 0;
        for (const auto& frame : frames_)
        {
            const auto &target = frame.targetAtIdx(metaData->targetIdx);

            // Resize the dims if necessary
            // Jagged if the dims need a resize (skip first one)
            if (target->exportDimIdxs.size() > metaData->dims.size())
            {
                metaData->dims.resize(target->exportDimIdxs.size());

                if (&frame != &frames_.front())
                {
                    metaData->jagged = true;
                }
            }

            // Capture the dimensional information
            auto pathIdx = 0;
            auto exportIdxIdx = 0;
            for (auto p = target->path.begin(); p != target->path.end() - 1; ++p)
            {
                if (frame[p->nodeId].counts.empty())
                {
                    metaData->missingFrames[frameIdx] = true;
                    break;
                }

                if (target->exportDimIdxs[exportIdxIdx] != pathIdx)
                {
                    ++pathIdx;
                    continue;
                }

                const auto newDimVal = std::max(metaData->dims[exportIdxIdx], max(frame[p->nodeId].counts));

                if (!metaData->jagged)
                {
                    metaData->jagged = !allEqual(frame[p->nodeId].counts);

                    if (!metaData->jagged && metaData->dims[exportIdxIdx] != 0)
                    {
                        metaData->jagged = (metaData->dims[exportIdxIdx] != newDimVal);
                    }
                }

                metaData->dims[exportIdxIdx] = newDimVal;
                pathIdx++;
                exportIdxIdx++;
            }

            if (metaData->missingFrames[frameIdx]) { continue; }

            // Fill in the type information
            metaData->typeInfo.reference =
                std::min(metaData->typeInfo.reference, target->typeInfo.reference);
            metaData->typeInfo.bits = std::max(metaData->typeInfo.bits, target->typeInfo.bits);

            if (std::abs(target->typeInfo.scale) > metaData->typeInfo.scale)
            {
                metaData->typeInfo.scale = target->typeInfo.scale;
            }

            if (metaData->typeInfo.unit.empty()) metaData->typeInfo.unit = target->typeInfo.unit;


            // Fill in the dimPaths data
            if (!target->dimPaths.empty() &&
                metaData->dimPaths.size() < target->dimPaths.size())
            {
                metaData->dimPaths = target->dimPaths;
            }

            ++frameIdx;
        }

        return metaData;
    }

    details::Data ResultSet::assembleData(const details::TargetMetaDataPtr& metaData) const
    {
        int rowLength = 1;
        for (size_t dimIdx = 1; dimIdx < metaData->dims.size(); ++dimIdx)
        {
            rowLength *= metaData->dims[dimIdx];
        }

        // Allocate the output data
        auto totalRows = frames_.size();
        auto data = details::Data();
        data.buffer.resize(totalRows * rowLength, MissingValue);
        data.dims = metaData->dims;

        // Copy the data fragments from the frames into the output data
        for (size_t frameIdx=0; frameIdx < frames_.size(); ++frameIdx)
        {
            const auto& frame = frames_[frameIdx];
            const auto& target = frame.targetAtIdx(metaData->targetIdx);

            if (metaData->jagged)
            {
                copyJaggedData(data, frame, target, frameIdx * rowLength, 0);
            }
            else
            {
                const auto& fragment = frame[target->nodeIdx].data;
                std::copy(fragment.begin(),
                          fragment.end(),
                          data.buffer.begin() + frameIdx * rowLength);
            }
        }

        // Update the dims to reflect the actual size of the data
        data.dims[0] = totalRows;

        return data;
    }

    void ResultSet::copyJaggedData(details::Data& data,
                                   const Frame& frame,
                                   const TargetPtr& target,
                                   size_t offset,
                                   size_t dimIdx) const
    {
        if (dimIdx >= data.dims.size()) return;

        size_t totalDimSize = 1;
        for (size_t i = dimIdx + 1; i < data.dims.size(); ++i)
        {
            totalDimSize *= data.dims[i];
        }

        size_t fragStartIdx = 0;
        const auto pathIdx = target->exportDimIdxs[dimIdx];
        for (size_t countIdx = 0;
             countIdx < frame[target->path[pathIdx].nodeId].counts.size();
             ++countIdx)
        {
            copyJaggedData(data, frame, target, offset, dimIdx + 1);

            // When we reach the last layer of counts then copy the data
            if (dimIdx == data.dims.size() - 1)
            {
                const auto& fragment = frame[target->nodeIdx].data;
                const auto count = frame[target->path[pathIdx].nodeId].counts[countIdx];
                std::copy(fragment.begin() + fragStartIdx,
                          fragment.begin() + fragStartIdx + count,
                          data.buffer.begin() + offset);

                fragStartIdx += count;
            }

            offset += totalDimSize;
        }
    }

    std::string ResultSet::unit(const std::string& fieldName) const
    {
        const auto targetIdx = frames_.front().getTargetIdx(fieldName);
        const auto& target = frames_.front().targetAtIdx(targetIdx);
        return target->typeInfo.unit;
    }

    std::shared_ptr<DataObjectBase> ResultSet::makeDataObject(
                                const std::string& fieldName,
                                const std::string& groupByFieldName,
                                const TypeInfo& info,
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

    std::shared_ptr<DataObjectBase> ResultSet::objectByTypeInfo(const TypeInfo &info) const
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
