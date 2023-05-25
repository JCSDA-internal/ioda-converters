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
        // Make sure we have accumulated frames otherwise something is wrong.
        if (frames_.size() == 0)
        {
            throw eckit::BadValue("ResultSet has no data.");
        }

        // Get the metadata for the target
        const auto targetMetaData = analyzeTarget(fieldName);

        // Assemble Result Data
        const auto data = assembleData(targetMetaData);

//        // Apply groupby
//        if (!groupByFieldName.empty())
//        {
//            const auto groupByMetaData = analyzeTarget(groupByFieldName);
//            validateGroupByField(targetMetaData, groupByMetaData);
//
//            if (groupByMetaData->dims.size() > targetMetaData->dims.size())
//            {
//            }
//            else if (groupByMetaData->dims.size() < targetMetaData->dims.size())
//            {
//            }
//        }

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

            if (target->path.size() == 0)
            {
                metaData->missingFrames[frameIdx] = true;
                ++frameIdx;
                continue;
            }

            if (target->path.size() - 1 > metaData->rawDims.size())
            {
                metaData->rawDims.resize(target->path.size() -  1);
            }

            // Resize the dims if necessary
            // Jagged if the dims need a resize (skip first one)
            if (target->exportDimIdxs.size() > metaData->dims.size())
            {
                metaData->dims.resize(target->exportDimIdxs.size());
                metaData->filteredDims.resize(target->exportDimIdxs.size(), 0);

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

                metaData->rawDims[pathIdx] = std::max(metaData->rawDims[pathIdx],
                                                      std::max(max(frame[p->nodeId].counts), 1));

                if (target->exportDimIdxs[exportIdxIdx] != pathIdx)
                {
                    ++pathIdx;
                    continue;
                }

                const auto newDimVal = std::max(metaData->dims[exportIdxIdx],
                                                std::max(max(frame[p->nodeId].counts), 1));

                if (!metaData->jagged)
                {
                    metaData->jagged = !allEqual(frame[p->nodeId].counts);

                    if (!metaData->jagged && metaData->dims[exportIdxIdx] != 0)
                    {
                        metaData->jagged = (metaData->dims[exportIdxIdx] != newDimVal);
                    }
                }

                metaData->dims[exportIdxIdx] = newDimVal;

                // Capture the filtered dimension information
                if (!p->queryComponent->filter.empty())
                {
                    metaData->filteredDims[exportIdxIdx] =
                        std::max(metaData->filteredDims[exportIdxIdx],
                        static_cast<int> (p->queryComponent->filter.size()));
                }

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

        // Fill the filtered dims array with the raw dims for elements that are not filtered
        for (size_t dimIdx = 0; dimIdx < metaData->filteredDims.size(); ++dimIdx)
        {
            if (metaData->filteredDims[dimIdx] == 0)
            {
                metaData->filteredDims[dimIdx] = metaData->dims[dimIdx];
            }
        }

        return metaData;
    }

    details::Data ResultSet::assembleData(const details::TargetMetaDataPtr& metaData) const
    {
        int rowLength = 1;
        for (size_t dimIdx = 1; dimIdx < metaData->rawDims.size(); ++dimIdx)
        {
            rowLength *= metaData->rawDims[dimIdx];
        }

        // Allocate the output data
        auto totalRows = frames_.size();
        auto data = details::Data();
        data.buffer.resize(totalRows * rowLength, MissingValue);
        data.dims = metaData->dims;
        data.rawDims = metaData->rawDims;

        // Update the dims to reflect the actual size of the data
        data.dims[0] = totalRows;
        data.rawDims[0] = totalRows;

        bool needsFiltering = false;

        // Copy the data fragments into the raw data array.
        for (size_t frameIdx=0; frameIdx < frames_.size(); ++frameIdx)
        {
            if (metaData->missingFrames[frameIdx])
            {
                continue;
            }

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

            if (target->usesFilters) needsFiltering = true;
        }

        if (needsFiltering)
        {
            int filteredRowLength = 1;
            for (size_t dimIdx = 1; dimIdx < metaData->filteredDims.size(); ++dimIdx)
            {
                filteredRowLength *= metaData->filteredDims[dimIdx];
            }

            auto filteredData = details::Data();
            filteredData.buffer.resize(totalRows * filteredRowLength, MissingValue);

            size_t outputOffset = 0;
            for (size_t frameIdx = 0; frameIdx < frames_.size(); ++frameIdx)
            {
                const auto &frame = frames_[frameIdx];
                const auto &target = frame.targetAtIdx(metaData->targetIdx);

                size_t inputOffset = frameIdx * rowLength;
                copyFilteredData(filteredData, data, target, inputOffset, outputOffset, 1, false);
            }

            filteredData.dims.resize(metaData->filteredDims.size());
            filteredData.dims[0] = data.dims[0];
            for (size_t dimIdx = 1; dimIdx < metaData->filteredDims.size(); ++dimIdx)
            {
                filteredData.dims[dimIdx] = metaData->filteredDims[dimIdx];
            }

            data = std::move(filteredData);
        }

        return data;
    }

    void ResultSet::copyJaggedData(details::Data& data,
                                   const Frame& frame,
                                   const TargetPtr& target,
                                   size_t offset,
                                   size_t dimIdx) const
    {
        size_t totalDimSize = 1;
        for (size_t i = dimIdx + 1; i < data.dims.size(); ++i)
        {
            totalDimSize *= data.dims[i];
        }

        if (!totalDimSize ||
            dimIdx >= data.dims.size() ||
            frame[target->nodeIdx].data.size() == 0) return;

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
                const auto count = static_cast<size_t>
                    (frame[target->path[pathIdx].nodeId].counts[countIdx]);

                std::copy(fragment.begin() + fragStartIdx,
                          fragment.begin() + fragStartIdx + count,
                          data.buffer.begin() + offset);

                fragStartIdx += count;
            }

            offset += totalDimSize;
        }
    }

    void ResultSet::validateGroupByField(const details::TargetMetaDataPtr& targetMetaData,
                                         const details::TargetMetaDataPtr& groupByMetaData) const
    {
        // Validate the groupby field is in the same path as the field
        auto& groupByPath = groupByMetaData->dimPaths.back();
        auto& targetPath = targetMetaData->dimPaths.back();

        auto groupByPathComps = splitPath(groupByPath.str());
        auto targetPathComps = splitPath(targetPath.str());

        for (size_t i = 1;
             i < std::min(groupByPathComps.size(), targetPathComps.size());
             i++)
        {
            if (targetPathComps[i] != groupByPathComps[i])
            {
                std::ostringstream errStr;
                errStr << "The GroupBy and Target Fields do not share a common path.\n";
                errStr << "GroupByField path: " << groupByPath.str()<< std::endl;
                errStr << "TargetField path: " << targetPath.str() << std::endl;
                throw eckit::BadParameter(errStr.str());
            }
        }
    }

    void ResultSet:: copyFilteredData(details::Data& resData,
                                      const details::Data& srcData,
                                      const TargetPtr& target,
                                      size_t& inputOffset,
                                      size_t& outputOffset,
                                      size_t depth,
                                      bool skipResult) const
    {
        if (depth == target->path.size() - 1)
        {
            if (!skipResult)
            {
//                std::cout << outputOffset << " " << srcData.buffer[inputOffset] << std::endl;
//                //print srcData::buffer to std::cout
//
//                for (size_t i = 0; i < srcData.buffer.size(); i++)
//                {
//                    std::cout << srcData.buffer[i] << " ";
//                }
//                std::cout << std::endl;

                resData.buffer[outputOffset] = srcData.buffer[inputOffset];
                outputOffset++;
            }

            inputOffset++;

            return;
        }

        auto& layerFilter = target->path[depth].queryComponent->filter;

        if (layerFilter.empty())
        {
            for (size_t countIdx = 0; countIdx < srcData.rawDims.size(); countIdx++)
            {
                copyFilteredData(
                    resData, srcData, target, inputOffset, outputOffset, depth + 1, skipResult);
            }
        }
        else
        {
//            std::cout << depth << " " << srcData.rawDims[0] << " " << srcData.rawDims[1] << std::endl;
            for (size_t count = 1; count <= static_cast<size_t>(srcData.rawDims[depth]); count++)
            {
                bool skip = skipResult;

                if (!skip)
                {
                    skip = std::find(layerFilter.begin(),
                                     layerFilter.end(),
                                     count) == layerFilter.end();
                }

                copyFilteredData(
                    resData, srcData, target, inputOffset, outputOffset, depth + 1, skip);
            }
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
