/*
 * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory>
#include <vector>
#include <unordered_map>
#include <unordered_set>

#include "VectorMath.h"
#include "DataProvider/DataProvider.h"
#include "SubsetTable.h"
#include "Target.h"

namespace Ingester {
namespace bufr {

    namespace __details
    {
        /// \brief BUFR messages are indexed according to start and stop values that are dependant
        /// on the message itself (the indexing is a property of the message). This object allows
        /// lets you make an array where the indexing is offset with respect to the actual position
        /// of the object in the array.
        template <typename T>
        class OffsetArray
        {
        public:
            OffsetArray(size_t startIdx, size_t endIdx)
                : offset_(startIdx)
            {
                data_.resize(endIdx - startIdx + 1);
            }

            T& operator[](size_t idx) { return data_[idx - offset_]; }

        private:
            std::vector<T> data_;
            size_t offset_;
        };
    }  // namespace __details

    class DataWalker
    {
        typedef size_t NodeId;
        typedef std::vector<double> DataVector;
        typedef std::vector<std::vector<int>> CountsVector;

        typedef std::unordered_map<NodeId, std::vector<int>> CountsMap;
        typedef std::unordered_map<NodeId, DataVector> DataMap;
        typedef std::unordered_map<NodeId, TargetComponent> ComponentMap;

        struct DataElement
        {
        public:
            DataVector data;
            CountsVector counts;

            explicit DataElement() = default;

            size_t size() const
            {
                return data.size();
            }
        };

        typedef std::unordered_map<std::string, DataElement> TargetDataMap;

      public:
        DataWalker(const std::shared_ptr<DataProvider>& dataProvider) :
            dataProvider_(dataProvider)
        {}

        /// \brief Walk the data tree along the given path. For each repeated node in the path
        ///        (i.e. delayed or fixed repeat), record the counts in the counts vector vector, the
        ///        offsets for each block of data and the total number of data elements that will be
        ///        returned (product of the counts along the path).
        /// \param path The path to walk.
        /// \return DataNode containing the name of the node, the counts, offsets and size.

        TargetDataMap walk(const Targets& targets) const
        {
            TargetDataMap targetDataMap;

            CountsMap countsMap = getCountsMap(targets);
            DataMap dataMap = getDataMap(targets, countsMap);

            for (const auto& target : targets)
            {
                auto data = DataElement();
                data.counts = {};
                data.counts.resize(target->numDimensions);

                size_t depth = 0;
                for (size_t pathIdx = 0; pathIdx < target->path.size(); ++pathIdx)
                {
                    if (target->path[pathIdx].type == TargetComponent::Type::Repeat ||
                        target->path[pathIdx].type == TargetComponent::Type::Subset)
                    {
                        data.counts[depth] = countsMap[target->path[pathIdx].nodeId];
                        ++depth;
                    }
                }

                data.data = std::move(dataMap[target->nodeIdx]);

//                std::cout << "** " << target->name << " **"  << std::endl;
//
//                // print data to stdout
//                std::cout << "data.data = ";
//                for (auto i : data.data) {
//                    std::cout << i << " ";
//                }
//
//                std::cout << std::endl;
//
//                std::cout << "data.counts = ";
//                for (auto i : data.counts) {
//                    std::cout << "[";
//                    for (auto j : i) {
//                        std::cout << j << " ";
//                    }
//                    std::cout << "]";
//                }
//                std::cout << std::endl;
//                std::cout << std::endl;

                targetDataMap[target->name] = std::move(data);
            }

            return targetDataMap;
        }


     private:
        std::shared_ptr<DataProvider> dataProvider_;

        CountsMap getCountsMap(const Targets& targets) const
        {
            CountsMap countsMap;
            ComponentMap componentMap;

            for (const auto& target : targets)
            {
                for (const auto& path : target->path)
                {
                    if (path.type == TargetComponent::Type::Repeat ||
                        path.type == TargetComponent::Type::Subset)
                    {
                        countsMap[path.nodeId] = {};
                        componentMap[path.nodeId] = path;
                    }
                }
            }

            for (size_t cursor = 1; cursor <= dataProvider_->getNVal(); ++cursor)
            {
                auto nodeId = static_cast<size_t>(dataProvider_->getInv(cursor));
                if (countsMap.find(nodeId) != countsMap.end())
                {
                    const auto& component = componentMap[nodeId];

                    if (component.type == TargetComponent::Type::Subset)
                    {
                        countsMap[nodeId].push_back(1);
                    }
                    else if (component.fixedRepeatCount > 1)
                    {
                        countsMap[nodeId].push_back(component.fixedRepeatCount);
                    }
                    else
                    {
                        countsMap[nodeId].push_back(dataProvider_->getVal(cursor));
                    }
                }
            }

            return countsMap;
        }

        DataMap getDataMap(const Targets& targets, const CountsMap& countsMap) const
        {
            DataMap dataMap;
            std::unordered_map<size_t, size_t> valIdxMap;

            for (const auto& target : targets)
            {
                const auto& path = target->path.back();
                dataMap[target->nodeIdx].resize(product(countsMap.at(path.parentNodeId)));
                valIdxMap[target->nodeIdx] = 0;
            }

            for (size_t cursor = 1; cursor <= dataProvider_->getNVal(); ++cursor)
            {
                const auto nodeId = dataProvider_->getInv(cursor);
                if (dataMap.find(nodeId) != dataMap.end())
                {
                    dataMap[nodeId][valIdxMap[nodeId]] = dataProvider_->getVal(cursor);
                    ++valIdxMap[nodeId];
                }
            }

            return dataMap;
        }
    };
}  // namespace bufr
}  // namespace Ingester
