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
        typedef std::vector<int> CountsVector;
        typedef std::vector<CountsVector> Counts;

        struct DataElement
        {
            DataVector data;
            Counts counts;
        };

        struct NodeData
        {
            DataVector data;
            CountsVector counts;
            TargetComponent component;
            bool isCollected = false;
        };

        typedef __details::OffsetArray<NodeData> LookupTable;
        typedef std::unordered_map<std::string, DataElement> DataMap;

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

        DataMap walk(const Targets& targets) const
        {
            auto dataMap = DataMap(targets.size());
            auto lookup = makeLookTable(targets);

            for (const auto& target : targets)
            {
                auto data = DataElement();
                data.counts = {};
                data.counts.resize(target->numDimensions);

                size_t depth = 0;
                for (size_t pathIdx = 0; pathIdx < target->path.size(); ++pathIdx)
                {
                    if (target->path[pathIdx].addsDimension())
                    {
                        data.counts[depth] = lookup[target->path[pathIdx].nodeId].counts;
                        ++depth;
                    }
                }

                data.data = lookup[target->nodeIdx].data;
                dataMap[target->name] = std::move(data);
            }

            return dataMap;
        }


     private:
        std::shared_ptr<DataProvider> dataProvider_;

        LookupTable makeLookTable(const Targets& targets) const
        {
            auto lookupTable = LookupTable(dataProvider_->getInode(),
                                           dataProvider_->getIsc(dataProvider_->getInode()));

            addCounts(targets, lookupTable);
            addData(targets, lookupTable);

            return lookupTable;
        }

        void addCounts(const Targets& targets, LookupTable& lookup) const
        {
            for (const auto& target : targets)
            {
                for (const auto& path : target->path)
                {
                    if (path.type == TargetComponent::Type::Repeat ||
                        path.type == TargetComponent::Type::Subset)
                    {
                        lookup[path.nodeId].counts = {};
                        lookup[path.nodeId].component = path;
                        lookup[path.nodeId].isCollected = true;
                    }
                }
            }

            for (size_t cursor = 1; cursor <= dataProvider_->getNVal(); ++cursor)
            {
                auto nodeId = static_cast<size_t>(dataProvider_->getInv(cursor));
                if (lookup[nodeId].isCollected)
                {
                    const auto& component = lookup[nodeId].component;

                    if (component.type == TargetComponent::Type::Subset)
                    {
                        lookup[nodeId].counts.push_back(1);
                    }
                    else if (component.fixedRepeatCount > 1)
                    {
                        lookup[nodeId].counts.push_back(component.fixedRepeatCount);
                    }
                    else
                    {
                        lookup[nodeId].counts.push_back(dataProvider_->getVal(cursor));
                    }
                }
            }
        }

        void addData(const Targets& targets, LookupTable& lookup) const
        {
            std::unordered_set<size_t> validNodeIds;
            validNodeIds.reserve(targets.size());

            for (const auto& target : targets)
            {
                const auto& path = target->path.back();
                lookup[target->nodeIdx].data.reserve(product(lookup[path.parentNodeId].counts));
                validNodeIds.insert(target->nodeIdx);
            }

            for (size_t cursor = 1; cursor <= dataProvider_->getNVal(); ++cursor)
            {
                const auto nodeId = dataProvider_->getInv(cursor);

                if (validNodeIds.find(nodeId) != validNodeIds.end())
                {
                    lookup[nodeId].data.push_back(dataProvider_->getVal(cursor));
                }
            }
        }
    };
}  // namespace bufr
}  // namespace Ingester
