/*
 * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "SubsetLookupTable.h"


namespace Ingester {
namespace bufr {
    SubsetLookupTable::SubsetLookupTable(const std::shared_ptr<DataProvider>& dataProvider,
                                         const std::shared_ptr<Targets>& targets) :
        targets_(targets),
        lookupTable_(makeLookupTable(dataProvider, *targets))
    {
    }

    SubsetLookupTable::LookupTable
    SubsetLookupTable::makeLookupTable(const std::shared_ptr<DataProvider>& dataProvider,
                                       const Targets& targets) const
    {
        auto lookupTable = LookupTable(dataProvider->getInode(),
                                       dataProvider->getIsc(dataProvider->getInode()));

        auto lookupMetaTable = LookupMetaTable(dataProvider->getInode(),
                                               dataProvider->getIsc(dataProvider->getInode()));

        // Populate the lookup table with the counts and data corresponding to each BUFR node
        // we care about.
        addCounts(dataProvider, targets, lookupTable, lookupMetaTable);
        addData(dataProvider, targets, lookupTable, lookupMetaTable);

        return lookupTable;
    }

    void SubsetLookupTable::addCounts(const std::shared_ptr<DataProvider>& dataProvider,
                                      const Targets &targets,
                                      LookupTable& lookup,
                                      LookupMetaTable& lookupMeta) const
    {
        // Add entries for all the path nodes in the targets that are containers (can contain)
        // children. Uses merged data from the Subset metadata and Query strings.
        for (const auto& target : targets)
        {
            for (const auto& path : target->path)
            {
                if (path.isContainer())
                {
                    lookupMeta[path.nodeId].component = path;
                    lookupMeta[path.nodeId].collectedCounts = true;
                }
            }
        }

        // Collect all the counts for the nodes that were flagged from the BUFR subset data section.
        for (size_t cursor = 1; cursor <= dataProvider->getNVal(); ++cursor)
        {
            auto nodeId = static_cast<size_t>(dataProvider->getInv(cursor));
            if (lookupMeta[nodeId].collectedCounts)
            {
                const auto &component = lookupMeta[nodeId].component;

                if (component.type == TargetComponent::Type::Subset)
                {
                    // Subsets always have a count of 1.
                    lookup[nodeId].counts.push_back(1);
                }
                else if (component.fixedRepeatCount > 1)
                {
                    // Fixed repeat counts are stored in the component.
                    lookup[nodeId].counts.push_back(component.fixedRepeatCount);
                }
                else
                {
                    // Otherwise, the count is stored in the val array.
                    lookup[nodeId].counts.push_back(dataProvider->getVal(cursor));
                }
            }
        }
    }

    void SubsetLookupTable::addData(const std::shared_ptr<DataProvider>& dataProvider,
                                    const Targets &targets,
                                    LookupTable& lookup,
                                    LookupMetaTable& lookupMeta) const
    {
        // Reserve space for the data in the lookup table by summing the counts for each node.
        for (const auto& target : targets)
        {
            if (target->nodeIdx == 0) { continue; }
            const auto &path = target->path.back();

            lookup[target->nodeIdx].data.reserve(sum(lookup[path.parentDimensionNodeId].counts));
            lookupMeta[target->nodeIdx].collectedData = true;
            lookup[target->nodeIdx].data.isLongStr(target->typeInfo.isLongString());
            lookupMeta[target->nodeIdx].longStrId = target->longStrId;
        }

        for (size_t cursor = 1; cursor <= dataProvider->getNVal(); ++cursor)
        {
            const auto nodeId = dataProvider->getInv(cursor);
            if (lookupMeta[nodeId].collectedData)
            {
                if (lookup[nodeId].data.isLongStr())
                {
                    auto longStr = dataProvider->getLongStr(lookupMeta[nodeId].longStrId);
                    lookup[nodeId].data.push_back(longStr);
                }
                else
                {
                    lookup[nodeId].data.push_back(dataProvider->getVal(cursor));
                }
            }
        }
    }
}  // namespace bufr
}  // namespace Ingester
