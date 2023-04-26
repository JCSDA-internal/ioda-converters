/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include "QueryRunner.h"

#include "eckit/exception/Exceptions.h"
#include "oops/util/Logger.h"

#include <string>
#include <iostream>
#include <memory>

#include "Constants.h"
#include "SubsetTable.h"
#include "VectorMath.h"
#include "SubsetLookupTable.h"


namespace Ingester {
namespace bufr
{
    QueryRunner::QueryRunner(const QuerySet &querySet,
                             ResultSet &resultSet,
                             const DataProviderType &dataProvider) :
        querySet_(querySet),
        resultSet_(resultSet),
        dataProvider_(dataProvider)
    {
    }

    void QueryRunner::accumulate()
    {
        Targets targets;
        findTargets(targets);

        resultSet_.addTarget(dataProvider_->getSubsetVariant(), targets);
        resultSet_.addFrame(SubsetLookupTable(dataProvider_, targets));
    }

    void QueryRunner::findTargets(Targets &targets)
    {
        // Check if the target list for this subset is cached
        if (targetCache_.find(dataProvider_->getSubsetVariant()) != targetCache_.end())
        {
            targets = targetCache_.at(dataProvider_->getSubsetVariant());
            return;
        }

        auto table = SubsetTable(dataProvider_);

        for (const auto &name : querySet_.names())
        {
            // Find the table node for the query. Loop through all the sub-queries until you find
            // one.
            Query foundQuery;
            std::shared_ptr<BufrNode> tableNode;
            for (const auto &query : querySet_.queriesFor(name))
            {
                if (query.subset->isAnySubset ||
                    (query.subset->name == dataProvider_->getSubsetVariant().subset &&
                     query.subset->index == dataProvider_->getSubsetVariant().variantId))
                {
                    tableNode = table.getNodeForPath(query.path);
                    foundQuery = query;
                    if (tableNode != nullptr) break;
                }
            }

            auto target = std::make_shared<Target>();

            // There was no corresponding table node for any of the sub-queries so create empty
            // target.
            if (tableNode == nullptr)
            {
                // Create empty target
                target->name = name;
                target->nodeIdx = 0;
                target->queryStr = querySet_.queriesFor(name)[0].str();
                target->dimPaths.push_back({Query()});
                target->typeInfo = TypeInfo();
                target->exportDimIdxs = {0};
                targets.push_back(target);

                // Print message to inform the user of the missing targetz
                oops::Log::warning() << "Warning: Query String ";
                oops::Log::warning() << querySet_.queriesFor(name)[0].str();
                oops::Log::warning() << " didn't apply to subset ";
                oops::Log::warning() << dataProvider_->getSubsetVariant().str();
                oops::Log::warning() << std::endl;

                continue;
            }

            // Create the target
            target->name = name;
            target->queryStr = foundQuery.str();

            // Create the target components
            std::vector<TargetComponent> path(foundQuery.path.size() + 1);

            int pathIdx = 0;
            path[pathIdx].queryComponent = foundQuery.subset;
            path[pathIdx].nodeId = table.getRoot()->nodeIdx;
            path[pathIdx].parentNodeId = 0;
            path[pathIdx].parentDimensionNodeId = 0;
            path[pathIdx].setType(Typ::Subset);
            pathIdx++;

            auto nodes = tableNode->getPathNodes();
            for (size_t nodeIdx = 1; nodeIdx < nodes.size(); nodeIdx++)
            {
                path[pathIdx].queryComponent = foundQuery.path[nodeIdx - 1];
                path[pathIdx].nodeId = nodes[nodeIdx]->nodeIdx;
                path[pathIdx].parentNodeId = nodes[nodeIdx]->getParent()->nodeIdx;
                path[pathIdx].parentDimensionNodeId =
                    nodes[nodeIdx]->getDimensionParent()->nodeIdx;
                path[pathIdx].setType(nodes[nodeIdx]->type);
                path[pathIdx].fixedRepeatCount = nodes[nodeIdx]->fixedRepCount;
                pathIdx++;
            }

            target->setPath(path);
            target->typeInfo = tableNode->typeInfo;
            target->nodeIdx = tableNode->nodeIdx;

            targets.push_back(target);
        }

        // Cache the targets and masks we just found
        targetCache_.insert({dataProvider_->getSubsetVariant(), targets});
    }
}  // namespace bufr
}  // namespace Ingester
