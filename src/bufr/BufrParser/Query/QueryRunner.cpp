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
#include "NodeLookupTable.h"


namespace Ingester {
namespace bufr
{
    struct NodeData
    {
        std::vector<double> values;
        std::vector<int> counts;
    };

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
        collectData(targets, resultSet_);
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
            path[pathIdx].setType(Typ::Subset);
            pathIdx++;

            auto nodes = tableNode->getPathNodes();
            for (size_t nodeIdx = 1; nodeIdx < nodes.size(); nodeIdx++)
            {
                path[pathIdx].queryComponent = foundQuery.path[nodeIdx - 1];
                path[pathIdx].nodeId = nodes[nodeIdx]->nodeIdx;
                path[pathIdx].parentNodeId = nodes[nodeIdx]->getDimensioningParent()->nodeIdx;
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

    void QueryRunner::collectData(Targets& targets, ResultSet &resultSet) const
    {
        auto lookupTable = NodeLookupTable(dataProvider_, targets);
        auto &dataFrame = resultSet.nextDataFrame();

        for (size_t targetIdx = 0; targetIdx < targets.size(); targetIdx++)
        {
            const auto &targ = targets.at(targetIdx);
            auto &dataField = dataFrame.fieldAtIdx(targetIdx);
            dataField.target = targ;

            if (targ->nodeIdx == 0)
            {
                dataField.data = {MissingValue};
                dataField.seqCounts = SeqCounts(std::vector<std::vector<int>>(1, {1}));
            }
            else
            {
                dataField.seqCounts.resize(targ->seqPath.size() + 1);
                dataField.seqCounts[0] = {1};
                SeqCounts counts;

                bool hasFilter = false;
                std::vector<std::vector<size_t>> filters;
                for (size_t pathIdx = 0; pathIdx < targ->seqPath.size(); pathIdx++)
                {
                    auto& pathComponent = targ->path[pathIdx + 1];
                    auto& filter = pathComponent.queryComponent->filter;
                    if (filter.empty())
                    {
                        dataField.seqCounts[pathIdx + 1] = lookupTable[pathComponent.nodeId].counts;
                    }
                    else
                    {
                        // Delay the creation of the counts and filters until we know we need them
                        // in order to avoid unnecessary allocations
                        if (counts.empty())
                            counts = SeqCounts(
                                std::vector<std::vector<int>>(targ->seqPath.size() + 1, {1}));

                        if (filters.empty())
                            filters = std::vector<std::vector<size_t>>(targ->seqPath.size() + 1);

                        filters[pathIdx + 1] = filter;
                        hasFilter = true;

                        auto filteredCounts =
                          std::vector<int>(lookupTable[targ->path[pathIdx].nodeId].counts.size(), 1);

                        for (size_t countIdx = 0; countIdx < filteredCounts.size(); countIdx++)
                        {
                            filteredCounts[countIdx] =
                                std::max(static_cast<int>(filter.size()), filteredCounts[countIdx]);
                        }

                        dataField.seqCounts[pathIdx + 1] = filteredCounts;
                        counts[pathIdx + 1] = lookupTable[targ->path[pathIdx].nodeId].counts;
                    }
                }

                if (!hasFilter)
                {
                    dataField.data = lookupTable[targ->path.back().nodeId].data;
                }
                else
                {
                    dataField.data = makeFilteredData(lookupTable[targ->path.back().nodeId].data,
                                                      counts,
                                                      filters);
                }
            }
        }
    }

    std::vector<double> QueryRunner::makeFilteredData(
                                            const std::vector<double>& srcData,
                                            const SeqCounts& origCounts,
                                            const std::vector<std::vector<size_t>>& filter) const
    {
        auto data = std::vector<double>();
        data.reserve(sum(origCounts.back()));

        size_t offset = 0;
        _makeFilteredData(srcData, origCounts, filter, data, offset, 0);

        return data;
    }

    void QueryRunner::_makeFilteredData(const std::vector<double>& srcData,
                                        const SeqCounts& origCounts,
                                        const std::vector<std::vector<size_t>> &filters,
                                        std::vector<double>& data,
                                        size_t& offset,
                                        size_t depth,
                                        bool skipResult) const
    {
        if (depth > origCounts.size() - 1)
        {
            if (!skipResult) data.push_back(srcData[offset]);
            offset++;

            return;
        }

        auto& layerCounts = origCounts[depth];
        auto& layerFilter = filters[depth];

        if (layerFilter.empty())
        {
            for (size_t countIdx = 0; countIdx < layerCounts.size(); countIdx++)
            {
                _makeFilteredData(srcData, origCounts, filters, data,
                                  offset, depth + 1, skipResult);
            }
        }
        else
        {
            for (size_t countIdx = 0; countIdx < layerCounts.size(); countIdx++)
            {
                for (size_t count = 1; count <= static_cast<size_t>(layerCounts[countIdx]); count++)
                {
                    bool skip = skipResult;

                    if (!skip)
                    {
                        skip = std::find(layerFilter.begin(),
                                         layerFilter.end(),
                                         count) == layerFilter.end();
                    }

                    _makeFilteredData(srcData, origCounts, filters, data, offset, depth + 1, skip);
                }
            }
        }
    }
}  // namespace bufr
}  // namespace Ingester
