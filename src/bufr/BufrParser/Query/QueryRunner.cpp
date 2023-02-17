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
        std::shared_ptr<__details::ProcessingMasks> masks;

        findTargets(targets, masks);
        collectData(targets, masks, resultSet_);
    }

    void QueryRunner::findTargets(Targets &targets,
                                  std::shared_ptr<__details::ProcessingMasks> &masks)
    {
        // Check if the target list for this subset is cached
        if (targetCache_.find(dataProvider_->getSubsetVariant()) != targetCache_.end())
        {
            targets = targetCache_.at(dataProvider_->getSubsetVariant());
            masks = maskCache_.at(dataProvider_->getSubsetVariant());
            return;
        }

        masks = std::make_shared<__details::ProcessingMasks>();
        {  // Initialize Masks
            size_t numNodes = dataProvider_->getIsc(dataProvider_->getInode());
            masks->valueNodeMask.resize(numNodes, false);
            masks->pathNodeMask.resize(numNodes, false);
        }

        auto table = SubsetTable(dataProvider_);

        for (const auto &name: querySet_.names())
        {
            // Find the table node for the query. Loop through all the sub-queries until you find one.
            Query foundQuery;
            std::shared_ptr<BufrNode> tableNode;
            for (const auto &query: querySet_.queriesFor(name))
            {
                tableNode = table.getNodeForPath(query.path);
                foundQuery = query;
                if (tableNode != nullptr) break;
            }

            auto target = std::make_shared<Target>();

            // There was no corresponding table node for any of the sub-queries so create empty target.
            if (tableNode == nullptr)
            {
                // Create empty target
                target->name = name;
                target->nodeIdx = 0;
                target->queryStr = querySet_.queriesFor(name)[0].queryStr;
                target->dimPaths = {"*"};
                target->exportDimIdxs = {0};
                target->typeInfo = TypeInfo();
                targets.push_back(target);

                // Print message to inform the user of the missing target
                oops::Log::warning() << "Warning: Query String ";
                oops::Log::warning() << querySet_.queriesFor(name)[0].queryStr;
                oops::Log::warning() << " didn't apply to subset ";
                oops::Log::warning() << dataProvider_->getSubsetVariant().str();
                oops::Log::warning() << std::endl;

                continue;
            }

            // Create the target
            target->name = name;
            target->queryStr = foundQuery.queryStr;

            // Create the target components
            std::vector<TargetComponent> path(foundQuery.path.size() + 1);

            int pathIdx = 0;
            path[pathIdx].queryComponent = foundQuery.subset;
            path[pathIdx].branch = 0;
            path[pathIdx].setType(Typ::Subset);
            pathIdx++;

            auto nodes = tableNode->getPathNodes();
            for (size_t nodeIdx = 1; nodeIdx < nodes.size(); nodeIdx++)
            {
                path[pathIdx].queryComponent = foundQuery.path[nodeIdx - 1];
                path[pathIdx].branch = nodes[nodeIdx]->nodeIdx;
                path[pathIdx].setType(nodes[nodeIdx]->type);
                pathIdx++;
            }

            target->setPath(path);
            target->typeInfo = tableNode->typeInfo;
            target->nodeIdx = tableNode->nodeIdx;
            target->dimPaths = tableNode->getDimPaths();
            target->exportDimIdxs = tableNode->getDimIdxs();

            targets.push_back(target);

            // Set the mask
            masks->valueNodeMask[target->nodeIdx] = true;
            for (size_t pathIdx = 0; pathIdx < target->seqPath.size(); ++pathIdx)
            {
                masks->pathNodeMask[target->seqPath[pathIdx]] = true;
            }
        }

        // Cache the targets and masks we just found
        targetCache_.insert({dataProvider_->getSubsetVariant(), targets});
        maskCache_.insert({dataProvider_->getSubsetVariant(), masks});
    }

    bool QueryRunner::isQueryNode(int nodeIdx) const
    {
        return (dataProvider_->getTyp(nodeIdx) == Typ::DelayedRep ||
                dataProvider_->getTyp(nodeIdx) == Typ::FixedRep ||
                dataProvider_->getTyp(nodeIdx) == Typ::DelayedRepStacked ||
                dataProvider_->getTyp(nodeIdx) == Typ::DelayedBinary);
    }

    void QueryRunner::collectData(Targets &targets,
                                  std::shared_ptr<__details::ProcessingMasks> masks,
                                  ResultSet &resultSet) const
    {
        std::vector<int> currentPath;
        std::vector<int> currentPathReturns;

        currentPath.reserve(10);
        currentPathReturns.reserve(10);

        auto &dataFrame = resultSet.nextDataFrame();
        int returnNodeIdx = -1;
        int lastNonZeroReturnIdx = -1;

        // Reorganize the data into a NodeValueTable to make lookups faster (avoid looping over all
        // the data a bunch of times)
        auto dataTable = __details::OffsetArray<NodeData>(
            dataProvider_->getInode(),
            dataProvider_->getIsc(dataProvider_->getInode()));

        for (size_t dataCursor = 1; dataCursor <= dataProvider_->getNVal(); ++dataCursor)
        {
            int nodeIdx = dataProvider_->getInv(dataCursor);

            if (masks->valueNodeMask[nodeIdx])
            {
                dataTable[nodeIdx].values.push_back(dataProvider_->getVal(dataCursor));
            }

            // Unfortuantely the fixed replicated sequences do not store their counts as values for
            // the Fixed Replication nodes. It's therefore necessary to discover this information by
            // manually tracing the nested sequences and counting everything manually. Since we have
            // to do it for fixed reps anyways, its easier just to do it for all the squences.
            if (dataProvider_->getJmpb(nodeIdx) > 0 &&
                masks->pathNodeMask[dataProvider_->getJmpb(nodeIdx)])
            {
                const auto typ = dataProvider_->getTyp(nodeIdx);
                const auto jmpbTyp = dataProvider_->getTyp(dataProvider_->getJmpb(nodeIdx));
                if ((typ == Typ::Sequence && (jmpbTyp == Typ::Sequence ||
                                              jmpbTyp == Typ::DelayedBinary ||
                                              jmpbTyp == Typ::FixedRep)) ||
                    typ == Typ::Repeat ||
                    typ == Typ::StackedRepeat)
                {
                    dataTable[nodeIdx].counts.back()++;
                }
            }

            if (currentPath.size() >= 1)
            {
                if (nodeIdx == returnNodeIdx ||
                    dataCursor == dataProvider_->getNVal() ||
                    (currentPath.size() > 1 && nodeIdx == *(currentPath.end() - 1) + 1))
                {
                    // Look for the first path return idx that is not 0 and check if its this node
                    // idx. Exit the sequence if its appropriate. A return idx of 0 indicates a
                    // sequence that occurs as the last element of another sequence.
                    for (int pathIdx = currentPathReturns.size() - 1;
                         pathIdx >= lastNonZeroReturnIdx;
                         --pathIdx)
                    {
                        currentPathReturns.pop_back();
                        auto seqNodeIdx = currentPath.back();
                        currentPath.pop_back();

                        const auto typSeqNode = dataProvider_->getTyp(seqNodeIdx);
                        if (typSeqNode == Typ::DelayedRep ||
                            typSeqNode == Typ::DelayedRepStacked)
                        {
                            dataTable[seqNodeIdx + 1].counts.back()--;
                        }
                    }

                    lastNonZeroReturnIdx = currentPathReturns.size() - 1;
                    returnNodeIdx = currentPathReturns[lastNonZeroReturnIdx];
                }
            }

            if (masks->pathNodeMask[nodeIdx] && isQueryNode(nodeIdx))
            {
                if (dataProvider_->getTyp(nodeIdx) == Typ::DelayedBinary &&
                    dataProvider_->getVal(dataCursor) == 0)
                {
                    // Ignore the node if it is a delayed binary and the value is 0
                }
                else
                {
                    currentPath.push_back(nodeIdx);
                    const auto tmpReturnNodeIdx = dataProvider_->getLink(nodeIdx);
                    currentPathReturns.push_back(tmpReturnNodeIdx);

                    if (tmpReturnNodeIdx != 0)
                    {
                        lastNonZeroReturnIdx = currentPathReturns.size() - 1;
                        returnNodeIdx = tmpReturnNodeIdx;
                    }
                    else
                    {
                        lastNonZeroReturnIdx = 0;
                        returnNodeIdx = 0;

                        if (dataCursor != dataProvider_->getNVal())
                        {
                            for (int pathIdx = currentPath.size() - 1; pathIdx >= 0; --pathIdx)
                            {
                                returnNodeIdx = dataProvider_->getLink(
                                    dataProvider_->getJmpb(currentPath[pathIdx]));
                                lastNonZeroReturnIdx = currentPathReturns.size() - pathIdx;

                                if (returnNodeIdx != 0) break;
                            }
                        }
                    }
                }

                dataTable[nodeIdx + 1].counts.push_back(0);
            }
        }

        for (size_t targetIdx = 0; targetIdx < targets.size(); targetIdx++)
        {
            const auto &targ = targets.at(targetIdx);
            auto &dataField = dataFrame.fieldAtIdx(targetIdx);
            dataField.target = targ;

            if (targ->nodeIdx == 0)
            {
                dataField.data = {MissingValue};
                dataField.seqCounts = {{1}};
            }
            else
            {
                dataField.seqCounts.resize(targ->seqPath.size() + 1);
                dataField.seqCounts[0] = {1};
                for (size_t pathIdx = 0; pathIdx < targ->seqPath.size(); pathIdx++)
                {
                    dataField.seqCounts[pathIdx + 1] = dataTable[targ->seqPath[pathIdx] +
                                                                 1].counts;
                }

                dataField.data = dataTable[targ->nodeIdx].values;
            }
        }
    }
}  // namespace bufr
}  // namespace Ingester
