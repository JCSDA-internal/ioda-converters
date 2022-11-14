/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "SubsetTable.h"

#include <algorithm>
#include <iostream>
#include <memory>
#include <sstream>


namespace Ingester {
namespace bufr {

    SubsetTable::SubsetTable(const DataProvider& dataProvider) :
      dataProvider_(dataProvider)
    {
        initialize();
    }

    std::vector<QueryData> SubsetTable::allQueryData()
    {
        std::vector<QueryData> queryData;
        queryData.reserve(queryMap_.size());

        for (auto queryKey : queryMapKeys_)
        {
            queryData.push_back(queryMap_[queryKey]);
        }

        return queryData;
    }


    QueryData SubsetTable::dataForQuery(const std::vector<std::string>& queryComponents) const
    {
        QueryData queryData;
        auto key = mapKey(queryComponents);
        if (queryMap_.find(key) != queryMap_.end())
        {
            queryData = queryMap_.at(key);
        }
        else
        {
            queryData.isMissing = true;
            queryData.nodeId = 0;
            queryData.pathComponents = {};
            queryData.isString = false;
            queryData.seqPath = {};
            queryData.dimIdxs = {};
        }

        return queryData;
    }


    void SubsetTable::initialize()
    {
        std::vector<size_t> seqPath;
        std::vector<std::vector<std::string>> currentPathElements = {{}};

        std::vector<std::shared_ptr<QueryData>> allQueries;
        std::unordered_map<std::string, std::shared_ptr<QueryData>> foundQueryMap;

        seqPath.push_back(dataProvider_.getInode());
        for (auto nodeIdx = dataProvider_.getInode();
             nodeIdx <= dataProvider_.getIsc(dataProvider_.getInode());
             nodeIdx++)
        {
            if (dataProvider_.getTyp(nodeIdx) == Typ::Repeat ||
                dataProvider_.getTyp(nodeIdx) == Typ::StackedRepeat)
            {
                seqPath.push_back(nodeIdx);
                currentPathElements.push_back({});
            }
            else if (dataProvider_.getTyp(nodeIdx) == Typ::DelayedBinary ||
                     dataProvider_.getTyp(nodeIdx) == Typ::FixedRep)
            {
                seqPath.push_back(nodeIdx + 1);  // push the node idx for the embedded sequence
                currentPathElements.push_back({});
            }
            else if (dataProvider_.getTyp(nodeIdx) == Typ::Number ||
                     dataProvider_.getTyp(nodeIdx) == Typ::Character)
            {
                auto elementTag = dataProvider_.getTag(nodeIdx);
                currentPathElements.back().push_back(elementTag);

                auto numElements = std::count(currentPathElements.back().begin(),
                                                currentPathElements.back().end(),
                                               elementTag);

                auto pathComponents = makePathComponents(seqPath, nodeIdx);

                if (numElements == 2)
                {
                    // Update the matching quiry to require idxs.
                    auto& otherQuery = foundQueryMap[mapKey(pathComponents, 1)];
                    otherQuery->requiresIdx = true;
                }

                auto query = std::make_shared<QueryData>();
                query->nodeId = nodeIdx;
                query->isMissing = false;
                query->seqPath = seqPath;
                query->pathComponents = pathComponents;
                query->isString = (dataProvider_.getItp(nodeIdx) == 3);
                query->dimIdxs = dimPathIdxs(seqPath);
                query->idx = numElements;
                query->requiresIdx = (numElements > 1);
                query->typeInfo = dataProvider_.getTypeInfo(nodeIdx);

                allQueries.push_back(query);
                foundQueryMap[mapKey(query->pathComponents, numElements)] = allQueries.back();
            }

            if (seqPath.size() > 1)
            {
                auto jumpBackNode = dataProvider_.getInode();
                if (nodeIdx < dataProvider_.getIsc(dataProvider_.getInode()))
                {
                    // Skip pure sequences not inside any kind of repeated sequence
                    jumpBackNode = dataProvider_.getJmpb(nodeIdx + 1);
                    if (jumpBackNode == 0) jumpBackNode = dataProvider_.getInode();

                    while (dataProvider_.getTyp(jumpBackNode) == Typ::Sequence &&
                           dataProvider_.getTyp(jumpBackNode - 1) != Typ::DelayedRep &&
                           dataProvider_.getTyp(jumpBackNode - 1) != Typ::FixedRep &&
                           dataProvider_.getTyp(jumpBackNode - 1) != Typ::DelayedRepStacked &&
                           dataProvider_.getTyp(jumpBackNode - 1) != Typ::DelayedBinary)
                    {
                        auto newJumpBackNode = dataProvider_.getJmpb(jumpBackNode);
                        if (newJumpBackNode != jumpBackNode)
                        {
                            jumpBackNode = newJumpBackNode;
                        }
                        else
                        {
                            break;
                        }
                    }
                }

                // Peak ahead to see if the next node is inside one of the containing sequences.
                for (int pathIdx = seqPath.size() - 2; pathIdx >= 0; pathIdx--)
                {
                    // Check if the node idx is the next node for the current path
                    // or if the parent node of the next node is the previous path index

                    if (seqPath[pathIdx] == jumpBackNode)
                    {
                        auto numToRewind = seqPath.size() - pathIdx - 1;
                        for (size_t rewindIdx = 0; rewindIdx < numToRewind; rewindIdx++)
                        {
                            seqPath.pop_back();
                            currentPathElements.pop_back();
                        }
                    }
                }
            }
        }

        for (const auto& query : allQueries)
        {
            auto queryStr = mapKey(query);
            queryMapKeys_.push_back(queryStr);
            queryMap_[queryStr] = *query;
        }
    }


    std::vector<size_t> SubsetTable::dimPathIdxs(std::vector<size_t> seqPath) const
    {
        std::vector<size_t> dimPathIdxs;
        dimPathIdxs.push_back(0);
        for (size_t idx = 1; idx < seqPath.size(); idx++)
        {
            if (dataProvider_.getTyp(seqPath[idx] - 1) == Typ::DelayedRep ||
                dataProvider_.getTyp(seqPath[idx] - 1) == Typ::FixedRep ||
                dataProvider_.getTyp(seqPath[idx] - 1) == Typ::DelayedRepStacked)
            {
                dimPathIdxs.push_back(idx);
            }
        }

        return dimPathIdxs;
    }


    std::vector<std::string> SubsetTable::makePathComponents(std::vector<size_t> seqPath,
                                                             int nodeIdx)
    {
        std::vector<std::string> pathComps;

        pathComps.push_back(dataProvider_.getTag(seqPath[0]));
        for (size_t idx = 1; idx < seqPath.size(); idx++)
        {
            if (dataProvider_.getTyp(seqPath[idx] - 1) == Typ::DelayedRep ||
                dataProvider_.getTyp(seqPath[idx] - 1) == Typ::FixedRep ||
                dataProvider_.getTyp(seqPath[idx] - 1) == Typ::DelayedRepStacked ||
                dataProvider_.getTyp(seqPath[idx] - 1) == Typ::DelayedBinary)
            {
                pathComps.push_back(dataProvider_.getTag(seqPath[idx]));
            }
        }

        pathComps.push_back(dataProvider_.getTag(nodeIdx));
        return pathComps;
    }


    std::string SubsetTable::mapKey(const std::vector<std::string>& pathComponents,
                                    size_t idx) const
    {
        std::ostringstream ostr;
        for (size_t pathIdx = 1; pathIdx < pathComponents.size(); pathIdx++)
        {
            ostr << "/" << pathComponents[pathIdx];
        }

        if (idx > 0)
        {
            ostr << "[" << idx << "]";
        }

        return ostr.str();
    }

    std::string SubsetTable::mapKey(const std::shared_ptr<QueryData> query) const
    {
        std::string key;
        if (query->requiresIdx)
        {
            key = mapKey(query->pathComponents, query->idx);
        }
        else
        {
            key = mapKey(query->pathComponents);
        }

        return key;
    }
}  // namespace bufr
}  // namespace Ingester


