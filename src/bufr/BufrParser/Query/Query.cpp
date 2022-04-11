/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include "Query.h"
#include "bufr_interface.h"

#include "eckit/exception/Exceptions.h"

#include <string>
#include <sstream>
#include <iostream>

#include "QueryParser.h"

namespace Ingester {
namespace bufr {

    struct NodeData
    {
        std::vector<double> values;
        std::vector<int> counts;
    };

    //constructor
    Query::Query(const QuerySet& querySet, ResultSet& resultSet, const DataProvider& dataProvider) :
        querySet_(querySet),
        resultSet_(resultSet),
        dataProvider_(dataProvider)
    {
    }

    void Query::query()
    {
        std::shared_ptr<std::vector<__details::Target>> targets;
        std::shared_ptr<__details::ProcessingMasks> masks;

        findTargets(targets, masks);
        return collectData(targets, masks, resultSet_);
    }

    void Query::findTargets(std::shared_ptr<std::vector<__details::Target>>& targets,
                            std::shared_ptr<__details::ProcessingMasks>& masks)
    {
        // Check if the target list for this subset is cached in the targetMap_
        if (targetCache_.find(dataProvider_.getSubset()) != targetCache_.end())
        {
            targets = targetCache_.at(dataProvider_.getSubset());
            masks = maskCache_.at(dataProvider_.getSubset());
            return;
        }

        masks = std::make_shared<__details::ProcessingMasks>();
        targets = std::make_shared<std::vector<__details::Target>>();

        size_t numNodes = dataProvider_.getIsc(dataProvider_.getInode());

        masks->valueNodeMask.resize(numNodes, false);
        masks->pathNodeMask.resize(numNodes, false);

        for (size_t targetIdx = 0; targetIdx < querySet_.size(); ++targetIdx)
        {
            auto queryName = querySet_.nameAt(targetIdx);
            auto queryStr = querySet_.queryAt(targetIdx);

            auto subQueries = QueryParser::splitMultiquery(queryStr);

            bool foundTarget = false;
            __details::Target target;
            for (size_t subQueryIdx = 0; subQueryIdx < subQueries.size(); ++subQueryIdx)
            {
                const std::string& subQuery = subQueries[subQueryIdx];

                target = findTarget(queryName, subQuery);

                if (target.nodeIds.size() > 0)
                {
                    // Collect mask data
                    masks->valueNodeMask[target.nodeIds[0]] = true;
                    for (size_t pathIdx = 0; pathIdx < target.seqPath.size(); ++pathIdx)
                    {
                        masks->pathNodeMask[target.seqPath[pathIdx]] = true;
                    }

                    targets->push_back(target);
                    foundTarget = true;
                    break;
                }
            }

            if (!foundTarget)
            {
                // Add the last missing target to the list
                targets->push_back(target);
                std::cout << "Warning: Query String "
                          << queryStr
                          << " didn't apply to subset "
                          << dataProvider_.getSubset()
                          << std::endl;
            }
        }

        targetCache_.insert({dataProvider_.getSubset(), targets});
        maskCache_.insert({dataProvider_.getSubset(), masks});
    }

    __details::Target Query::findTarget(const std::string& targetName,
                                        const std::string& query) const
    {
        std::string querySubset;
        std::vector<std::string> mnemonics;
        int index;

        QueryParser::splitQueryStr(query, querySubset, mnemonics, index);

        std::vector<int> branches;
        bool isString = false;
        std::vector<int> targetNodes;
        std::vector<size_t> seqPath;
        std::vector<std::string> dimPaths;
        std::vector<int> dimIdxs;

        bool targetMissing = !(querySubset == "*" || querySubset == dataProvider_.getSubset());
        if (!targetMissing)
        {
            branches.resize(mnemonics.size() - 1);

            seqPath.push_back(dataProvider_.getInode());

            int tableCursor = -1;
            int mnemonicCursor = -1;

            for (auto nodeIdx = dataProvider_.getInode();
                 nodeIdx <= dataProvider_.getIsc(dataProvider_.getInode());
                 nodeIdx++)
            {
                if (dataProvider_.getTyp(nodeIdx) == Typ::Sequence ||
                    dataProvider_.getTyp(nodeIdx) == Typ::Repeat ||
                    dataProvider_.getTyp(nodeIdx) == Typ::StackedRepeat)
                {
                    if (isQueryNode(nodeIdx - 1))
                    {
                        if (dataProvider_.getTag(nodeIdx) == mnemonics[mnemonicCursor + 1] &&
                            tableCursor == mnemonicCursor)
                        {
                            mnemonicCursor++;
                            branches[mnemonicCursor] = nodeIdx - 1;
                        }
                        tableCursor++;
                    }
                    seqPath.push_back(nodeIdx);
                }
                else if (mnemonicCursor == static_cast<int>(mnemonics.size()) - 2 &&
                         tableCursor == mnemonicCursor &&
                        dataProvider_.getTag(nodeIdx) == mnemonics.back())
                {
                    // We found a target
                    targetNodes.push_back(nodeIdx);
                    isString = (dataProvider_.getItp(nodeIdx) == 3);

                    getDimInfo(branches, mnemonicCursor, dimPaths, dimIdxs);
                }

                // Step back up the tree (unfortunately this is finicky)
                if (seqPath.size() > 1)
                {
                    // Peak ahead to see if the next node is inside one of the containing sequences
                    // then go back up the approptiate number of sequences. You may have to exit several
                    // sequences in a row if the current sequence is the last element in the containing
                    // sequence.
                    for (int pathIdx = seqPath.size() - 2; pathIdx >= 0; pathIdx--)
                    {
                        if (seqPath[pathIdx] == dataProvider_.getJmpb(nodeIdx + 1))
                        {
                            for (int rewindIdx = seqPath.size() - 1; rewindIdx > pathIdx; rewindIdx--)
                            {
                                // Exit the sequence
                                if (isQueryNode(seqPath[rewindIdx] - 1))
                                {
                                    if (mnemonicCursor > -1 && tableCursor == mnemonicCursor)
                                    {
                                        mnemonicCursor--;
                                    }

                                    tableCursor--;
                                }
                                // Pop out of the current sequence
                                seqPath.pop_back();
                            }
                            break;
                        }
                    }
                }
            }

            if (index > static_cast<int>(targetNodes.size()))
            {
                std::ostringstream errMsg;
                errMsg << "Invalid index in query str " << query << ".";
                throw eckit::BadParameter(errMsg.str());
            }

            if (index > 0 && index <= static_cast<int>(targetNodes.size()))
            {
                targetNodes = { targetNodes[index - 1] };
            }

            if (targetNodes.size() > 1)
            {
                std::ostringstream errMsg;
                errMsg << "Query string must return 1 target. Are you missing an index? " << query << ".";
                throw eckit::BadParameter(errMsg.str());
            }
        }

        auto target = __details::Target();
        target.name = targetName;
        target.queryStr = query;
        target.isString = isString;
        target.seqPath = branches;
        target.nodeIds = targetNodes;

        if (targetNodes.size() > 0)
        {
            target.dimPaths = dimPaths;
            target.exportDimIdxs = dimIdxs;
        }
        else
        {
            target.dimPaths = {"*"};
            target.exportDimIdxs = {0};
        }

        return target;
    }

    bool Query::isQueryNode(int nodeIdx) const
    {
        return (dataProvider_.getTyp(nodeIdx) == Typ::DelayedRep ||
                dataProvider_.getTyp(nodeIdx) == Typ::FixedRep ||
                dataProvider_.getTyp(nodeIdx) == Typ::DelayedRepStacked ||
                dataProvider_.getTyp(nodeIdx) == Typ::DelayedBinary);
    }

    void Query::getDimInfo(const std::vector<int>& branches,
                           int mnemonicCursor,
                           std::vector<std::string>& dimPaths,
                           std::vector<int>& dimIdxs) const
    {
        std::string currentDimPath;
        std::string mnemonicStr;

        // Initialize out parameters
        dimPaths = std::vector<std::string>();
        dimIdxs = std::vector<int>();

        // Allocate enough memory to hold all the dim paths
        dimPaths.reserve(branches.size() + 1);
        dimIdxs.reserve(branches.size() + 1);

        currentDimPath = "*";
        dimPaths.push_back(currentDimPath);
        dimIdxs.push_back(0);

        // Split the branches into node idxs for each additional dimension
        if (mnemonicCursor >= 0)
        {
            int dimIdx = 1;
            for (int branchIdx = 0; branchIdx <= mnemonicCursor; branchIdx++)
            {
                int nodeIdx = branches[branchIdx];
                mnemonicStr = dataProvider_.getTag(nodeIdx);

                std::ostringstream path;
                path << currentDimPath << "/" << mnemonicStr.substr(1, mnemonicStr.size() - 2);
                currentDimPath = path.str();

                if (dataProvider_.getTyp(nodeIdx)  == Typ::DelayedRep ||
                    dataProvider_.getTyp(nodeIdx)  == Typ::FixedRep ||
                    dataProvider_.getTyp(nodeIdx)  == Typ::DelayedRepStacked)
                {
                    dimIdx = dimIdx + 1;
                    dimIdxs.push_back(branchIdx + 1);  // +1 to account for the root dimension
                    dimPaths.push_back(currentDimPath);
                }
            }
        }
    }

    void Query::collectData(std::shared_ptr<std::vector<__details::Target>> targets,
                            std::shared_ptr<__details::ProcessingMasks> masks,
                            ResultSet& resultSet) const
    {
        std::vector<int> currentPath;
        std::vector<int> currentPathReturns;

        auto& dataFrame = resultSet.nextDataFrame();
        int returnNodeIdx = -1;
        int lastNonZeroReturnIdx = -1;

        // Reorganize the data into a NodeValueTable to make lookups faster (avoid looping over all the data a bunch of
        // times)
        auto dataTable = __details::OffsetArray<NodeData>(dataProvider_.getInode(),
                                                          dataProvider_.getIsc(dataProvider_.getInode()));

        for (size_t dataCursor = 1; dataCursor <= dataProvider_.getNVal(); ++dataCursor)
        {
            int nodeIdx = dataProvider_.getInv(dataCursor);

            if (masks->valueNodeMask[nodeIdx])
            {
                auto& values = dataTable[nodeIdx].values;
                values.push_back(dataProvider_.getVal(dataCursor));
            }

            // Unfortuantely the fixed replicated sequences do not store their counts as values for the Fixed Replication
            // nodes. It's therefore necessary to discover this information by manually tracing the nested sequences and
            // counting everything manually. Since we have to do it for fixed reps anyways, its easier just to do it for all
            // the squences.

            if (dataProvider_.getJmpb(nodeIdx) > 0 &&
                masks->pathNodeMask[dataProvider_.getJmpb(nodeIdx)])
            {
                const auto typ = dataProvider_.getTyp(nodeIdx);
                const auto jmpbTyp = dataProvider_.getTyp(dataProvider_.getJmpb(nodeIdx));
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
                    dataCursor == dataProvider_.getNVal() ||
                    (currentPath.size() > 1 && nodeIdx == *(currentPath.end() - 1) + 1))
                {
                    // Look for the first path return idx that is not 0 and check if its this node idx. Exit the sequence if its
                    // appropriate. A return idx of 0 indicates a sequence that occurs as the last element of another sequence.

                    for (int pathIdx = currentPathReturns.size() - 1; pathIdx >= lastNonZeroReturnIdx; --pathIdx)
                    {
                        currentPathReturns.pop_back();
                        auto seqNodeIdx = currentPath.back();
                        currentPath.pop_back();

                        const auto typSeqNode = dataProvider_.getTyp(seqNodeIdx);
                        if (typSeqNode == Typ::DelayedRep || typSeqNode == Typ::DelayedRepStacked)
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
                if (dataProvider_.getTyp(nodeIdx) == Typ::DelayedBinary &&
                    dataProvider_.getVal(dataCursor) == 0)
                {
                    // Ignore the node if it is a delayed binary and the value is 0
                }
                else
                {
                    currentPath.push_back(nodeIdx);
                    const auto tmpReturnNodeIdx = dataProvider_.getLink(nodeIdx);
                    currentPathReturns.push_back(tmpReturnNodeIdx);

                    if (tmpReturnNodeIdx != 0)
                    {
                        lastNonZeroReturnIdx = currentPathReturns.size() - 1;
                        returnNodeIdx = tmpReturnNodeIdx;
                    }
                    else {
                        lastNonZeroReturnIdx = 0;
                        returnNodeIdx = 0;

                        if (dataCursor != dataProvider_.getNVal())
                        {
                            for (int pathIdx = currentPath.size() - 1; pathIdx >= 0; --pathIdx)
                            {
                                returnNodeIdx = dataProvider_.getLink(dataProvider_.getJmpb(currentPath[pathIdx]));
                                lastNonZeroReturnIdx = currentPathReturns.size() - pathIdx;

                                if (returnNodeIdx != 0) break;
                            }
                        }
                    }
                }

                dataTable[nodeIdx + 1].counts.push_back(0);
            }
        }

        for (size_t targetIdx = 0; targetIdx < targets->size(); targetIdx++)
        {
            const auto& targ = targets->at(targetIdx);
            auto& dataField = dataFrame.fieldAtIdx(targetIdx);
            dataField.name = targ.name;
            dataField.queryStr = targ.queryStr;
            dataField.isString = targ.isString;
            if (targ.isString) resultSet.indicateFieldIsString(targetIdx); // Whole column is string.
            dataField.dimPaths = targ.dimPaths;
            dataField.seqPath.resize(targ.seqPath.size() + 1);
            dataField.seqPath[0] = 1;
            std::copy(targ.seqPath.begin(), targ.seqPath.end(), dataField.seqPath.begin() + 1);
            dataField.exportDims = targ.exportDimIdxs;

            if (targ.nodeIds.size() == 0)
            {
                dataField.data = {MissingValue};
                dataField.missing = true;
                dataField.seqCounts = {{1}};
            }
            else
            {
                dataField.seqCounts.resize(targ.seqPath.size() + 1);
                dataField.seqCounts[0] = {1};
                for (size_t pathIdx = 0; pathIdx < targ.seqPath.size(); pathIdx++)
                {
                    dataField.seqCounts[pathIdx + 1] = dataTable[targ.seqPath[pathIdx] + 1].counts;
                }

                if (resultSet.isFieldStr(targetIdx) != targ.isString)
                {
                    std::ostringstream errMsg;
                    errMsg << "Different subsets don't agree whether " << dataField.name
                           << "is a string or not (there is a type mismatch).";
                    throw eckit::BadParameter(errMsg.str());
                }

                dataField.data = dataTable[targ.nodeIds[0]].values;
                if (dataField.data.size() == 0) dataField.missing = true;
            }
        }
   }
}  // namespace bufr
}  // namespace Ingester
