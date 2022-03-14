//
// Created by rmclaren on 1/28/22.
//

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
    Query::Query(const QuerySet& querySet, ResultSet& resultSet) :
        querySet_(querySet),
        resultSet_(resultSet)
    {
    }

    void Query::query(const std::string& subset, int bufrLoc)
    {
        bufrLoc_ = bufrLoc;

        std::shared_ptr<std::vector<__details::Target>> targets;
        std::shared_ptr<__details::ProcessingMasks> masks;

        findTargets(subset, targets, masks);
        return collectData(targets, masks, resultSet_);
    }

    void Query::findTargets(const std::string& subset,
                            std::shared_ptr<std::vector<__details::Target>>& targets,
                            std::shared_ptr<__details::ProcessingMasks>& masks)
    {
        // Check if the target list for this subset is cached in the targetMap_
        if (targetCache_.find(subset) != targetCache_.end())
        {
            targets = targetCache_.at(subset);
            masks = maskCache_.at(subset);
            return;
        }

        masks = std::make_shared<__details::ProcessingMasks>();
        targets = std::make_shared<std::vector<__details::Target>>();

        auto bufrInfo = DataProvider::instance();

        size_t numNodes = bufrInfo->getIsc(bufrInfo->getInode());

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

                target = findTarget(subset, queryName, subQuery);

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
                std::cout << "Warning: Query String " << queryStr << " didn't apply to subset " << subset << std::endl;
            }
        }

        targetCache_.insert({subset, targets});
        maskCache_.insert({subset, masks});
    }

    __details::Target Query::findTarget(const std::string& subset,
                                        const std::string& targetName,
                                        const std::string& query) const
    {
        std::string querySubset;
        std::vector<std::string> mnemonics;
        int index;

        auto bufrInfo = DataProvider::instance();

        QueryParser::splitQueryStr(query, querySubset, mnemonics, index);

        std::vector<int> branches;
        bool isString = false;
        std::vector<int> targetNodes;
        std::vector<int> seqPath;
        std::vector<std::string> dimPaths;
        std::vector<int> dimIdxs;

        bool targetMissing = !(querySubset == "*" || querySubset == subset);
        if (!targetMissing)
        {
            branches.resize(mnemonics.size() - 1);

            seqPath.push_back(bufrInfo->getInode());

            int tableCursor = -1;
            int mnemonicCursor = -1;

            for (auto nodeIdx = bufrInfo->getInode();
                 nodeIdx <= bufrInfo->getIsc(bufrInfo->getInode());
                 nodeIdx++)
            {
                if (bufrInfo->getTyp(nodeIdx) == Typ::Sequence ||
                    bufrInfo->getTyp(nodeIdx) == Typ::Repeat ||
                    bufrInfo->getTyp(nodeIdx) == Typ::StackedRepeat)
                {
                    if (isQueryNode(nodeIdx - 1))
                    {
                        if (bufrInfo->getTag(nodeIdx) == mnemonics[mnemonicCursor + 1] &&
                            tableCursor == mnemonicCursor)
                        {
                            mnemonicCursor++;
                            branches[mnemonicCursor] = nodeIdx - 1;
                        }
                        tableCursor++;
                    }
                    seqPath.push_back(nodeIdx);
                }
                else if (mnemonicCursor == mnemonics.size() - 2 &&
                         tableCursor == mnemonicCursor &&
                        bufrInfo->getTag(nodeIdx) == mnemonics.back())
                {
                    // We found a target
                    targetNodes.push_back(nodeIdx);
                    isString = (bufrInfo->getItp(nodeIdx) == 3);

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
                        if (seqPath[pathIdx] == bufrInfo->getJmpb(nodeIdx + 1))
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

            if (index > 0 && index <= targetNodes.size())
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
        auto bufrInfo = DataProvider::instance();

        return (bufrInfo->getTyp(nodeIdx) == Typ::DelayedRep ||
                bufrInfo->getTyp(nodeIdx) == Typ::FixedRep ||
                bufrInfo->getTyp(nodeIdx) == Typ::DelayedRepStacked ||
                bufrInfo->getTyp(nodeIdx) == Typ::DelayedBinary);
    }

    void Query::getDimInfo(const std::vector<int>& branches,
                           int mnemonicCursor,
                           std::vector<std::string>& dimPaths,
                           std::vector<int>& dimIdxs) const
    {
        auto bufrInfo = DataProvider::instance();

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
                mnemonicStr = bufrInfo->getTag(nodeIdx);

                std::ostringstream path;
                path << currentDimPath << "/" << mnemonicStr.substr(1, mnemonicStr.size() - 2);
                currentDimPath = path.str();

                if (bufrInfo->getTyp(nodeIdx)  == Typ::DelayedRep ||
                    bufrInfo->getTyp(nodeIdx)  == Typ::FixedRep ||
                    bufrInfo->getTyp(nodeIdx)  == Typ::DelayedRepStacked)
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

        auto bufrInfo = DataProvider::instance();

        std::vector<int> currentPath;
        std::vector<int> currentPathReturns;

        auto& dataFrame = resultSet.nextDataFrame();
        int returnNodeIdx = -1;
        int lastNonZeroReturnIdx = -1;

        // Reorganize the data into a NodeValueTable to make lookups faster (avoid looping over all the data a bunch of
        // times)
        auto dataTable = __details::OffsetArray<NodeData>(bufrInfo->getInode(),
                                                          bufrInfo->getIsc(bufrInfo->getInode()));

        for (size_t dataCursor = 1; dataCursor <= bufrInfo->getNVal(); ++dataCursor)
        {
            int nodeIdx = bufrInfo->getInv(dataCursor);

            if (masks->valueNodeMask[nodeIdx])
            {
                auto& values = dataTable[nodeIdx].values;
                values.push_back(bufrInfo->getVal(dataCursor));
            }

            // Unfortuantely the fixed replicated sequences do not store their counts as values for the Fixed Replication
            // nodes. It's therefore necessary to discover this information by manually tracing the nested sequences and
            // counting everything manually. Since we have to do it for fixed reps anyways, its easier just to do it for all
            // the squences.

            if (bufrInfo->getJmpb(nodeIdx) > 0 &&
                masks->pathNodeMask[bufrInfo->getJmpb(nodeIdx)])
            {
                const auto typ = bufrInfo->getTyp(nodeIdx);
                const auto jmpbTyp = bufrInfo->getTyp(bufrInfo->getJmpb(nodeIdx));
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
                    dataCursor == bufrInfo->getNVal() ||
                    (currentPath.size() > 1 && nodeIdx == *(currentPath.end() - 1) + 1))
                {
                    // Look for the first path return idx that is not 0 and check if its this node idx. Exit the sequence if its
                    // appropriate. A return idx of 0 indicates a sequence that occurs as the last element of another sequence.

                    for (int pathIdx = currentPathReturns.size() - 1; pathIdx >= lastNonZeroReturnIdx; --pathIdx)
                    {
                        currentPathReturns.pop_back();
                        auto seqNodeIdx = currentPath.back();
                        currentPath.pop_back();

                        const auto typSeqNode = bufrInfo->getTyp(seqNodeIdx);
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
                if (bufrInfo->getTyp(nodeIdx) == Typ::DelayedBinary &&
                    bufrInfo->getVal(dataCursor) == 0)
                {
                    // Ignore the node if it is a delayed binary and the value is 0
                }
                else
                {
                    currentPath.push_back(nodeIdx);
                    const auto tmpReturnNodeIdx = bufrInfo->getLink(nodeIdx);
                    currentPathReturns.push_back(tmpReturnNodeIdx);

                    if (tmpReturnNodeIdx != 0)
                    {
                        lastNonZeroReturnIdx = currentPathReturns.size() - 1;
                        returnNodeIdx = tmpReturnNodeIdx;
                    }
                    else {
                        lastNonZeroReturnIdx = 0;
                        returnNodeIdx = 0;

                        if (dataCursor != bufrInfo->getNVal())
                        {
                            for (int pathIdx = currentPath.size() - 1; pathIdx >= 0; --pathIdx)
                            {
                                returnNodeIdx = bufrInfo->getLink(bufrInfo->getJmpb(currentPath[pathIdx]));
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

                dataField.data = dataTable[targ.nodeIds[0]].values;
                if (dataField.data.size() == 0) dataField.missing = true;
            }
        }
   }
}  // namespace bufr
}  // namespace Ingester
