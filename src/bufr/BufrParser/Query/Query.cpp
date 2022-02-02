//
// Created by rmclaren on 1/28/22.
//

#include "Query.h"
#include "bufr_interface.h"

#include <string>
#include <sstream>
#include <iostream>

#include "QueryParser.h"


namespace
{
    const char* Subset = "SUB";
    const char* DelayedRep = "DRP";
    const char* FixedRep = "REP";
    const char* DelayedRepStacked = "DRS";
    const char* DelayedBinary = "DRB";
    const char* Sequence = "SEQ";
    const char* Repeat = "RPC";
    const char* StackedRepeat = "RPS";
}

namespace Ingester {
namespace bufr {
    //constructor
    Query::Query(const QuerySet& querySet) :
        querySet_(querySet)
    {
    }

    void Query::query()
    {
        std::string subset;
        std::vector<std::string> mnemonics;
        int index = 0;

        mnemonics = QueryParser::splitMultiquery("[*/ABC[1], */XYZ[3]]");

//        QueryParser::splitQueryStr("*/ABC/XYZ", subset, mnemonics, index);

        std::cout << "subset: " << subset << std::endl;

        for (auto mnemonic : mnemonics)
        {
            std::cout << "mnemonic: " << mnemonic << std::endl;
        }

        std::cout << "index: " << index << std::endl;

//        return collectData(findTargets(subset));
    }
//
//    void Query::findTargets(const std::string& subset,
//                            std::vector<Target>& targets,
//                            std::vector<ProcessingMasks>& masks) const
//    {
//        // Check if the target list for this subset is cached in the targetMap_
//        if (targetCache_.find(subset) != targetCache_.end())
//        {
//            targets = targetCache_.at(subset);
//            masks = maskCache_.at(subset);
//            return;
//        }
//
//        size_t numNodes = bufrTable_.isc[bufrTable_.inode[bufrLoc_]];
//
//        ProcessingMasks newMasks;
//        newMasks.value_node_mask.resize(numNodes, false);
//        newMasks.path_node_mask.resize(numNodes, false);
//
//        for (int targetIdx = 0, targetIdx < querySet_.size(); ++targetIdx)
//        {
//            const std::string& queryName = querySet_.getQueryName(targetIdx);
//            const std::string& queryStr = querySet_.getQueryStr(targetIdx);
//
//            std::vector<std::string> subQueries = splitIntoSubQueries(queryStr);
//
//            for (size_t subQueryIdx = 0, subQueryIdx < subQueries.size(); ++subQueryIdx)
//            {
//                const std::string& subQuery = subQueries[subQueryIdx];
//
//                Target target = findTarget(bufrLoc_, subset, queryName, subQuery);
//
//                if (target.nodeIds.size() > 0)
//                {
//                    // Collect mask data
//                    newMasks.value_node_mask[target.nodeIds[0]] = true;
//                    for (size_t pathIdx = 0, pathIdx < target.seqPath.size(); ++pathIdx)
//                    {
//                        newMasks.path_node_mask[target.seqPath[pathIdx]] = true;
//                    }
//
//                    foundTarget = true;
//                }
//            }
//
//            if (!foundTarget)
//            {
//                // Add the last missing target to the list
//                targets.push_back(target);
//                masks.push_back(newMasks);
//
//                cerr << "Warning: Query String " << queryStr << " didn't apply to subset " << subset << endl;
//            }
//        }
//
//        targetCache_.insert({subset, targets});
//        maskCache_.insert({subset, masks});
//    }
//
//    Target Query::findTarget(const std::string& subset, const std::string& targetName, const std::string query) const
//    {
//        std::string querySubset;
//        std::vector<std::string> mnemonics;
//        int index;
//
//        QueryParser::splitQueryStr(query, querySubset, mnemonics, index);
//
//        std::vector<int> branches;
//        bool isString = false;
//        std::vector<int> targetNodes;
//        std::vector<int> seqPath;
//        std::vector<std::string> dimPaths;
//        std::vector<int> dimIdxs;
//
//        bufrTable_.update();
//
//        bool targetMissing = (subset != "*" && querySubset != subset);
//        if (!targetMissing)
//        {
//            branches.resize(mnemonics.size() - 1);
//
//            seqPath.push_back(bufrTable_.inode[bufrLoc_]);
//
//            int tableCursor = 0;
//            int mnemonicCursor = 0;
//
//            for (auto nodeIdx = bufrTable_.inode[bufrLoc_];
//                 nodeIdx != bufrTable_.isc[bufrTable_.inode[bufrLoc_]];
//                 nodeIdx++)
//            {
//                if (bufrTable_.typ[nodeIdx] == Sequence ||
//                    bufrTable_.typ[nodeIdx] == Repeat ||
//                    bufrTable_.typ[nodeIdx] == StackedRepeat)
//                {
//                    if (isQueryNode(nodeIdx - 1))
//                    {
//                        if (bufrTable_.tag[nodeIdx] == mnemonics[mnemonicCursor + 1] &&
//                            tableCursor == mnemonicCursor)
//                        {
//                            mnemonicCursor++;
//                            branches[mnemonicCursor] = nodeIdx - 1;
//                        }
//                        tableCursor++;
//                    }
//                    seqPath.push_back(nodeIdx);
//                }
//                else if (mnemonicCursor == mnemonics.size() - 1 &&
//                         tableCursor == mnemonicCursor &&
//                         bufrTable_.tag[nodeIdx] == mnemonics[mnemonics.size()])
//                {
//                    // We found a target
//                    targetNodes.push_back(nodeIdx);
//                    isString = (bufrTable_.itp[nodeIdx] == 3);
//
//                    getDimInfo(branches, mnemonicCursor, dimPaths, dimIdxs);
//                }
//
//                if (seqPath.size() > 1)
//                {
//                    // Peak ahead to see if the next node is inside one of the containing sequences
//                    // then go back up the approptiate number of sequences. You may have to exit several
//                    // sequences in a row if the current sequence is the last element in the containing
//                    // sequence.
//                    for (int pathIdx = seqPath.size() - 1; pathIdx > 0; pathIdx--)
//                    {
//                        if (seqPath[pathIdx] == bufrTable_.jmpb[nodeIdx + 1])
//                        {
//                            for (int rewindIdx = seqPath.size() - 1; rewindIdx >= pathIdx; rewindIdx--)
//                            {
//                                // Exit the sequence
//                                if (isQueryNode(seqPath[rewindIdx] - 1))
//                                {
//                                    if (mnemonicCursor > 0 && tableCursor == mnemonicCursor)
//                                    {
//                                        if (bufrTable_.link[branches[mnemonicCursor]] == nodeIdx)
//                                        {
//                                            mnemonicCursor--;
//                                        }
//                                    }
//                                    tableCursor--;
//                                }
//                                // Pop out of the current sequence
//                                seqPath.pop_back();
//                            }
//                            break;
//                        }
//                    }
//                }
//            }
//        }
//
//        auto target = Target();
//        target.name = targetName;
//        target.is_string = isString;
//        target.seq_path = seqPath;
//        target.node_ids = targetNodes;
//        target.dim_paths = dimPaths;
//        target.export_dim_idxs = dimIdxs;
//
//        return target;
//    }
//
//    bool Query::isQueryNode(int nodeIdx) const
//    {
//        return (bufrTable_.typ[nodeIdx] == DelayedRep ||
//                bufrTable_.typ[nodeIdx] == FixedRep ||
//                bufrTable_.typ[nodeIdx] == DelayedRepStacked ||
//                bufrTable_.typ[nodeIdx] == DelayedBinary);
//    }
//
//    void Query::getDimInfo(const std::vector<int>& branches,
//                           int mnemonicCursor,
//                           std::vector<std::string>& dimPaths,
//                           std::vector<int>& dimIdxs) const
//    {
//        std::string currentDimPath;
//        std::string mnemonicStr;
//
//        // Allocate enough memory to hold all the dim paths
//        dimPaths.reserve(branches.size() + 1);
//
//        int dimIdx = 1;
//        currentDimPath = "*";
//        dimPaths[dimIdx].assign(currentDimPath.begin(), currentDimPath.end());
//        dimIdxs.push_back(1);
//
//        // Split the branches into node idxs for each additional dimension
//        if (mnemonicCursor > 0) {
//            for (int branchIdx = 0; branchIdx < mnemonicCursor; branchIdx++) {
//                int nodeIdx = branches[branchIdx];
//                mnemonicStr = bufrTable_.tag[nodeIdx];
//
//                std::ostringstream path;
//                path << currentDimPath << "/" << mnemonicStr.substr(2, mnemonicStr.size() - 3);
//                currentDimPath = path.str();
//
//                if (bufrTable_.typ[nodeIdx] == DelayedRep ||
//                    bufrTable_.typ[nodeIdx] == FixedRep ||
//                    bufrTable_.typ[nodeIdx] == DelayedRepStacked)
//                {
//                    dimIdx = dimIdx + 1;
//                    dimIdxs.push_back(branchIdx + 1);
//                    dimPaths.push_back(currentDimPath);
//                }
//            }
//        }
//    }
//
//// Convert Fortran to C++
////    subroutine collect_data(lun, targets, masks, result_set)
////        integer, intent(in) :: lun
////                type(Target), target, intent(in) :: targets(:)
////        type(ProcessingMasks), intent(in) :: masks
////                type(ResultSet), intent(inout) :: result_set
////
////                integer :: path_idx, target_idx
////        integer :: node_idx, seq_node_idx, tmp_return_node_idx
////                integer :: data_cursor
////        integer :: return_node_idx
////                type(DataField), pointer :: data_field
////        type(DataFrame), pointer :: data_frame
////        type(Target), pointer :: targ
////        type(NodeValueTable) :: node_value_table
////                type(RealList), pointer :: real_list
////        type(IntList), pointer :: count_list, rep_list
////        integer :: last_non_zero_return_idx
////                type(IntList) :: current_path, current_path_returns
////
////
////                current_path = IntList()
////        current_path_returns = IntList()
////
////        data_frame => result_set%next_data_frame()
////        return_node_idx = -1
////
////        ! Reorganize the data into a NodeValueTable to make lookups faster (avoid looping over all the data a bunch of
////        ! times)
////        node_value_table = NodeValueTable(inode(lun), isc(inode(lun)))
////        do data_cursor = 1, nval(lun)
////        node_idx = inv(data_cursor, lun)
////        real_list => node_value_table%values_for_node(node_idx)
////
////        if (masks%value_node_mask(node_idx)) then
////                call real_list%push(val(data_cursor, lun))
////        end if
////
////        count_list => node_value_table%counts_for_node(node_idx)
////
////        ! Unfortuantely the fixed replicated sequences do not store their counts as values for the Fixed Replication
////        ! nodes. It's therefore necessary to discover this information by manually tracing the nested sequences and
////        ! counting everything manually. Since we have to do it for fixed reps anyways, its easier just to do it for all
////        ! the squences.
////
////        if (jmpb(node_idx) > 0) then
////        if (masks%path_node_mask(jmpb(node_idx))) then
////        if ((typ(node_idx) == Sequence .and. &
////        (typ(jmpb(node_idx)) == DelayedBinary .or. typ(jmpb(node_idx)) == FixedRep)) .or. &
////        typ(node_idx) == Repeat .or. &
////        typ(node_idx) == StackedRepeat) then
////
////        ! Add to the count of the currently active sequence
////                count_list%at(count_list%length()) = count_list%at(count_list%length()) + 1
////        end if
////        end if
////        end if
////
////        if (current_path%length() >= 1) then
////        if (node_idx == return_node_idx .or. &
////        data_cursor == nval(lun) .or. &
////        (current_path%length() > 1 .and. node_idx == current_path%at(current_path%length() - 1) + 1)) then
////        ! Look for the first path return idx that is not 0 and check if its this node idx. Exit the sequence if its
////        ! appropriate. A return idx of 0 indicates a sequence that occurs as the last element of another sequence.
////
////        do path_idx = current_path_returns%length(), last_non_zero_return_idx, -1
////        call current_path_returns%pop()
////        call current_path%pop(seq_node_idx)
////
////        ! Delayed and Stacked Reps are inconsistent with other sequence types and add an extra replication
////        ! per sequence. We need to account for this here.
////        if (typ(seq_node_idx) == DelayedRep .or. &
////        typ(seq_node_idx) == DelayedRepStacked) then
////                rep_list => node_value_table%counts_for_node(seq_node_idx + 1)
////        rep_list%at(rep_list%length()) = rep_list%at(rep_list%length()) - 1
////        end if
////        end do
////
////        last_non_zero_return_idx = current_path_returns%length()
////        return_node_idx = current_path_returns%at(last_non_zero_return_idx)
////        end if
////        end if
////
////        if (masks%path_node_mask(node_idx) .and. is_query_node(node_idx)) then
////        if (typ(node_idx) == DelayedBinary .and. val(data_cursor, lun) == 0) then
////        ! Ignore the node if it is a delayed binary and the value is 0
////        else
////        call current_path%push(node_idx)
////        tmp_return_node_idx = link(node_idx)
////
////        call current_path_returns%push(tmp_return_node_idx)
////
////        if (tmp_return_node_idx /= 0) then
////                last_non_zero_return_idx = current_path_returns%length()
////        return_node_idx = tmp_return_node_idx
////        else
////        last_non_zero_return_idx = 1
////        return_node_idx = 0
////
////        if (data_cursor /= nval(lun)) then
////        do path_idx = current_path%length(), 1, -1
////        return_node_idx = link(jmpb(current_path%at(path_idx)))
////        last_non_zero_return_idx = current_path_returns%length() - path_idx + 1
////
////        if (return_node_idx /= 0) exit
////                end do
////        end if
////        end if
////        end if
////
////        count_list => node_value_table%counts_for_node(node_idx + 1)
////        call count_list%push(0)
////        end if
////        end do
////
////        ! Uncomment to print the node value table
////        !    do node_idx = inode(lun), isc(inode(lun))
////        !      real_list = node_value_table%values_for_node(node_idx)
////        !      print *, tag(node_idx), real_list%array()
////        !    end do
////
////
////        ! Uncomment to print he sequnce counts table
////        !    do node_idx = inode(lun), isc(inode(lun))
////        !      if (jmpb(node_idx) > 0) then
////        !        if (masks%path_node_mask(jmpb(node_idx))) then
////        !          count_list => node_value_table%counts_for_node(node_idx)
////        !          print *, " ",  tag(node_idx), "  size: ", count_list%length(), "  counts: ",  count_list%array()
////        !        end if
////        !      end if
////        !    end do
////
////        do target_idx = 1, size(targets)
////        targ => targets(target_idx)
////
////        data_field => data_frame%data_fields(target_idx)
////        data_field%name = String(targ%name)
////        data_field%query_str = String(targ%query_str)
////        data_field%is_string = targ%is_string
////        allocate(data_field%dim_paths, source=targ%dim_paths)
////        allocate(data_field%seq_path(size(targ%seq_path) + 1))
////        data_field%seq_path(1) = 1
////        data_field%seq_path(2:size(targ%seq_path)+1) = targ%seq_path
////                allocate(data_field%export_dim_idxs, source=targ%export_dim_idxs)
////
////        if (size(targ%node_ids) == 0) then
////        ! Ignore targets where the required nodes could not be found in this subset
////                allocate(data_field%data(1))
////        data_field%data(1) = MissingValue
////                data_field%missing = .true.
////        allocate(data_field%seq_counts(1))
////        data_field%seq_counts(1)%counts = [1]
////        else
////        allocate(data_field%seq_counts(size(targ%seq_path) + 1))
////        data_field%seq_counts(1)%counts = [1]
////        do path_idx = 1, size(targ%seq_path)
////        count_list => node_value_table%counts_for_node(targ%seq_path(path_idx) + 1)
////        allocate(data_field%seq_counts(path_idx + 1)%counts, source=count_list%array())
////        end do
////
////        real_list => node_value_table%values_for_node(targ%node_ids(1))
////        allocate(data_field%data, source=real_list%array())
////
////        if (size(data_field%data) == 0) data_field%missing = .true.
////        end if
////        end do
////
////        call node_value_table%delete()
////    end subroutine
//
//    void Query::collectData(const std::vector<Target>& targets,
//                            const std::vector<ProcessingMasks>& masks,
//                            ResultSet& resultSet) const
//    {
//        std::vector<int> currentPath;
//        std::vector<int> currentPathReturns;
//
//        auto dataFrame = resultSet.nextDataFrame();
//        int returnNodeIdx = -1;
//
//        // Reorganize the data into a NodeValueTable to make lookups faster (avoid looping over all the data a bunch of
//        // times)
//
//        struct NodeData
//        {
//            std::vector<double> values;
//            std::vector<int> counts;
//        };
//
//        std::vector<int, NodeData> nodeDataTable;
//        nodeDataTable.reserve( bufrTable_.isc[bufrTable_.inode[bufrLoc_]] - bufrTable_.inode[bufrLoc_]);
//
//        for (size_t dataCursor = 0; dataCursor < bufrData_.nval; ++dataCursor)
//        {
//            int nodeIdx = bufrData_.inv[dataCursor][bufrLoc_];
//            int nodeIdxInTable = bufrTable_.inode[bufrLoc_] + nodeIdx - 1;
//
//            if (nodeIdxInTable >= nodeDataTable.size())
//            {
//                nodeDataTable.resize(nodeIdxInTable + 1);
//            }
//
//            auto& nodeData = nodeDataTable[nodeIdxInTable];
//
//            if (nodeData.values.empty())
//            {
//                nodeData.values.resize(bufrTable_.nval[nodeIdxInTable]);
//                nodeData.counts.resize(bufrTable_.nval[nodeIdxInTable]);
//            }
//
//            nodeData.values[bufrData_.val[dataCursor]] += 1;
//            nodeData.counts[bufrData_.val[dataCursor]] += 1;
//        }
//
//   }

}  // namespace bufr
}  // namespace Ingester
