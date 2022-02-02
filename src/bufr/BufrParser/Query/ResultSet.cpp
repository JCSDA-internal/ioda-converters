//
// Created by rmclaren on 6/30/21.
//


#include "ResultSet.h"
#include "query_interface.h"

#include <string>
#include <algorithm> 
#include <iostream>
#include <sstream>

#include "VectorMath.h"


namespace Ingester {
namespace bufr {
    ResultSet::ResultSet()
    {
    }

    ResultSet::~ResultSet()
    {
    }

    std::shared_ptr<ResultBase> ResultSet::get(const std::string& fieldName,
                                               const std::string& groupByFieldName) const
    {
//        std::vector<double> data;
//        std::vector<size_t> dims;
//        std::vector<std::string> dimPaths;
//
//        getRawValues(fieldName,
//                     groupByFieldName,
//                     data,
//                     dims,
//                     dimPaths);
//
//
//        std::shared_ptr<ResultBase> result;
//        if (isString(fieldName))
//        {
//            auto data = std::vector<std::string>();
//
//            const char* char_ptr = (char*) data_ptr;
//            for (int row_idx = 0; row_idx < dims_ptr[0]; row_idx++)
//            {
//                std::string str = std::string(char_ptr + row_idx * sizeof(double), sizeof(double));
//
//                // trim trailing whitespace from str
//                str.erase(std::find_if(str.rbegin(), str.rend(),
//                                       [](char c) { return !std::isspace(c); }).base(), str.end());
//
//                data.push_back(str);
//            }
//
//            auto strResult = std::make_shared<Result<std::string>>();
//            strResult->field_name = field_name;
//            strResult->group_by_field_name = group_by_field;
//            strResult->data = data;
//            strResult->dims.push_back(dims_ptr[0]);
//            result = strResult;
//        }
//        else
//        {
//            // Compute product of dimensions
//            int tot_elements = 1;
//            for (int row = 0; row < num_dims; row++)
//            {
//                tot_elements *= dims_ptr[row];
//            }
//
//            auto floatResult = std::make_shared<Result<float>>();
//            floatResult->field_name = field_name;
//            floatResult->group_by_field_name = group_by_field;
//            floatResult->data = std::vector<float>(data_ptr, data_ptr + tot_elements);
//            floatResult->dims = std::vector<std::size_t>(dims_ptr, dims_ptr + num_dims);
//            result = floatResult;
//        }
//
//        // Add dim path strings
//        const char* ws = " \t\n\r\f\v";
//        for (int dim_idx = 0; dim_idx < num_dims; dim_idx++)
//        {
//            auto path_str = std::string(dims_paths_ptr + dim_idx * dim_paths_str_len, dim_paths_str_len);
//
//            // Trim extra chars from the path str
//            path_str.erase(path_str.find_last_not_of(ws) + 1);
//            result->dimPaths.push_back(path_str);
//        }
//
//        free_result_get_data_f();

//        return result;

        return nullptr;
    }


    DataFrame& ResultSet::nextDataFrame()
    {
        dataFrames_.push_back(DataFrame());
        return dataFrames_.back();
    }

    void ResultSet::getRawValues(const std::string& field_name,
                                 const std::string& group_by_field,
                                 std::vector<double>& data,
                                 std::vector<int>& dims,
                                 std::vector<std::string>& dimPaths)
    {
        // Find the dims based on the largest sequence counts in the fields


        // Compute Dims
//        std::vector<size_t> dimsList;
//        std::vector<size_t> exportDims;
//        size_t groupbyIdx = 1;
//        size_t totalGroupbyElements = 0;
//
//        int targetFieldIdx = 0;
//        int groupByFieldIdx = 0;
//        if (dataFrames_.size() > 0)
//        {
//            targetFieldIdx = dataFrames_.front().fieldIndexForNodeNamed(field_name);
//
//            if (group_by_field != "")
//            {
//                groupByFieldIdx = dataFrames_.front().fieldIndexForNodeNamed(group_by_field);
//            }
//
//            auto& targetField = dataFrames_.front().fieldAtIdx(targetFieldIdx);
//            dimPaths = targetField.dimPaths;
//
//            exportDims = targetField.exportDims;
//        }
//
//        for (auto& dataFrame : dataFrames_)
//        {
//            auto& targetField = dataFrame.fieldAtIdx(targetFieldIdx);
//            if (!targetField.dimPaths.empty() && dimPaths.size() < targetField.dimPaths.size())
//            {
//                dimPaths = targetField.dimPaths;
//                exportDims = targetField.exportDims;
//            }
//
//            size_t dimsLen = targetField.seqCounts.size();
//            if (dimsList.size() < dimsLen)
//            {
//                dimsList.resize(dimsLen, 0);
//            }
//
//            for (size_t cntIdx = 0; cntIdx < targetField.seqCounts.size(); ++cntIdx)
//            {
//                dimsList[cntIdx] = std::max(dimsList[cntIdx],
//                                            *std::max_element(targetField.seqCounts[cntIdx].begin(),
//                                                              targetField.seqCounts[cntIdx].end()));
//            }
//
//            if (group_by_field != "")
//            {
//                auto& groupByField = dataFrame.fieldAtIdx(groupByFieldIdx);
//                groupbyIdx = std::max(groupbyIdx, groupByField.seqCounts.size());
//
//                if (groupbyIdx > dimsList.size())
//                {
//                    dimPaths = {groupByField.dimPaths.back()};
//
//                    size_t groupbyElementsForFrame = 1;
//                    for (auto &seqCount: groupByField.seqCounts)
//                    {
//                        groupbyElementsForFrame *= seqCount.size();
//                    }
//
//                    totalGroupbyElements = std::max(totalGroupbyElements, groupbyElementsForFrame);
//                }
//                else
//                {
//                    dimPaths = {};
//                    for (size_t targetIdx = exportDims.size(); targetIdx < targetField.dimPaths.size(); ++targetIdx)
//                    {
//                        dimPaths.push_back(targetField.dimPaths[targetIdx]);
//                    }
//                }
//            }
//        }
//
//        auto allDims = dimsList;
//
//        if (groupbyIdx > 0)
//        {
//            // The groupby field occurs at the same or greater repetition level as the target field.
//            if (groupbyIdx > dimsList.size())
//            {
//                exportDims = {1};
//                dims[0] = { totalGroupbyElements };
//                allDims = dims;
//            }
//            // The groupby field occurs at a lower repetition level than the target field.
//            else
//            {
//                dims.resize(dimsList.size() - groupbyIdx);
//                for (size_t dimIdx = 0; dimIdx < groupbyIdx; ++dimIdx)
//                {
//                    dims[0]*= allDims[dimIdx];
//                }
//
//                // Filter out exportDims that are 0
//                std::vector<size_t> filteredExportDims;
//                for (size_t dimIdx = 2; dimIdx < dims.size(); ++dimIdx)
//                {
//                    dims[0]*= allDims[dimIdx];
//
//                    if (exportDims[dimIdx] > 0)
//                    {
//                        filteredExportDims.push_back(exportDims[dimIdx]);
//                    }
//                }
//                exportDims = filteredExportDims;
//            }
//        }
//        else
//        {
//            dims = allDims;
//        }
//
//        size_t totalRows = dims[0] * dataFrames_.size();
//
//
//        int rowLength = 1;
//        for (size_t dimIdx = 1; dimIdx < dims.size(); ++dimIdx)
//        {
//            rowLength *= dims[dimIdx];
//        }
//
//        data.resize(totalRows * rowLength, 10.0e10);
//        for (auto& dataFrame : dataFrames_)
//        {
//            auto& targetField = dataFrame.fieldAtIdx(targetFieldIdx);
//            getRowsForField(targetField,
//                            frameData,
//                            allDims,
//                            groupbyIdx);
//        }

    }

//    subroutine result_set__get_rows_for_field(self, target_field, data_rows, dims, groupby_idx)

    void getRowsForField(const DataField& targetField,
                         std::vector<double>& data,
                         const std::vector<size_t>& dims,
                         size_t groupbyIdx)
    {
//        size_t maxCounts = 0;
//        std::vector<size_t> idxs(targetField.data.size());
//        for (size_t i = 0; i < idxs.size(); ++i)
//        {
//            idxs[i] = i;
//        }
//
//        // Compute max counts
//        for (size_t i = 0; i < targetField.seqCounts.size(); ++i)
//        {
//            if (maxCounts < targetField.seqCounts[i].size())
//            {
//                maxCounts = targetField.seqCounts[i].size();
//            }
//        }
//
//        // Compute insert array
//        std::vector<std::vector<size_t>> inserts(targetField.seqCounts.size());
//        for (size_t repIdx = 0; repIdx < targetField.seqCounts.size(); ++repIdx)
//        {
//            inserts[repIdx] = product<size_t>(dims.begin() + repIdx, dims.end()) -
//                              targetField.seqCounts[repIdx] * product<size_t>(dims.begin() + repIdx + 1, dims.end());
//        }
//
//        // Inflate the data, compute the idxs for each data element in the result array
//        for (size_t dim_idx = dims.size() - 1; dim_idx >= 0; --dim_idx)
//        {
//            for (size_t insert_idx = 0; insert_idx < inserts[dim_idx].size(); ++insert_idx)
//            {
//                size_t num_inserts = inserts[dim_idx][insert_idx];
//                if (num_inserts > 0)
//                {
//                    size_t data_idx = product<size_t>(dims.begin() + dim_idx, dims.end()) * (insert_idx - 1) +
//                                      product<size_t>(dims.begin() + dim_idx, dims.end()) - num_inserts;
//
//                    for (size_t i = 0; i < idxs.size(); ++i)
//                    {
//                        if (idxs[i] > data_idx)
//                        {
//                            idxs[i] += num_inserts;
//                        }
//                    }
//                }
//            }
//        }
//
//        auto output = std::vector<double>(idxs.size(), 10.0e10);
//        output = slice(targetField.data, idxs);


    }

    bool ResultSet::isString(const std::string& fieldName) const
    {
//        return dataFrames_.front().fieldForNodeNamed(fieldName).isString;
        return false;
    }
}  // namespace bufr
}  // namespace Ingester
