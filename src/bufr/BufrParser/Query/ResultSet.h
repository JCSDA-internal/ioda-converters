/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <vector>


namespace Ingester {
namespace bufr {
    const double MissingValue = 10e10;

    struct DataField
    {
        std::string name;
        std::string queryStr;
        bool isString;
        bool missing = false;
        std::vector<double> data;
        std::vector<size_t> seqPath;
        std::vector<std::vector<int>> seqCounts;
        std::vector<std::string> dimPaths;
        std::vector<int> exportDims;
    };

    class DataFrame
    {
     public:
        DataFrame(int fieldCnt)
        {
            fields_.resize(fieldCnt);
        }

        inline const DataField& fieldAtIdx(size_t idx) const { return fields_[idx]; }
        inline DataField& fieldAtIdx(size_t idx) { return fields_[idx]; }
        int fieldIndexForNodeNamed(const std::string& name) const
        {
            auto result = -1;
            for (size_t fieldIdx = 0; fieldIdx < fields_.size(); fieldIdx++)
            {
                if (fields_[fieldIdx].name == name)
                {
                    result = fieldIdx;
                    break;
                }
            }

            return result;
        }

     private:
        std::vector<DataField> fields_;
    };

    struct ResultBase
    {
        std::string field_name;
        std::string group_by_field_name;
        std::vector<int> dims;
        std::vector<std::string> dimPaths;
        std::map<std::string, int> fieldIdxMap_;

        virtual ~ResultBase() {}
        virtual void print() = 0;
    };

    template <typename T>
    struct Result : ResultBase
    {
        typedef T value_type;
        
        std::vector<T> data;

        void print() final
        {
            std::cout << data.size() << std::endl;
            for (auto val : data)
            {
                std::cout << val << ", ";
            }
        }
    };

    class ResultSet
    {
     public:
        ResultSet(const std::vector<std::string>& names);
        ~ResultSet();

        std::shared_ptr<ResultBase> get(const std::string& fieldName,
                                        const std::string& groupByFieldName = "") const;

        DataFrame& nextDataFrame();

        void indicateFieldIsString(int fieldIdx) { dataFrames_.front().fieldAtIdx(fieldIdx).isString = true; }
        bool isFieldStr(int fieldIdx) { return dataFrames_.front().fieldAtIdx(fieldIdx).isString; }

     private:
        std::vector<DataFrame> dataFrames_;
        std::vector<std::string> names_;
        std::vector<int> fieldWidths;

        void getRawValues(const std::string& fieldName,
                          const std::string& groupByField,
                          std::vector<double>& data,
                          std::vector<int>& dims,
                          std::vector<std::string>& dimPaths) const;

        void getRowsForField(const DataField& targetField,
                             std::vector<std::vector<double>>& dataRows,
                             const std::vector<int>& dims,
                             int groupbyIdx) const;

        bool isString(const std::string& fieldName) const;
    };
}  // namespace bufr
}  // namespace Ingester
