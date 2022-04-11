/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <vector>
#include <string>
#include <map>
#include <set>

#include "DataProvider.h"

namespace Ingester {
namespace bufr {
    struct QueryData
    {
        bool isMissing;
        int nodeId;
        std::vector<std::string> pathComponents;
        bool isString;
        std::vector<size_t> seqPath;
        std::vector<size_t> dimIdxs;
        size_t idx;
        bool requiresIdx;
    };

    class SubsetTable
    {
     public:
        SubsetTable() = delete;
        explicit SubsetTable(const DataProvider& dataProvider);
        ~SubsetTable() = default;

        std::vector<QueryData> allQueryData();
        QueryData dataForQuery(const std::vector<std::string>& queryComponents) const;

     private:
        const DataProvider& dataProvider_;
        std::map<std::string, QueryData> queryMap_;
        std::vector<std::string> queryMapKeys_;

        void initialize();
        std::vector<size_t> dimPathIdxs(std::vector<size_t> seqPath) const;
        std::vector<std::string> makePathComponents(std::vector<size_t> seqPath, int nodeIdx);
        std::string mapKey(const std::vector<std::string>& pathComponents, size_t idx = 0) const;
        std::string mapKey(const std::shared_ptr<QueryData> query) const;
    };
}  // namespace bufr
}  // namespace Ingester

