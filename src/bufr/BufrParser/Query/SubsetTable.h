/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <vector>
#include <string>
#include <unordered_map>
#include <set>

#include "DataProvider.h"

namespace Ingester {
namespace bufr {
    /// \brief Meta data for a query according to the BUFR subset table.
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
        TypeInfo typeInfo;
    };

    /// \brief Parses the BUFR message subset Meta data tables.
    class SubsetTable
    {
     public:
        SubsetTable() = delete;
        explicit SubsetTable(const DataProvider& dataProvider);
        ~SubsetTable() = default;

        /// \brief Returns all the query data elements.
        /// \returns BUFR table meta data for all the queries
        std::vector<QueryData> allQueryData();

        /// \brief Get meta data for the query with the given components.
        QueryData dataForQuery(const std::vector<std::string>& queryComponents) const;

     private:
        const DataProvider& dataProvider_;
        std::unordered_map<std::string, QueryData> queryMap_;
        std::vector<std::string> queryMapKeys_;

        void initialize();
        std::vector<size_t> dimPathIdxs(std::vector<size_t> seqPath) const;
        std::vector<std::string> makePathComponents(std::vector<size_t> seqPath, int nodeIdx);
        std::string mapKey(const std::vector<std::string>& pathComponents, size_t idx = 0) const;
        std::string mapKey(const std::shared_ptr<QueryData> query) const;

//        void parseSequence(std::shared_ptr<Node> parentNode, size_t& nodeIdx, size_t endIdx);
    };
}  // namespace bufr
}  // namespace Ingester

