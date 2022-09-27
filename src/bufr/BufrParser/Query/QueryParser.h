/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>

namespace Ingester {
namespace bufr {

    struct Query
    {
        std::string queryStr;
        std::string subset;
        std::vector<std::string> mnemonics;
        int index;
    };

    /// \brief Parses a user supplied query string into its component parts.
    /// \note Will be refactored to properly tokenize the query string.
    class QueryParser
    {
     public:
        static std::vector<Query> parse(const std::string& queryStr);

     private:
        /// \brief Split a multi query (ex: ["*/CLONH", "*/CLON"]) into a vector of single queries.
        /// \param query The query to split.
        static std::vector<std::string> splitMultiquery(const std::string& query);

        /// \brief Split a single query (ex: "*/ROSEQ1/ROSEQ2/PCCF[2]") into its component parts.
        /// \param query The query to split.
        static Query splitQueryStr(const std::string& query);

     private:
        /// \brief Private constructor.
        QueryParser();
    };
}  // namespace bufr
}  // namespace Ingester
