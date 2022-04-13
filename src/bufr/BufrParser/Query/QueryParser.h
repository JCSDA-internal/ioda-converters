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

    /// \brief Parses a user supplied query string into its component parts.
    /// \note Will be refactored to properly tokenize the query string.
    class QueryParser
    {
     public:
        /// \brief Split a multi query (ex: ["*/CLONH", "*/CLON"]) into a vector of single queries.
        /// \param query The query to split.
        static std::vector<std::string> splitMultiquery(const std::string& query);

        /// \brief Split a single query (ex: "*/ROSEQ1/ROSEQ2/PCCF[2]") into its component parts.
        /// \param query The query to split.
        /// \param[out] subset The subset part of the query (ex: *).
        /// \param[out] mnemonics  Query path components (ex: ["ROSEQ1", "ROSEQ2", "PCCF"]).
        /// \param[out] index The index associated with this query (ex: 2).
        static void splitQueryStr(const std::string& query,
                                  std::string& subset,
                                  std::vector<std::string>& mnemonics,
                                  int& index);

     private:
        /// \brief Private constructor.
        QueryParser();
    };
}  // namespace bufr
}  // namespace Ingester
