/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>
#include <memory>

#include "Parser/Tokenizer.h"

namespace Ingester {
namespace bufr {

    struct QueryComponent
    {
        std::string str;
    };

    struct SubsetComponent : QueryComponent
    {
    };

    struct PathComponent : QueryComponent
    {
        size_t index;
    };

    struct FilterComponent : QueryComponent
    {
        std::vector<size_t> indices;
    };

    struct Query
    {
        std::string queryStr;
        std::shared_ptr<SubsetComponent> subset;
        std::vector<std::shared_ptr<PathComponent>> path;
        std::shared_ptr<FilterComponent> filter;
    };

//    typedef std::vector<std::shared_ptr<QueryComponent>> Query;

    /// \brief Parses a user supplied query string into its component parts.
    /// \note Will be refactored to properly tokenize the query string.
    class QueryParser
    {
     public:
        static std::vector<Query> parse(const std::string& queryStr);

     private:
        /// \brief Private constructor.
        QueryParser();

        static Query parseQueryToken(std::shared_ptr<QueryToken> query);
    };
}  // namespace bufr
}  // namespace Ingester
