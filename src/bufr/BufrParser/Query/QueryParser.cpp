/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "QueryParser.h"

#include <iostream>
#include <algorithm>

#include "eckit/exception/Exceptions.h"

#include "Parser/Tokenizer.h"

namespace Ingester {
namespace bufr {
    std::vector<Query> QueryParser::parse(const std::string& queryStr)
    {
        const auto tokens = Tokenizer::tokenize(queryStr);

        if (tokens.empty())
        {
            throw eckit::BadParameter("QueryParser::parse: Invalid query string: " + queryStr);
        }

        const auto& token = tokens[0];

        if (!std::dynamic_pointer_cast<QueryToken>(token) &&
            !std::dynamic_pointer_cast<MultiQueryToken>(token))
        {
            throw eckit::BadParameter("QueryParser::parse: Invalid query string: " + queryStr);
        }

        std::vector<Query> queries;
        for (const auto& queryToken : tokens[0]->queryTokens())
        {
            auto componentTokens = queryToken->split();

            Query query;
            query.queryStr = queryStr;
            query.subset = SubsetComponent::parse(componentTokens.front());

            for (auto compIt = componentTokens.begin() + 1;
                 compIt != componentTokens.end();
                 ++compIt)
            {
                query.path.push_back(PathComponent::parse(*compIt));
            }

            query.filter = FilterComponent::parse(componentTokens.back());

            queries.push_back(query);
        }

        return queries;
    }
}  // namespace bufr
}  // namespace Ingester
