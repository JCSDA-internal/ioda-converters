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
        for (const auto& queryToken : token->queryTokens())
        {
            auto componentTokens = queryToken->split();

            std::vector<std::shared_ptr<QueryComponent>> path;

            for (auto compIt = componentTokens.begin();
                 compIt != componentTokens.end();
                 ++compIt)
            {
                if (compIt == componentTokens.begin())
                {
                    path.push_back(SubsetComponent::parse(*compIt));
                }
                else
                {
                    path.push_back(PathComponent::parse(*compIt));
                }
            }

            queries.emplace_back(path);
        }

        return queries;
    }
}  // namespace bufr
}  // namespace Ingester
