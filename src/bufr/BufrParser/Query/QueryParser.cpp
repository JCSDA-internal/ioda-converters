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

        // parse into query components
        std::vector<Query> queries;
        for (const auto& queryToken : tokens[0]->queryTokens())
        {
            queries.push_back(parseQueryToken(queryToken));
            queries.back().queryStr = queryStr;
        }

        return queries;
    }

    Query QueryParser::parseQueryToken(const QueryToken& queryToken)
    {
        Query query;

        // Get the subset component
        auto componentTokens = queryToken.split();
        auto subsetTokens = componentTokens[0];
        if (subsetTokens.size() != 1)
        {
            throw eckit::BadParameter("QueryParser::parseQueryToken: Invalid subset in query string: " + queryToken.str());
        }

        // Parse the subset component
        auto subsetComponent = std::make_shared<SubsetComponent>();
        if (std::dynamic_pointer_cast<AnySubset>(subsetTokens[0]) ||
            std::dynamic_pointer_cast<MnemonicToken>(subsetTokens[0]))
        {
            subsetComponent->str = subsetTokens[0]->str();
            query.subset = std::move(subsetComponent);
        }
        else
        {
            throw eckit::BadParameter("QueryParser::parseQueryToken: Invalid subset component: " + subsetTokens[0]->str());
        }

        for (auto compIt = componentTokens.begin() + 1; compIt != componentTokens.end(); ++compIt)
        {
            auto component = *compIt;
            if (component.empty() || component.size() > 2)
            {
                throw eckit::BadParameter("QueryParser::parseQueryToken: Invalid query string: " + queryToken.str());
            }

            if (auto mnemonicToken = std::dynamic_pointer_cast<MnemonicToken>(component[0]))
            {
                auto pathComponent = std::make_shared<PathComponent>();
                pathComponent->str = mnemonicToken->str();

                if (component.size() == 2)
                {
                    if (auto indexToken = std::dynamic_pointer_cast<IndexToken>(component[1]))
                    {
                        pathComponent->index = indexToken->index();
                    }
                    else
                    {
                        throw eckit::BadParameter("QueryParser::parseQueryToken: Invalid index token: " + component[1]->str());
                    }
                }

                query.path.push_back(pathComponent);
            }
            else if (auto filterToken = std::dynamic_pointer_cast<FilterToken>(component[0]))
            {
                if (compIt != componentTokens.end() - 1)
                {
                    throw eckit::BadParameter("QueryParser::parseQueryToken: Filter must be at the end of the query string: " + queryToken.str());
                }

                auto filterComponent = std::make_shared<FilterComponent>();
                filterComponent->indices = filterToken->indices();
                query.filter = std::move(filterComponent);
            }
            else
            {
                throw eckit::BadParameter("QueryParser::parseQueryToken: Invalid query string: " + queryToken.str());
            }
        }

        return query;
    }
}  // namespace bufr
}  // namespace Ingester
