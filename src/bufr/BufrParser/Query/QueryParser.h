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

#include "Tokenizer.h"

namespace Ingester {
namespace bufr {

    struct QueryComponent
    {
    };

    struct SubsetComponent : QueryComponent
    {
        std::string subset;
        bool isAnySubset = false;

        static auto parse(std::vector<std::shared_ptr<Token>> tokens)
        {
            const auto& token = tokens[0];

            auto component = std::make_shared<SubsetComponent>();;
            component->subset = token->str();
            if (std::dynamic_pointer_cast<AnySubset>(token))
            {
                component->isAnySubset = true;
            }
            else if (std::dynamic_pointer_cast<MnemonicToken>(token))
            {
                component->isAnySubset = false;
            }
            else
            {
                throw eckit::BadParameter("QueryParser::parseQueryToken: Invalid subset in query "
                                          "string: " + component->subset);
            }

            return component;
        }
    };

    struct PathComponent : QueryComponent
    {
        std::string mnemonic;
        size_t index = 0;
        std::vector<size_t> filter;

        static auto parse(std::vector<std::shared_ptr<Token>> tokens)
        {
            if (tokens.size() < 1 && tokens.size() > 2)
            {
                throw eckit::BadParameter("QueryParser::parseQueryToken: Invalid path component "
                                          "query string: " + tokens[0]->str());
            }

            auto component = std::make_shared<PathComponent>();

            if (std::dynamic_pointer_cast<MnemonicToken>(tokens[0]))
            {
                component->mnemonic = tokens[0]->str();
            }
            else
            {
                throw eckit::BadParameter("QueryParser::parseQueryToken: Invalid path component "
                                          "query string: " + tokens[0]->str());
            }

            for (const auto& token : tokens)
            {
                if (auto indexToken = std::dynamic_pointer_cast<IndexToken>(token))
                {
                    component->index = indexToken->index();
                }
                else if (auto filterToken = std::dynamic_pointer_cast<FilterToken>(token))
                {
                    component->filter = filterToken->indices();
                }
            }

            return component;
        }
    };


    struct Query
    {
        std::string queryStr;
        std::shared_ptr<SubsetComponent> subset;
        std::vector<std::shared_ptr<PathComponent>> path;
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
    };
}  // namespace bufr
}  // namespace Ingester
