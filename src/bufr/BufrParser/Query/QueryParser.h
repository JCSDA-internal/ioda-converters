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

    /// \brief A component of a query string. Abstract base class.
    struct QueryComponent
    {
        std::string name;
        size_t index = 0;
        std::vector<size_t> filter;

        virtual ~QueryComponent() = default;
    };

    /// \brief A component of a query string that represents a subset.
    struct SubsetComponent : public QueryComponent
    {
        bool isAnySubset = false;

        SubsetComponent()
        {
            name = "*";
            isAnySubset = true;
        }

        /// \brief Parse a subset component from a token.
        static auto parse(std::vector<std::shared_ptr<Token>> tokens)
        {
            const auto& token = tokens[0];

            auto component = std::make_shared<SubsetComponent>();;
            component->name = token->str();
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
                                          "string: " + component->name);
            }

            return component;
        }
    };

    /// \brief == operator for SubsetComponent.
    inline bool operator==(const SubsetComponent& lhs, const SubsetComponent& rhs)
    {
        return lhs.name == rhs.name && lhs.isAnySubset == rhs.isAnySubset;
    }

    /// \brief A component of a query string that represents a part of a path.
    struct PathComponent : public QueryComponent
    {
        static auto parse(const std::vector<std::shared_ptr<Token>> tokens)
        {
            if (tokens.size() < 1 && tokens.size() > 2)
            {
                throw eckit::BadParameter("QueryParser::parseQueryToken: Invalid path component "
                                          "query string: " + tokens[0]->str());
            }

            auto component = std::make_shared<PathComponent>();

            if (std::dynamic_pointer_cast<MnemonicToken>(tokens[0]))
            {
                component->name = tokens[0]->str();
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

    /// \brief == operator for PathComponent.
    /// \param lhs The left hand side of the operator.
    /// \param rhs The right hand side of the operator.
    inline bool operator==(const PathComponent& lhs, const PathComponent& rhs)
    {
        return lhs.name == rhs.name && lhs.index == rhs.index && lhs.filter == rhs.filter;
    }

    /// \brief A query. Contains the components that make up a query.
    struct Query
    {
        std::shared_ptr<SubsetComponent> subset;
        std::vector<std::shared_ptr<PathComponent>> path;

        /// \brief Constructor.
        Query() : subset(std::make_shared<SubsetComponent>()),
                  path({}),
                  queryStr_(subset->name) {}

        /// \brief Constructor.
        /// \param components The components that make up the query.
        explicit Query(std::vector<std::shared_ptr<QueryComponent>> components)
        {
            std::stringstream pathStr;
            if (auto subsetComponent = std::dynamic_pointer_cast<SubsetComponent>(components[0]))
            {
                subset = subsetComponent;
                pathStr << subset->name;
            }
            else
            {
                throw eckit::BadParameter(
                    "QueryParser::parseQueryToken: Invalid query string: " + queryStr_);
            }

            for (size_t i = 1; i < components.size(); ++i)
            {
                if (auto pathComponent = std::dynamic_pointer_cast<PathComponent>(components[i]))
                {
                    path.push_back(pathComponent);
                    pathStr << "/" << pathComponent->name;

                    // Add index string
                    if (pathComponent->index > 0)
                    {
                        pathStr << "[" << pathComponent->index << "]";
                    }

                    // Add filter string
                    if (!pathComponent->filter.empty())
                    {
                        pathStr << "{";
                        for (const auto filterIdx : pathComponent->filter)
                        {
                            pathStr << "" << filterIdx;
                            if (filterIdx != pathComponent->filter.back()) pathStr << ",";
                        }
                        pathStr << "}";
                    }
                }
                else
                {
                    throw eckit::BadParameter(
                        "QueryParser::parseQueryToken: Invalid query string: " + queryStr_);
                }
            }

            queryStr_ = pathStr.str();
        }

        /// \brief Returns the query string.
        /// \return The query string.
        std::string str() const
        {
            return queryStr_;
        }

     private:
        std::string queryStr_;
    };

    /// \brief == Implements operator for Query.
    inline bool operator==(const Query& lhs, const Query& rhs)
    {
        return lhs.str() == rhs.str();
    }

    /// \brief != Implements operator for Query.
    inline bool operator!=(const Query& lhs, const Query& rhs)
    {
        return !(lhs == rhs);
    }

    /// \brief < Implements operator for Query.
    inline bool operator<(const Query& lhs, const Query& rhs)
    {
        return lhs.str() < rhs.str();
    }

    /// \brief Parses a user supplied query string into its component parts.
    class QueryParser
    {
     public:
        /// \brief Parses a query string into its component parts.
        /// \param queryStr The query string to parse.
        /// \return The components of the query string as Query objects.
        static std::vector<Query> parse(const std::string& queryStr);

     private:
        /// \brief Private constructor.
        QueryParser();
    };
}  // namespace bufr
}  // namespace Ingester
