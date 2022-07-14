/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <vector>
#include <string>

namespace Ingester {
namespace bufr
{
    /// \brief Manages a collection of queries.
    class QuerySet
    {
     public:
        QuerySet() = default;
        ~QuerySet() = default;

        /// \brief Add a new query to the collection.
        /// \param[in] name The name of the query.
        /// \param[in] query The query string.
        void add(const std::string& name, const std::string& query)
        {
            queryList_.push_back({name, query});
        }

        /// \brief Returns the size of the collection.
        size_t size() const { return queryList_.size(); }

        /// \brief Returns the name of the query at the specified index.
        /// \param[in] idx The index of the query..
        /// \return The name of the query.
        std::string nameAt(size_t idx) const { return queryList_.at(idx).first; }

        /// \brief Returns the query string at the specified index.
        /// \param[in] idx The index of the query.
        /// \return The query string.
        std::string queryAt(size_t idx) const { return queryList_.at(idx).second; }

        /// \brief Returns the names of all the queries.
        /// \return A vector of the names of all the queries.
        std::vector<std::string> names() const;

     private:
        std::vector<std::pair<std::string, std::string>> queryList_;
    };
}  // namespace bufr
}  // namespace Ingester
