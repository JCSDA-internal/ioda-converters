/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "QuerySet.h"

#include <algorithm>
#include <iostream>

namespace Ingester {
namespace bufr {

    void QuerySet::add(const std::string& name, const std::string& queryStr)
    {
        std::vector<Query> queries;
        for (const auto& query : QueryParser::parse(queryStr))
        {
            if (query.subset == "*")
            {
                includesAllSubsets_ = true;
            }

            includedSubsets_.emplace(query.subset);
            queries.emplace_back(query);
        }

        queryMap_[name] = queries;
    }

    bool QuerySet::includesSubset(const std::string& subset) const
    {
        bool includesSubset = true;
        if (!includesAllSubsets_)
        {
            includesSubset = (includedSubsets_.find(subset) != includedSubsets_.end());
        }

        return includesSubset;
    }

    void QuerySet::limitSubsets(std::vector<std::string> subsets)
    {
        auto subsetsSet = std::set<std::string>(subsets.begin(), subsets.end());
        if (includesAllSubsets_)
        {
            includedSubsets_ = subsetsSet;

        }
        else
        {
            for(auto& s : includedSubsets_) std::cout << s << " ";
            std::cout << std::endl;

            std::vector<std::string> newSubsets;
            std::set_intersection(subsetsSet.begin(),
                                  subsetsSet.end(),
                                  includedSubsets_.begin(),
                                  includedSubsets_.end(),
                                  std::back_inserter(newSubsets));

            includedSubsets_ = std::set<std::string>(newSubsets.begin(),
                                                     newSubsets.end());

            for(auto& s : includedSubsets_) std::cout << s << " ";
            std::cout << std::endl;
        }

        includesAllSubsets_ = false;
    }

    std::vector<std::string> QuerySet::names() const
    {
        std::vector<std::string> names;
        for (auto const& query : queryMap_)
        {
            names.push_back(query.first);
        }

        return names;
    }

}  // namespace bufr
}  // namespace Ingester
