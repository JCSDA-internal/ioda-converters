/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "QuerySet.h"


namespace Ingester {
namespace bufr {

    std::vector<std::string> QuerySet::names() const
    {
        std::vector<std::string> names;
        for (auto const& query : queryList_)
        {
            names.push_back(query.first);
        }

        return names;
    }

}  // namespace bufr
}  // namespace Ingester
