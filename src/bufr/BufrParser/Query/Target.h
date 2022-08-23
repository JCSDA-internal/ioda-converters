/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>
#include <unordered_map>

#include "DataProvider.h"

namespace Ingester {
namespace bufr {
    /// \brief The information or Meta data for a BUFR field whose data we wish to capture when
    /// we execute a query.
    struct Target
    {
        std::string name;
        std::string queryStr;
        std::string unit;
        std::vector<int> seqPath;
        std::vector<int> nodeIds;
        std::vector<std::string> dimPaths;
        std::vector<int> exportDimIdxs;
        TypeInfo typeInfo;
    };

    typedef std::vector<std::shared_ptr<Target>> Targets;
}  // namespace bufr
}  // namespace Ingester
