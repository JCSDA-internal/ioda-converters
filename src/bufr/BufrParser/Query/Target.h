//
// Created by Ronald McLaren on 7/13/22.
//

#pragma once

#include <string>
#include <vector>
#include <unordered_map>

#include "DataProvider.h"

namespace Ingester {
namespace bufr {
    /// \brief The information or Meta data for a BUFR field whose data we wish to capture when
    /// we execute a query.
    /// \note Will be refactored to use the SubsetTable object.
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
}  // Ingester
}  // bufr
