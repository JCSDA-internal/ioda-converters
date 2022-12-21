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
#include "Tokenizer.h"
#include "QueryParser.h"

namespace Ingester {
namespace bufr {

    struct TargetComponent
    {
        std::string name;
        size_t branch;
        bool addsDimension;
        std::vector<size_t> filter;
    };

    /// \brief The information or Meta data for a BUFR field whose data we wish to capture when
    /// we execute a query.
    struct Target
    {
        std::string name;
        std::string queryStr;
        std::string unit;
        TypeInfo typeInfo;
        std::vector<int> nodeIds;
        bool anySubset = false;
        std::vector<std::shared_ptr<TargetComponent>> path;
        size_t numDimensions = 0;

        std::vector<std::string> dimPaths;
        std::vector<int> exportDimIdxs;
        std::vector<int> seqPath;


        Target() = default;

        void setPath(const std::vector<std::shared_ptr<TargetComponent>>& components)
        {
            std::string currentPath;
            for (const auto& component : components)
            {
//                if (component->isQueryComponent)
//                {
//                    if (!currentPath.empty()) currentPath.append("/");
//                    currentPath.append(component->name);
//                    dimPaths.push_back(currentPath);
//                }

                if (component->addsDimension || component->filter.size() > 1)
                {
                    numDimensions++;
                }
            }

            path = components;
        }
    };

    typedef std::vector<std::shared_ptr<Target>> Targets;
}  // namespace bufr
}  // namespace Ingester
