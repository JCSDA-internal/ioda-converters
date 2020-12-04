/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>

#include "BufrParser/BufrTypes.h"

namespace Ingester
{
    class Split
    {
     public:
        Split() = default;

        virtual std::vector<std::string> subCategories() = 0;
        virtual std::map<std::string, BufrDataMap> split(const BufrDataMap& dataMap) = 0;
    };
}  //namespace Ingester


