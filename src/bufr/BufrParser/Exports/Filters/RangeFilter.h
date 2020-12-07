/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "Filter.h"

#include <string>
#include <vector>

#include "BufrParser/BufrTypes.h"


namespace Ingester
{
    class RangeFilter : public Filter
    {
     public:
        RangeFilter(const std::string& mnemonic, const std::vector<float>& extents);

        void apply(BufrDataMap& dataMap) final;

     private:
         const std::string mnemonic_;
         const std::vector<float> extents_;
    };
}  // namespace Ingester
