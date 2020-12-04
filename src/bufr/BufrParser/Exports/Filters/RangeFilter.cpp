/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "RangeFilter.h"

namespace Ingester
{
    RangeFilter::RangeFilter(const std::string& mnemonic, const std::vector<float>& extents) :
      mnemonic_(mnemonic),
      extents_(extents)
    {
    }

    void RangeFilter::apply(IngesterArray& array)
    {

    }
}  // namespace Ingester