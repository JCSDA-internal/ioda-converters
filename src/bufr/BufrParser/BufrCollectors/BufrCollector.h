/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <set>
#include <string>

#include "Eigen/Dense"

#include "BufrParser/BufrTypes.h"
#include "BufrAccumulator.h"


namespace Ingester
{
    class BufrCollector
    {
     public:
        BufrCollector(const int fileUnit, const BufrAccumulator accumulator);
        virtual ~BufrCollector() = default;

        virtual void collect() = 0;
        virtual BufrDataMap finalize() = 0;

     protected:
        const int fileUnit_;
        BufrAccumulator accumulator_;
    };
}  // namespace Ingester
