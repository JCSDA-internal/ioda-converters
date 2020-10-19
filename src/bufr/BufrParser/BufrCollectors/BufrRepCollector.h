/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "BufrAccumulator.h"
#include "BufrCollector.h"
#include "BufrParser/BufrMnemonicSet.h"
#include "BufrParser/BufrTypes.h"


namespace Ingester
{
    class BufrRepCollector : public BufrCollector
    {
     public:
        explicit BufrRepCollector(const int fileUnit, const BufrMnemonicSet mnemonicSet);
        ~BufrRepCollector() = default;

        void collect() final;

     private:
        std::vector<double> scratchData_;
    };
}  // namespace Ingester
