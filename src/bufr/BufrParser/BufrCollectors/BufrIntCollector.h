/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "BufrAccumulator.h"
#include "BufrCollector.h"
#include "BufrParser/BufrMnemonicSet.h"
#include "BufrParser/BufrTypes.h"

#pragma once

namespace Ingester
{
    class BufrIntCollector: public BufrCollector
    {
     public:
        explicit BufrIntCollector(const int fileUnit, const BufrMnemonicSet mnemonicSet);

        ~BufrIntCollector() override;

        void collect() final;
        IngesterArrayMap finalize() final;

     private:
        double *scratchData_;
        const BufrMnemonicSet mnemonicSet_;
    };
}  // namespace Ingester

