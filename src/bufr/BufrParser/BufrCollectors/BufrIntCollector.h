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
    /// \brief Collector that uses the BUFR interface ufbint call to grab data (single col data).
    class BufrIntCollector: public BufrCollector
    {
     public:
        explicit BufrIntCollector(const int fortranFileId, const BufrMnemonicSet mnemonicSet);
        ~BufrIntCollector() = default;

        /// \brief Grab the next section of data
        void collect() final;

     private:
        /// \brief Pre-allocated buffer to hand to the Fortran interface.
        std::vector<double> scratchData_;
    };
}  // namespace Ingester

