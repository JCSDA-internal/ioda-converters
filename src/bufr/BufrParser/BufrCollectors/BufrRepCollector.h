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
    /// \brief Collector that uses the BUFR interface ufbrep call to grab data (multi col data).
    class BufrRepCollector : public BufrCollector
    {
     public:
        explicit BufrRepCollector(const int fortranFileId, const BufrMnemonicSet& mnemonicSet);
        ~BufrRepCollector() = default;

        /// \brief Grab the next section of data
        void collect() final;

     private:
        /// \brief Pre-allocated buffer to hand to the Fortran interface.
        std::vector<double> scratchData_;
        std::vector<FloatType> floatTypeScratchData_;
    };
}  // namespace Ingester
