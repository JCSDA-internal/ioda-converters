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

#include "BufrTypes.h"
#include "BufrMnemonicSet.h"
#include "BufrAccumulator.h"


namespace BufrParser
{
    /// \brief Collectors know how to use the BUFR interface to grab data associated with
    /// configured mnemonicSets.
    class BufrCollector
    {
     public:
        BufrCollector(const int fortranFileId, const BufrMnemonicSet mnemonicSet);
        virtual ~BufrCollector() = default;

        /// \brief Grab the data
        virtual void collect() = 0;

        /// \brief Get the data we want from the accumulator and make our data map. Resets
        /// the accumulator.
        BufrDataMap finalize();

     protected:
        /// \brief Fortran file ID for the open BUFR file
        const int fortranFileId_;

        /// \brief Accumulator to collect the data we are collecting
        BufrAccumulator accumulator_;

        /// \brief Specifies the mnemonics and channels this collector gets from the BUFR file.
        const BufrMnemonicSet mnemonicSet_;
    };
}  // namespace BufrParser
