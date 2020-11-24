/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <map>
#include <memory>
#include <string>
#include <vector>

#include "BufrTypes.h"


namespace BufrParser
{
    class BufrMnemonicSet;
    class BufrAccumulator;
    class BufrCollector;

    /// \brief Manager of collectors.
    class BufrCollectors
    {
     public:
        explicit BufrCollectors(unsigned int fortranFileId);
        ~BufrCollectors() = default;

        /// \brief Add collectors for mnemonic sets
        /// \param mnemonicSets list of BufrMnemonicSet to use
        void addMnemonicSets(const std::vector<BufrMnemonicSet>& mnemonicSets);

        /// \brief Add collector for a mnemonic set
        /// \param mnemonicSet BufrMnemonicSet to use
        void addMnemonicSet(const BufrMnemonicSet& mnemonicSet);

        /// \brief Cause all the collectors to grab the next peaces of data from the BUFR file.
        void collect();

        /// \brief Finalize all the collectors and assemble the resulting data into a map.
        BufrDataMap finalize();

     private:
        /// \brief Fortran file ID for the open BUFR file
        unsigned int fortranFileId_;

        /// \brief Collection of all the collectors being managed.
        std::vector<std::shared_ptr<BufrCollector>> collectors_;
    };
}  // namespace BufrParser
