/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>

#include "BufrTypes.h"

namespace BufrParser
{
    /// \brief Defenition of BUFR mnemonics and associated channels of interest.
    class BufrMnemonicSet
    {
     public:
        explicit BufrMnemonicSet(const std::vector<std::string>& mnemonics,
                                 const Channels& channels = {1});

        // Getters
        inline std::vector<std::string> getMnemonics() const { return mnemonics_; }
        inline std::string getMnemonicsStr() const { return mnemonicsStr_; }
        inline Channels getChannels() const  { return channels_; }
        inline size_t getMaxColumn() const { return maxColumn_; }
        inline size_t getSize() const  { return mnemonics_.size(); }

     private:
        /// \brief Collection of BUFR mnemonics.
        const std::vector<std::string> mnemonics_;

        /// \brief String of assembled mnemonics to use when reading from the buffer interface
        const std::string mnemonicsStr_;

        /// \brief Collection of channels to read for each mnemonic
        const Channels channels_;

        /// \brief The value of the greatest channel.
        const size_t maxColumn_;

        /// \brief Concatinates mnemonics into a space seperated string.
        static std::string makeMnemonicsStr(std::vector<std::string> mnemonics);
    };
}  // namespace BufrParser
