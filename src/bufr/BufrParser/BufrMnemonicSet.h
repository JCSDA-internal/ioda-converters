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

namespace Ingester
{
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
        const std::vector<std::string> mnemonics_;
        const std::string mnemonicsStr_;
        const Channels channels_;
        const size_t maxColumn_;

        static std::string makeMnemonicsStr(std::vector<std::string> mnemonics);
        static size_t findMaxChannel(const Channels& channels);
    };
}  // namespace Ingester
