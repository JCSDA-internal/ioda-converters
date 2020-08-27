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
        BufrMnemonicSet(const std::string& mnemonics, const Channels& channels);

        // Getters
        inline std::vector<std::string> getMnemonics() const { return mnemonics_; }
        inline std::string getMnemonicStr() const  { return mnemonicsStr_; }
        inline Channels getChannels() const  { return channels_; }
        inline size_t getMaxColumn() const { return maxColumn_; }
        inline size_t getSize() const  { return mnemonics_.size(); }

     private:
        std::string mnemonicsStr_;
        std::vector<std::string> mnemonics_;
        Channels channels_;
        size_t maxColumn_;

        static std::vector<std::string> tokenizeMnemonics(const std::string& mnemonics);
    };
}  // namespace Ingester
