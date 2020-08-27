/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"


namespace Ingester
{
    class BufrMnemonicSet;

    class BufrDescription
    {
     public:
        BufrDescription() = default;
        explicit BufrDescription(const eckit::Configuration& conf, const std::string& basePath);

        void addMnemonicSet(BufrMnemonicSet mnemonicSet);

        inline void setFilepath(const std::string& filepath) { filepath_ = filepath; }
        inline std::vector<BufrMnemonicSet>& getMnemonicSets() { return mnemonicSets_; }
        inline std::string filepath() { return filepath_; }

     private:
        std::vector<BufrMnemonicSet> mnemonicSets_;
        std::string filepath_;
    };
}  // namespace Ingester
