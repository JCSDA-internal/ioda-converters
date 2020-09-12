/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <map>
#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"


namespace Ingester
{
    class BufrMnemonicSet;
    class Export;

    class BufrDescription
    {
     public:
        BufrDescription() = default;
        explicit BufrDescription(const eckit::Configuration& conf, const std::string& basePath);

        void addMnemonicSet(BufrMnemonicSet mnemonicSet);
        void addExport(std::string key, std::shared_ptr<Export> bufrExport);

        inline void setFilepath(const std::string& filepath) { filepath_ = filepath; }
        inline std::vector<BufrMnemonicSet>& getMnemonicSets() { return mnemonicSets_; }
        inline std::string filepath() { return filepath_; }
        inline std::map<std::string, std::shared_ptr<Export>>& getExportMap() { return exportMap_; }

     private:
        std::vector<BufrMnemonicSet> mnemonicSets_;
        std::string filepath_;
        std::map<std::string, std::shared_ptr<Export>> exportMap_;
    };
}  // namespace Ingester
