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

#include "BufrTypes.h"


namespace Ingester
{
    class BufrMnemonicSet;
    class Export;

    class BufrDescription
    {
     public:
        BufrDescription() = default;
        explicit BufrDescription(const eckit::Configuration& conf);

        void addMnemonicSet(BufrMnemonicSet mnemonicSet);
        void addExport(std::string key, std::shared_ptr<Export> bufrExport);

        inline void setFilepath(const std::string& filepath) { filepath_ = filepath; }
        inline std::vector<BufrMnemonicSet> getMnemonicSets() const { return mnemonicSets_; }
        inline std::string filepath() const { return filepath_; }
        inline ExportMap getExportMap() const { return exportMap_; }

     private:
        std::vector<BufrMnemonicSet> mnemonicSets_;
        std::string filepath_;
        ExportMap exportMap_;
    };
}  // namespace Ingester
