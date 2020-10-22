/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"

#include "Export.h"
#include "IngesterTypes.h"
#include "DataObject/ArrayDataObject.h"


namespace Ingester
{
    /// \brief Exports parsed data associated with a mnemonic (ex: "CLAT")
    class MnemonicExport final : public Export
    {
     public:
        explicit MnemonicExport(std::string mnemonic);
        ~MnemonicExport() final = default;

        /// \brief Gets the requested data, applies transforms, and returns the requested data
        /// \param map BufrDataMap that contains the parsed data for each mnemonic
        std::shared_ptr<DataObject> exportData(BufrDataMap map) final;

     private:
        /// \brief The BUFR mnemonic of interest
        std::string mnemonic_;
    };
}  // namespace Ingester
