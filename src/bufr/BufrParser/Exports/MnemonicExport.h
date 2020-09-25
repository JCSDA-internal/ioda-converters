/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <memory>

#include "eckit/config/LocalConfiguration.h"

#include "Export.h"
#include "IngesterTypes.h"
#include "DataObject/ArrayDataObject.h"
#include "Transforms/Transform.h"


namespace Ingester
{
    class MnemonicExport final : public Export
    {
     public:
        explicit MnemonicExport(std::string mnemonicStr, Transforms transforms);
        ~MnemonicExport() final = default;

        std::shared_ptr<DataObject> exportData(BufrDataMap map) final;

     private:
        std::string mnemonic_;
        Transforms transforms_;

        void applyTransforms(IngesterArray& data);
    };
}  // namespace Ingester
