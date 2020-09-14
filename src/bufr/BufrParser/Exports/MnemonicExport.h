//
// Created by Ronald McLaren on 9/2/20.
//

#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"

#include "Export.h"
#include "IngesterTypes.h"
#include "DataObject/ArrayDataObject.h"


namespace Ingester
{
    class MnemonicExport : public Export
    {
     public:
        explicit MnemonicExport(std::string mnemonicStr);
        ~MnemonicExport() override = default;

        std::shared_ptr<DataObject> exportData(BufrDataMap map);

     private:
        std::string mnemonic_;
    };
}   // namespace Ingester
