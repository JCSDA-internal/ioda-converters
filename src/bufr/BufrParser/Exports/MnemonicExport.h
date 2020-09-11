//
// Created by Ronald McLaren on 9/2/20.
//

#pragma once

#include <string>

#include "eckit/config/LocalConfiguration.h"

#include "Export.h"

#include "IngesterTypes.h"

namespace Ingester
{
    class MnemonicExport : public Export
    {
     public:
        explicit MnemonicExport(std::string mnemonicStr);
        ~MnemonicExport() override = default;

        IngesterArray exportData(BufrDataMap map);

     private:
        std::string mnemonic_;
    };
}   // namespace Ingester
