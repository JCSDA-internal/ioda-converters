//
// Created by Ronald McLaren on 9/2/20.
//

#include "MnemonicExport.h"

#include "IngesterTypes.h"


namespace Ingester
{
    MnemonicExport::MnemonicExport(std::string mnemonic) :
    mnemonic_(mnemonic)
    {
    }

    IngesterArray MnemonicExport::exportData(BufrDataMap map)
    {
        return map.at(mnemonic_);
    }
}  // namespace Ingsester