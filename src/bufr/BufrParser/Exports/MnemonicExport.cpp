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

    MnemonicExport::MnemonicExport(const eckit::Configuration& conf) :
    mnemonic_(conf.getString("name"))
    {

    }

    IngesterArray MnemonicExport::exportData(BufrDataMap map)
    {
        return map.at(mnemonic_);
    }
}  // namespace Ingsester