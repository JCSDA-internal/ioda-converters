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

    std::shared_ptr<DataObject> MnemonicExport::exportData(BufrDataMap map)
    {
        return std::make_shared<ArrayDataObject> (map.at(mnemonic_));
    }
}  // namespace Ingsester