/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "MnemonicExport.h"

#include "IngesterTypes.h"


namespace Ingester
{
    MnemonicExport::MnemonicExport(std::string mnemonic, Transforms transforms) :
      mnemonic_(mnemonic),
      transforms_(transforms)
    {
    }

    std::shared_ptr<DataObject> MnemonicExport::exportData(const BufrDataMap& map)
    {
        auto data = map.at(mnemonic_);
        applyTransforms(data);
        return std::make_shared<ArrayDataObject>(data);
    }

    void MnemonicExport::applyTransforms(IngesterArray& data)
    {
        for (auto transform : transforms_)
        {
            transform->apply(data);
        }
    }
}  // namespace Ingester
