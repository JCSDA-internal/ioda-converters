/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "MnemonicVariable.h"

#include <ostream>

#include "eckit/exception/Exceptions.h"

#include "IngesterTypes.h"


namespace Ingester
{
    MnemonicVariable::MnemonicVariable(const std::string& mnemonic, const Transforms& transforms) :
      mnemonic_(mnemonic),
      transforms_(transforms)
    {
    }

    std::shared_ptr<DataObject> MnemonicVariable::exportData(const BufrDataMap& map)
    {
        if (map.find(mnemonic_) == map.end())
        {
            std::stringstream errStr;
            errStr << "Mnemonic " << mnemonic_;
            errStr << " could not be found during export.";

            eckit::BadParameter(errStr.str());
        }

        auto data = map.at(mnemonic_);
        applyTransforms(data);
        return std::make_shared<ArrayDataObject>(data);
    }

    void MnemonicVariable::applyTransforms(IngesterArray& data)
    {
        for (auto transform : transforms_)
        {
            transform->apply(data);
        }
    }
}  // namespace Ingester
