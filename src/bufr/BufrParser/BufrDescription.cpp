/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>
#include <memory>

#include "oops/util/IntSetParser.h"

#include "BufrDescription.h"
#include "BufrMnemonicSet.h"
#include "BufrTypes.h"

#include "Exports/MnemonicExport.h"
#include "Exports/DatetimeExport.h"
#include "Exports/Export.h"
#include "Exports/Transforms/Transform.h"
#include "Exports/Transforms/TransformBuilder.h"


namespace
{
    namespace ConfKeys
    {
        const char* Filename = "obsdatain";
        const char* MnemonicSets = "mnemonicSets";
        const char* Mnemonics = "mnemonics";
        const char* Channels = "channels";
        const char* Exports = "exports";
        const char* Datetime = "datetime";
        const char* Mnemonic = "mnemonic";
    }  // namespace ConfKeys
}  // namespace

namespace Ingester
{
    BufrDescription::BufrDescription(const eckit::Configuration &conf)
    {
        setFilepath(conf.getString(ConfKeys::Filename));

        for (const auto& mnemonicSetConf : conf.getSubConfigurations(ConfKeys::MnemonicSets))
        {
            Channels channels = {1};
            if (mnemonicSetConf.has(ConfKeys::Channels))
            {
                auto intChannels = oops::parseIntSet(mnemonicSetConf.getString(ConfKeys::Channels));
                channels = Channels(intChannels.begin(), intChannels.end());
            }

            addMnemonicSet(BufrMnemonicSet(
                mnemonicSetConf.getStringVector(ConfKeys::Mnemonics), channels));
        }

        auto exportConfs = conf.getSubConfiguration(ConfKeys::Exports);
        for (const auto& key : exportConfs.keys())
        {
            auto subconf = exportConfs.getSubConfiguration(key);

            if (subconf.has(ConfKeys::Datetime))
            {
                auto dtconf = subconf.getSubConfiguration(ConfKeys::Datetime);
                addExport(key, std::make_shared<DatetimeExport>(dtconf));
            }
            else if (subconf.has(ConfKeys::Mnemonic))
            {
                Transforms transforms = TransformBuilder::makeTransforms(subconf);
                addExport(key, std::make_shared<MnemonicExport>(
                    subconf.getString(ConfKeys::Mnemonic), transforms));
            }
        }
    }

    void BufrDescription::addMnemonicSet(const BufrMnemonicSet& mnemonicSet)
    {
        mnemonicSets_.push_back(mnemonicSet);
    }

    void BufrDescription::addExport(const std::string& key,
                                    const std::shared_ptr<Export>& bufrExport)
    {
        exportMap_.insert({key, bufrExport});
    }
}  // namespace Ingester
