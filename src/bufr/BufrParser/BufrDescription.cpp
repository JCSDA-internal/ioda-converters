/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>

#include "oops/util/IntSetParser.h"

#include "BufrDescription.h"
#include "BufrMnemonicSet.h"
#include "BufrTypes.h"


static const char* PATH_SEPERATOR =
#if defined _WIN32 || defined __CYGWIN__
    "\\";
#else
    "/";
#endif

static const char* FILENAME = "filename";
static const char* MNEMONIC_SETS_YAML_SECTION = "mnemonicSets";
static const char* MNEMONIC_STR_YAML_NAME = "mnemonics";
static const char* CHANNEL_NAME = "channels";
static const char* EXPORT_NAME = "exports";


namespace Ingester
{
    BufrDescription::BufrDescription(const eckit::Configuration &conf, const std::string &basePath)
    {
        setFilepath(basePath + std::string(PATH_SEPERATOR) + conf.getString(FILENAME));

        for (const auto& mnemonicSetConf : conf.getSubConfigurations(MNEMONIC_SETS_YAML_SECTION))
        {
            Channels channels;
            if (mnemonicSetConf.has(CHANNEL_NAME))
            {
                channels = oops::parseIntSet(mnemonicSetConf.getString(CHANNEL_NAME));
            }
            else
            {
                channels = {1};
            }

            addMnemonicSet(BufrMnemonicSet(
                mnemonicSetConf.getString(MNEMONIC_STR_YAML_NAME), channels));
        }

        for (const auto& key : conf.getSubConfiguration(EXPORT_NAME).keys())
        {
            addExport(key, conf.getSubConfiguration(EXPORT_NAME).getString(key));
        }
    }

    void BufrDescription::addMnemonicSet(BufrMnemonicSet mnemonicSet)
    {
        mnemonicSets_.push_back(mnemonicSet);
    }

    void BufrDescription::addExport(std::string key, std::string mnemonic)
    {
        exportMap_.insert({key, mnemonic});
    }

}  // namespace Ingester
