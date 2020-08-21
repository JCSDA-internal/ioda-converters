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


using namespace Ingester;
using namespace std;

const string PATH_SEPERATOR =
#if defined _WIN32 || defined __CYGWIN__
    "\\";
#else
    "/";
#endif


const string FILENAME = "filename";
const string MNEMONIC_SETS_YAML_SECTION = "mnemonicSets";
const string MNEMONIC_STR_YAML_NAME = "mnemonics";
const string CHANNEL_NAME = "channels";

BufrDescription::BufrDescription(const eckit::Configuration& conf, const string& basePath)
{
    setFilepath(basePath + PATH_SEPERATOR + conf.getString(FILENAME));
    auto subConf = conf.getSubConfiguration(MNEMONIC_SETS_YAML_SECTION);

    for (const auto& mnemonicSetConf : subConf.getSubConfigurations())
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

        addMnemonicSet(BufrMnemonicSet(mnemonicSetConf.getString(MNEMONIC_STR_YAML_NAME),
                                       channels));

    }
}

void BufrDescription::addMnemonicSet(BufrMnemonicSet mnemonicSet)
{
    mnemonicSets_.push_back(mnemonicSet);
}
