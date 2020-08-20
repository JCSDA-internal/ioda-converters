/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <time.h>
#include <iostream>
#include <iomanip>

#include <eckit/config/YAMLConfiguration.h>
#include <eckit/filesystem/PathName.h>

#include "oops/IntSetParser.h"
#include "BufrParser/BufrParser.h"
#include "BufrParser/BufrDescription.h"
#include "BufrParser/BufrMnemonicSet.h"
#include "IngesterData.h"

using namespace Ingester;
using namespace std;
using namespace eckit;


static const string CONFIG_FILE = "/Users/rmclaren/Work/bufr-tools/src/test_ingester.yaml";
static const string INPUT_FILE = "/Users/rmclaren/Work/sample-bufr-data/gdas/gdas.20200704/12/gdas.t12z.1bmhs.tm00.bufr_d";
static const string OUTPUT_FILE = "/Users/rmclaren/Temp/ioda.nc";


void test_createDescriptionManually()
{
    //Create Description
    auto description = BufrDescription();

    description.setFilepath(INPUT_FILE);

    auto set1 = BufrMnemonicSet("SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HOLS", {1});
    auto set2 = BufrMnemonicSet("SAZA SOZA BEARAZ SOLAZI", {1});
    auto set3 = BufrMnemonicSet("TMBR", oops::parseIntSet("1-15"));

    description.addMnemonicSet(set1);
    description.addMnemonicSet(set2);
    description.addMnemonicSet(set3);

    //Read some data
    auto bufrParser = BufrParser(description);
    auto data = bufrParser.parse(4);

    cout << data->get("TMBR") << endl;
}

void test_parsePartialFile()
{
    unique_ptr<YAMLConfiguration> yaml(new YAMLConfiguration(PathName(CONFIG_FILE)));

    auto dataPath = yaml->getString("datapath");

    for (const auto& conf : yaml->getSubConfigurations("bufr"))
    {
        auto description = BufrDescription(conf, dataPath);
        auto bufrParser = BufrParser(description);

        shared_ptr<IngesterData> data = bufrParser.parse(5);

        cout << data->get("TMBR") << endl;
    }
}

void test_parseWholeFile()
{
    unique_ptr<YAMLConfiguration> yaml(new YAMLConfiguration(PathName(CONFIG_FILE)));

    auto dataPath = yaml->getString("datapath");

    for (const auto& conf : yaml->getSubConfigurations("bufr"))
    {
        auto description = BufrDescription(conf, dataPath);
        auto bufrParser = BufrParser(description);

        shared_ptr<IngesterData> data = bufrParser.parse();
    }
}

void test_parseFileIncrementally()
{
    unique_ptr<YAMLConfiguration> yaml(new YAMLConfiguration(PathName(CONFIG_FILE)));

    auto dataPath = yaml->getString("datapath");

    for (const auto& conf : yaml->getSubConfigurations("bufr"))
    {
        auto description = BufrDescription(conf, dataPath);
        auto bufrParser = BufrParser(description);

        bool endReached = false;
        shared_ptr<IngesterData> data;
        do
        {
            auto nextData = bufrParser.parse(10);

            if (nextData->size() > 0)
            {
                data = nextData;
            }
            else
            {
                endReached = true;
            }
        } while(!endReached);

        cout << data->get("TMBR") << endl;
    }
}

int main(int, const char**)
{
    clock_t startTime = clock();

    test_createDescriptionManually();

    cout << "Took " << setprecision(2) << ((float)clock() - startTime)/CLOCKS_PER_SEC << " seconds to run." << endl;

    return 0;
}
