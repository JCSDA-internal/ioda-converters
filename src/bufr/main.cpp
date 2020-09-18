/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <string>
#include <iostream>
#include <iomanip>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/filesystem/PathName.h"

#include "BufrParser/BufrDescription.h"
#include "BufrParser/BufrMnemonicSet.h"
#include "BufrParser/BufrParser.h"
#include "IodaEncoder/IodaDescription.h"
#include "IodaEncoder/IodaEncoder.h"
#include "DataContainer.h"


void handleBadYaml(std::string additionalMsg="")
{
    std::cout << std::string("Must provide a YAML file that maps BUFR to IODA arguments.") << std::cout;

//    if (!additionalMsg.empty())
//    {
//        std::cout << additionalMsg << std::cout;
//    }

    abort();
}

void parseFile(std::string yamlPath)
{
    std::unique_ptr<eckit::YAMLConfiguration>
        yaml(new eckit::YAMLConfiguration(eckit::PathName(yamlPath)));

    auto inputPath = yaml->getString("inputpath");
    auto outputPath = yaml->getString("outputpath");

    if (yaml->has("bufr"))
    {
        auto conf = yaml->getSubConfiguration("bufr");
        auto bufrDesc = Ingester::BufrDescription(conf, inputPath);
        auto bufrParser = Ingester::BufrParser(bufrDesc);

        std::shared_ptr<DataContainer> data = bufrParser.parse();

        if (yaml->has("ioda"))
        {
            auto iodaDesc = Ingester::IodaDescription(yaml->getSubConfiguration("ioda"),
                                                      outputPath);
            auto encoder = Ingester::IodaEncoder(iodaDesc);
            encoder.encode(data);
        }
        else
        {
            handleBadYaml("No section named \"ioda\"");
        }
    }
    else
    {
        handleBadYaml("No section named \"bufr\"");
    }
}

int main(int argc, char **argv)
{
    if (argc < 1)
    {
        handleBadYaml();
    }

    parseFile(std::string(argv[0]));

    return 0;
}

