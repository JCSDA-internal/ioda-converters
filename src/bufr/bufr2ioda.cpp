/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <string>
#include <iostream>
#include <ostream>
#include <iomanip>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "BufrParser/BufrDescription.h"
#include "BufrParser/BufrMnemonicSet.h"
#include "BufrParser/BufrParser.h"
#include "IodaEncoder/IodaDescription.h"
#include "IodaEncoder/IodaEncoder.h"
#include "IodaEncoder/DataContainer.h"
#include "IodaEncoder/ParserFactory.h"
#include "IodaEncoder/Parser.h"


namespace Bufr2Ioda
{
    void parse(std::string yamlPath)
    {
        std::unique_ptr<eckit::YAMLConfiguration>
            yaml(new eckit::YAMLConfiguration(eckit::PathName(yamlPath)));

        if (yaml->has("observations"))
        {
            for (const auto& obsConf : yaml->getSubConfigurations("observations"))
            {
                if (!obsConf.has("obs space") ||
                    !obsConf.has("ioda"))
                {
                    throw eckit::BadParameter(
                        "Incomplete obs found. All obs must have a obs space and ioda.");
                }

                auto parser =
                    IodaEncoder::ParserFactory::create(obsConf.getSubConfiguration("obs space"));
                auto data = parser->parse();

                auto encoder = IodaEncoder::IodaEncoder(obsConf.getSubConfiguration("ioda"));
                encoder.encode(data);
            }
        }
        else
        {
            throw eckit::BadParameter("No section named \"observations\"");
        }
    }

    void registerParsers()
    {
        IodaEncoder::ParserFactory::registerParser<BufrParser::BufrParser>("bufr");
    }
}  // namespace Bufr2Ioda



int main(int argc, char **argv)
{
    if (argc < 2)
    {
        throw eckit::BadParameter("Missing argument. Must include YAML file path.");
    }

    Bufr2Ioda::registerParsers();
    Bufr2Ioda::parse(std::string(argv[1]));

    return 0;
}

