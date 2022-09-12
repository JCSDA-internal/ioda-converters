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
#include "BufrParser/BufrParser.h"
#include "IodaEncoder/IodaDescription.h"
#include "IodaEncoder/IodaEncoder.h"
#include "DataContainer.h"

#include "ParserFactory.h"


namespace Ingester
{
    void parse(const std::string& yamlPath, std::size_t numMsgs = 0)
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
                    eckit::BadParameter(
                        "Incomplete obs found. All obs must have a obs space and ioda.");
                }

                auto parser = ParserFactory::create(obsConf.getSubConfiguration("obs space"));
                auto data = parser->parse(numMsgs);

                auto encoder = IodaEncoder(obsConf.getSubConfiguration("ioda"));
                encoder.encode(data);
            }
        }
        else
        {
            eckit::BadParameter("No section named \"observations\"");
        }
    }

    void registerParsers()
    {
        ParserFactory::registerParser<BufrParser>("bufr");
    }
}  // namespace Ingester


static void showHelp()
{
    std::cerr << "Usage: bufr2ioda.x [-n NUM_MESSAGES] YAML_PATH"
              << "Options:\n"
              << "  -h,  Show this help message\n"
              << "  -n NUM_MESSAGES,  Number of BUFR messages to parse."
              << std::endl;
}


int main(int argc, char **argv)
{
    if (argc < 2)
    {
        showHelp();
        return 0;
    }

    std::string yamlPath;
    std::size_t numMsgs = 0;

    std::size_t argIdx = 1;
    while (argIdx < static_cast<std::size_t> (argc))
    {
        if (strcmp(argv[argIdx], "-n") == 0)
        {
            if (static_cast<std::size_t> (argc) > argIdx + 1)
            {
                numMsgs = atoi(argv[argIdx + 1]);
            }
            else
            {
                showHelp();
                return 0;
            }

            argIdx += 2;
        }
        else if (strcmp(argv[argIdx], "-h") == 0)
        {
            showHelp();
            return 0;
        }
        else
        {
            yamlPath = std::string(argv[argIdx]);
            argIdx++;
        }
    }

    try
    {
        Ingester::registerParsers();
        Ingester::parse(yamlPath, numMsgs);
    }
    catch (const std::exception &e)
    {
        throw;
    }

    return 0;
}

