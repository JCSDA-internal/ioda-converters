/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "bufr2ioda_func.h"
#include "oops/util/Logger.h"

namespace Ingester
{
    std::shared_ptr<DataContainer> parse1(const std::string& yamlPath, std::size_t numMsgs)
    {
        ParseFactory parseFactory;
        parseFactory.registerObject<BufrParser>("bufr");

        std::unique_ptr<eckit::YAMLConfiguration>
            yaml(new eckit::YAMLConfiguration(yamlPath));
           // yaml(new eckit::YAMLConfiguration(eckit::PathName(yamlPath)));
        auto data = nullptr;
        oops::Log::info() << " Start to process Bufr Data" << std::endl;
        if (yaml->has("observations"))
        {
            oops::Log::info() << "Get yaml Data" << std::endl;
            for (const auto& obsConf : yaml->getSubConfigurations("observations"))
            {
                if (!obsConf.has("obs space") ||
                    !obsConf.has("ioda"))
                {
                    eckit::BadParameter(
                        "   Incomplete obs found. All obs must have a obs space and ioda.");
                }

                auto configuration = obsConf.getSubConfiguration("obs space");
                auto parser = parseFactory.create("bufr", configuration);
                auto data = parser->parse(numMsgs);
                oops::Log::info() << "Get Bufr Data" << std::endl;
                //auto encoder = IodaEncoder(obsConf.getSubConfiguration("ioda"));
                //encoder.encode(data);
                return data;
            }
        }
        else
        {
            eckit::BadParameter("No section named \"observations\"");
        }
        oops::Log::info() << "Return Bufr Data" << std::endl;
        return data;
    }

    void encode_save(const std::string& yamlPath, std::shared_ptr<DataContainer> data)
    {
        ParseFactory parseFactory;
        parseFactory.registerObject<BufrParser>("bufr");

        std::unique_ptr<eckit::YAMLConfiguration>
            yaml(new eckit::YAMLConfiguration(eckit::PathName(yamlPath)));
        oops::Log::info() << " Start to process Bufr Data" << std::endl;
        if (yaml->has("observations"))
        {
            oops::Log::info() << "Get yaml Data" << std::endl;
            for (const auto& obsConf : yaml->getSubConfigurations("observations"))
            {
                if (!obsConf.has("obs space") ||
                    !obsConf.has("ioda"))
                {
                    eckit::BadParameter(
                        "   Incomplete obs found. All obs must have a obs space and ioda.");
                }
                oops::Log::info() << "Start encoder Data" << std::endl;
                auto encoder = IodaEncoder(obsConf.getSubConfiguration("ioda"));
                oops::Log::info() << "start save Data" << std::endl;
                encoder.encode(data);
            }
        }
        else
        {
            eckit::BadParameter("No section named \"observations\"");
        }
        oops::Log::info() << "Return Bufr Data" << std::endl;
    }


}  // namespace Ingester

