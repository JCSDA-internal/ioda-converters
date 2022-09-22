/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <climits>
#include <iostream>
#include <iomanip>
#include <string>  //
#include <float.h>

#include <ostream>
#include <time.h>
#include <vector>

#include "eckit/exception/Exceptions.h"

#include "DataObject.h"
#include "AircraftAltitudeVariable.h"


namespace
{
    namespace ConfKeys
    {
        const char* Latitude = "latitude";
        const char* Pressure = "pressure";
        const char* AircraftIndicatedAltitude = "aircraftIndicatedAltitude"; 
        const char* PressureAltitudeRelativeToMeanSeaLevel = "pressureAltitudeRelativeToMeanSeaLevel";
        const char* FlightLevel = "flightLevel";
        const char* Height = "height";
        const char* HeightOrAltitude = "heightOrAltitude";
        const char* FlightLevelST = "flightLevelST";
        const char* GroupByField = "group_by";
    }  // namespace ConfKeys
}  // namespace


namespace Ingester
{
    AircraftAltitudeVariable::AircraftAltitudeVariable(const std::string& exportName,
                                       const std::string& groupByField,
                                       const eckit::Configuration &conf) :
      Variable(exportName),
      latitudeQuery_(conf.getString(ConfKeys::Latitude)),
      groupByField_(groupByField)
    {
        if (conf.has(ConfKeys::Pressure))
        {
            std::cout << "PRLC Confkey" << std::endl;
            pressureQuery_ = conf.getString(ConfKeys::Pressure);
        }

	if (conf.has(ConfKeys::AircraftIndicatedAltitude))
        {            
            std::cout << "IALR Confkey" << std::endl;
            aircraftIndicatedAltitudeQuery_ = conf.getString(ConfKeys::AircraftIndicatedAltitude);
        }

        if (conf.has(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))
        {
            std::cout << "PSAL Confkey" << std::endl;
            pressureAltitudeRelativeToMeanSeaLevelQuery_ = conf.getString(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel);
        }

        if (conf.has(ConfKeys::FlightLevel))
        {
            std::cout << "FLVL Confkey" << std::endl;
            flightLevelQuery_ = conf.getString(ConfKeys::FlightLevel);
        }

        if (conf.has(ConfKeys::Height))
        {
            std::cout << "HEIT Confkey" << std::endl;
            heightQuery_ = conf.getString(ConfKeys::Height);
        }

        if (conf.has(ConfKeys::HeightOrAltitude))
        {
            std::cout << "HMSL Confkey" << std::endl;
            heightOrAltitudeQuery_ = conf.getString(ConfKeys::HeightOrAltitude);
        }

        if (conf.has(ConfKeys::FlightLevelST))
        {
            std::cout << "FLVLST Confkey" << std::endl;
            flightLevelSTQuery_ = conf.getString(ConfKeys::FlightLevelST);
        }

        if (conf.has(ConfKeys::GroupByField))
        {
            std::cout << "Groupby Confkey" << std::endl;
            groupByField_ = conf.getString(ConfKeys::GroupByField);
        }

        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> AircraftAltitudeVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);
        static const float missing_int = 1.e+11; // types determined from print_queries.x
        int n = 0;

	std::vector<float> AircraftAltitudeArray;
	AircraftAltitudeArray.reserve(map.at(getExportKey(ConfKeys::Latitude))->size());
        for (unsigned int idx = 0; idx < map.at(getExportKey(ConfKeys::Latitude))->size(); idx++)
        {
            //float latitude = static_cast<int>(map.at(getExportKey(ConfKeys::Latitude))->getAsFloat(idx));
            float latitude = static_cast<float>(map.at(getExportKey(ConfKeys::Latitude))->getAsFloat(idx));
            std::cout << "lat: " << latitude << std::endl;
            float acftalt = FLT_MAX; 
            if (latitude != missing_int)
            //if (latitude != FLT_MAX)
            {
                float pressures = FLT_MAX;
                float aircraftIndicatedAltitudes = FLT_MAX;
                float pressureAltitudeRelativeToMeanSeaLevels = FLT_MAX;
                float flightLevels = FLT_MAX;
                float heights = FLT_MAX;
                float heightOrAltitudes = FLT_MAX;
                float flightLevelSTs = FLT_MAX;
                if (!pressureQuery_.empty()) 
                {
                    pressures = static_cast<float>(map.at(getExportKey(ConfKeys::Pressure))->getAsFloat(idx));
                    if (pressures != UINT_MAX)
                    {
                        float ht_from_p = FLT_MAX;
                        if (pressures < 22630)
                        {
                            ht_from_p = 11000 - (std::log1p(pressures/22630)/0.0001576106);
                        } else {
                            ht_from_p = (1.0-pow((pressures/101325),(1.0/5.256)))*(288.15/0.0065);
                        }
                        acftalt = ht_from_p;
                    } else if (!aircraftIndicatedAltitudeQuery_.empty())
                    {
                        aircraftIndicatedAltitudes = static_cast<float>(map.at(getExportKey(ConfKeys::AircraftIndicatedAltitude))->getAsFloat(idx));
                        if (aircraftIndicatedAltitudes != UINT_MAX)
                        {
                            acftalt = aircraftIndicatedAltitudes;
                        }
                    } else {
                        acftalt = FLT_MAX;
                    }
                }
                if (!pressureAltitudeRelativeToMeanSeaLevelQuery_.empty())
                {
                    pressureAltitudeRelativeToMeanSeaLevels = static_cast<float>(map.at(getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))->getAsFloat(idx));
                    if (pressureAltitudeRelativeToMeanSeaLevels != FLT_MAX)
                    {
                        acftalt = pressureAltitudeRelativeToMeanSeaLevels;
                    }
                }
                if (!flightLevelQuery_.empty())
                {
                    flightLevels = static_cast<float>(map.at(getExportKey(ConfKeys::FlightLevel))->getAsFloat(idx));
                    if (flightLevels != FLT_MAX)
                    {
                        acftalt = flightLevels;
                    }
                }

                if (!heightQuery_.empty())
                {
                    heights = static_cast<float>(map.at(getExportKey(ConfKeys::Height))->getAsFloat(idx));
                    if (heights != static_cast<float>(INT_MAX))
                    {
                        acftalt = heights;
                    }
                }

                if (!heightOrAltitudeQuery_.empty())
                {
                    heightOrAltitudes = static_cast<float>(map.at(getExportKey(ConfKeys::HeightOrAltitude))->getAsFloat(idx));
                    if (heightOrAltitudes != static_cast<float>(INT_MAX))
                    {
                        acftalt = heightOrAltitudes;
                    }
                }

                if (!flightLevelSTQuery_.empty())
                {
                    flightLevelSTs = static_cast<float>(map.at(getExportKey(ConfKeys::FlightLevelST))->getAsFloat(idx));
                    if (flightLevelSTs != static_cast<float>(INT_MAX))
                    {
                        acftalt = flightLevelSTs;
                    }

                }
            n = n + 1;
	    AircraftAltitudeArray.push_back(acftalt);
            }
        std::cout << "n is" << n << std::endl;
        } 
        Dimensions dims = {static_cast<int>(AircraftAltitudeArray.size())};
        return std::make_shared<DataObject<float>>(
                AircraftAltitudeArray, 
                getExportName(),
                groupByField_,
                dims,
                map.at(getExportKey(ConfKeys::Latitude))->getPath(),
                map.at(getExportKey(ConfKeys::Latitude))->getDimPaths());
    }

    void AircraftAltitudeVariable::checkKeys(const BufrDataMap& map)
    {

        std::vector<std::string> requiredKeys  = {getExportKey(ConfKeys::Latitude)};

        if (!pressureQuery_.empty())
        {
            requiredKeys.push_back(getExportKey(ConfKeys::Pressure));
        }

        if (!aircraftIndicatedAltitudeQuery_.empty())
        {
            requiredKeys.push_back(getExportKey(ConfKeys::AircraftIndicatedAltitude));
        }

        if (!pressureAltitudeRelativeToMeanSeaLevelQuery_.empty())
        {
            requiredKeys.push_back(getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel));
        }

        if (!flightLevelQuery_.empty())
        {
            requiredKeys.push_back(getExportKey(ConfKeys::FlightLevel));
        }

        if (!heightQuery_.empty())
        {
            requiredKeys.push_back(getExportKey(ConfKeys::Height));
        }

        if (!heightOrAltitudeQuery_.empty())
        {
            requiredKeys.push_back(getExportKey(ConfKeys::HeightOrAltitude));
        }

        if (!flightLevelSTQuery_.empty())
        {
            requiredKeys.push_back(getExportKey(ConfKeys::FlightLevelST));
        }


        std::stringstream errStr;
        errStr << "Query ";

        bool isKeyMissing = false;
        for (const auto& key : requiredKeys)
        {
            if (map.find(key) == map.end())
            {
                isKeyMissing = true;
                errStr << key;
                break;
            }
        }

        errStr << " could not be found during export of AircraftAltitude object.";

        if (isKeyMissing)
        {
            throw eckit::BadParameter(errStr.str());
        }
    }

    QueryList AircraftAltitudeVariable::makeQueryList() const
    {
        auto queries = QueryList();

        {
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Latitude);
            info.query = latitudeQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!pressureQuery_.empty())  // pressure (mnemonic PRLC)
        {
            std::cout << "PRLC Query not empty" << std::endl;
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Pressure);
            info.query = pressureQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!aircraftIndicatedAltitudeQuery_.empty())  // aircraftIndicateAltitude (IALT)
        {
            std::cout << "IALR Query not empty" << std::endl;
            QueryInfo info;
            info.name = getExportKey(ConfKeys::AircraftIndicatedAltitude);
            info.query = aircraftIndicatedAltitudeQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!pressureAltitudeRelativeToMeanSeaLevelQuery_.empty())  // (mnemonic PSAL)
        {
            std::cout << "PSAL Query not empty" << std::endl;
            QueryInfo info;
            info.name = getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel);
            info.query = pressureAltitudeRelativeToMeanSeaLevelQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!flightLevelQuery_.empty())  // flightLevel (FLVL) 
        {
            std::cout << "FLVL Query not empty" << std::endl;
            QueryInfo info;
            info.name = getExportKey(ConfKeys::FlightLevel);
            info.query = flightLevelQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!heightQuery_.empty())  // height (mnemonic HEIT)
        {
            std::cout << "HEIT Query not empty" << std::endl;
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Height);
            info.query = heightQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!heightOrAltitudeQuery_.empty())  // heightOrAltitude (mnemonic HMSL)
        {
            std::cout << "HMSL Query not empty" << std::endl;
            QueryInfo info;
            info.name = getExportKey(ConfKeys::HeightOrAltitude);
            info.query = heightOrAltitudeQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!flightLevelSTQuery_.empty())  // flightLevelST (mnemonic FLVLST)
        {
            std::cout << "FLVLST Query not empty" << std::endl;
            QueryInfo info;
            info.name = getExportKey(ConfKeys::FlightLevelST);
            info.query = flightLevelSTQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        return queries;
    }

    std::string AircraftAltitudeVariable::getExportKey(const char* name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
