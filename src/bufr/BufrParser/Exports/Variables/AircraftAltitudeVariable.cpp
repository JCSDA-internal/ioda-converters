/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <climits>
#include <iostream>
#include <iomanip>
#include <string> 
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
            pressureQuery_ = conf.getString(ConfKeys::Pressure);
        }

        if (conf.has(ConfKeys::AircraftIndicatedAltitude))
        {            
            aircraftIndicatedAltitudeQuery_ = conf.getString(ConfKeys::AircraftIndicatedAltitude);
        }

        if (conf.has(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))
        {
            pressureAltitudeRelativeToMeanSeaLevelQuery_ = conf.getString(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel);
        }

        if (conf.has(ConfKeys::FlightLevel))
        {
            flightLevelQuery_ = conf.getString(ConfKeys::FlightLevel);
        }

        if (conf.has(ConfKeys::Height))
        {
            heightQuery_ = conf.getString(ConfKeys::Height);
        }

        if (conf.has(ConfKeys::HeightOrAltitude))
        {
            heightOrAltitudeQuery_ = conf.getString(ConfKeys::HeightOrAltitude);
        }

        if (conf.has(ConfKeys::FlightLevelST))
        {
            flightLevelSTQuery_ = conf.getString(ConfKeys::FlightLevelST);
        }

        if (conf.has(ConfKeys::GroupByField))
        {
            groupByField_ = conf.getString(ConfKeys::GroupByField);
        }

        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> AircraftAltitudeVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);
//        static const float missing_int = 1.e+11;
//        static const float missing_int = DataObject<int>::missingValue();

        std::vector<float> aircraftaltitudearray;
        aircraftaltitudearray.reserve(map.at(getExportKey(ConfKeys::Latitude))->size());
        unsigned int datalength = map.at(getExportKey(ConfKeys::Latitude))->size();
//        auto prestype =  map.at(getExportKey(ConfKeys::Latitude))->typeInfo();
//        auto prestype =  map.at(getExportKey(ConfKeys::Latitude))->typeInfo;
//        std::cout << "prestype is " << prestype << std::endl;
        for (unsigned int idx = 0; idx < datalength; idx++)
        {
//            float latitude = map.at(getExportKey(ConfKeys::Latitude))->getAsFloat(idx);
            float acftalt = FLT_MAX; 
//            if(latitude != DataObject<float>::missingValue())
//            {
/*                float pressures = FLT_MAX;
                float aircraftIndicatedAltitudes = FLT_MAX;
                float pressureAltitudeRelativeToMeanSeaLevels = FLT_MAX;
                float flightLevels = FLT_MAX;
                float heights = FLT_MAX;
                float heightOrAltitudes = FLT_MAX;
                float flightLevelSTs = FLT_MAX; */
                if (!pressureQuery_.empty()) 
                {
                      auto prestype = std::shared_pt<map.at(getExportKey(ConfKeys::Pressure))> objectByTypeInfo(TypeInfo& info) const;
//                    auto prestype = map.at(getExportKey(ConfKeys::Pressure)).target->typeInfo;
//                    auto prestype = map.at(getExportKey(ConfKeys::Pressure)).target->typeInfo;
                    float pressures = map.at(getExportKey(ConfKeys::Pressure))->getAsFloat(idx);
                     
                    if (pressures != INT_MAX) 
//                    if (pressures != ResultSet<pressureQuery_.typeInfo>::missingValue())
//                    if (pressures != ResultSet<pressureQuery_.typeInfo>::missingValue())
                    {
//                      If pressure exists, derive height from US standard atmosphere
                        if (pressures < 22630)
                        {
                            acftalt = 11000 - (std::log1p(pressures/22630)/0.0001576106);
                        } else {
                            acftalt = (1.0-pow((pressures/101325),(1.0/5.256)))*(288.15/0.0065);
                        }
                    } else if (!aircraftIndicatedAltitudeQuery_.empty())
                    {
                        float aircraftIndicatedAltitudes = map.at(getExportKey(ConfKeys::AircraftIndicatedAltitude))->getAsFloat(idx);
                        if (aircraftIndicatedAltitudes != DataObject<unsigned int>::missingValue())
                        // If aircraftIndicatedAltitudes exists (it should if PRLC doesn't), set as the aircraftAltitude
                        {
                            acftalt = aircraftIndicatedAltitudes;
                        }
                    } //else {
                    //    acftalt = FLT_MAX;
                    //}
                }
                if (!pressureAltitudeRelativeToMeanSeaLevelQuery_.empty())
                {
                    float pressureAltitudeRelativeToMeanSeaLevels = map.at(getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))->getAsFloat(idx);
                    if (pressureAltitudeRelativeToMeanSeaLevels != DataObject<float>::missingValue())
                    {
                        acftalt = pressureAltitudeRelativeToMeanSeaLevels;
                    }
                } else if (!flightLevelQuery_.empty())
                {
                    float flightLevels = map.at(getExportKey(ConfKeys::FlightLevel))->getAsFloat(idx);
                    if (flightLevels != DataObject<float>::missingValue())
                    {
                        acftalt = flightLevels;
                    }
                } else if (!heightQuery_.empty())
                {
                    float heights = map.at(getExportKey(ConfKeys::Height))->getAsFloat(idx);
                    if (heights != DataObject<float>::missingValue())
                    {
                        acftalt = heights;
                    }
                } else if (!heightOrAltitudeQuery_.empty())
                {
                    float heightOrAltitudes = map.at(getExportKey(ConfKeys::HeightOrAltitude))->getAsFloat(idx);
                    if (heightOrAltitudes != DataObject<float>::missingValue())
                    {
                        acftalt = heightOrAltitudes;
                    }
                } else if (!flightLevelSTQuery_.empty())
                {
                    float flightLevelSTs = map.at(getExportKey(ConfKeys::FlightLevelST))->getAsFloat(idx);
                    if (flightLevelSTs != DataObject<float>::missingValue())
                    {
                        acftalt = flightLevelSTs;
                    }

                }
            aircraftaltitudearray.push_back(acftalt);
//            }
        } 
        Dimensions dims = {static_cast<int>(aircraftaltitudearray.size())};
        return std::make_shared<DataObject<float>>(
                aircraftaltitudearray, 
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
//            info.type = getExportKey(ConfKeys::Latitude).typeInfo; 
            queries.push_back(info);
        }

        if (!pressureQuery_.empty())  // pressure (mnemonic PRLC)
        {
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Pressure);
            info.query = pressureQuery_;
            info.groupByField = groupByField_;
//            info.type = getExportKey(ConfKeys::Pressure).typeInfo;
            queries.push_back(info);
        }

        if (!aircraftIndicatedAltitudeQuery_.empty())  // aircraftIndicateAltitude (IALT)
        {
            QueryInfo info;
            info.name = getExportKey(ConfKeys::AircraftIndicatedAltitude);
            info.query = aircraftIndicatedAltitudeQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!pressureAltitudeRelativeToMeanSeaLevelQuery_.empty())  // (mnemonic PSAL)
        {
            QueryInfo info;
            info.name = getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel);
            info.query = pressureAltitudeRelativeToMeanSeaLevelQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!flightLevelQuery_.empty())  // flightLevel (FLVL) 
        {
            QueryInfo info;
            info.name = getExportKey(ConfKeys::FlightLevel);
            info.query = flightLevelQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!heightQuery_.empty())  // height (mnemonic HEIT)
        {
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Height);
            info.query = heightQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!heightOrAltitudeQuery_.empty())  // heightOrAltitude (mnemonic HMSL)
        {
            QueryInfo info;
            info.name = getExportKey(ConfKeys::HeightOrAltitude);
            info.query = heightOrAltitudeQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!flightLevelSTQuery_.empty())  // flightLevelST (mnemonic FLVLST)
        {
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
