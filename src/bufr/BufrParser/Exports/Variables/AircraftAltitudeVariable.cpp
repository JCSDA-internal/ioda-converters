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
//            unsigned int datalength = getExportKey(ConfKeys::Pressure).size();
        }

        if (conf.has(ConfKeys::AircraftIndicatedAltitude))
        {            
            aircraftIndicatedAltitudeQuery_ = conf.getString(ConfKeys::AircraftIndicatedAltitude);
//            unsigned int datalength = map.at(getExportKey(ConfKeys::AircraftIndicatedAltitude))->size();
        }

        if (conf.has(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))
        {
            pressureAltitudeRelativeToMeanSeaLevelQuery_ = conf.getString(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel);
//            unsigned int datalength = map.at(getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))->size();
        }

        if (conf.has(ConfKeys::FlightLevel))
        {
            flightLevelQuery_ = conf.getString(ConfKeys::FlightLevel);
//            unsigned int datalength = map.at(getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))->size();
        }

        if (conf.has(ConfKeys::Height))
        {
            heightQuery_ = conf.getString(ConfKeys::Height);
//            unsigned int datalength = map.at(getExportKey(ConfKeys::Height))->size();
        }

        if (conf.has(ConfKeys::HeightOrAltitude))
        {
            heightOrAltitudeQuery_ = conf.getString(ConfKeys::HeightOrAltitude);
//            unsigned int datalength = map.at(getExportKey(ConfKeys::HeightOrAltitude))->size();
        }

        if (conf.has(ConfKeys::FlightLevelST))
        {
            flightLevelSTQuery_ = conf.getString(ConfKeys::FlightLevelST);
//            unsigned int datalength = map.at(getExportKey(ConfKeys::FlightLevelST))->size();
        }

        if (conf.has(ConfKeys::GroupByField))
        {
            groupByField_ = conf.getString(ConfKeys::GroupByField);
        }
//        unsigned int datalength = map.at(initQueryMap().front())->size();

        initQueryMap();
//        unsigned int datalength = map.at(initQueryMap().front())->size();

    }
//    unsigned int datalength = map.at(initQueryMap().front())->size();
    std::shared_ptr<DataObjectBase> AircraftAltitudeVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);
//        static const float missing_int = 1.e+11;
//        static const float missing_int = DataObject<int>::missingValue();
//        unsigned int datalength = map.at(checkKeys(map).front())->size();
        std::vector<float> aircraftaltitudearray;
        unsigned int datalength = 0;
//        aircraftaltitudearray.reserve(map.at(getExportKey(ConfKeys::Latitude))->size());
//        unsigned int datalength = map.at(requiredKeys(map).front())->size();
//        unsigned int datalength = map.at(getExportKey(ConfKeys::Latitude))->size();
//        auto prestype =  map.at(getExportKey(ConfKeys::Latitude))->typeInfo();
//        auto prestype =  map.at(getExportKey(ConfKeys::Latitude))->typeInfo;
//        std::cout << "prestype is " << prestype << std::endl;
        if (!pressureQuery_.empty())
        { 
            aircraftaltitudearray.reserve(map.at(getExportKey(ConfKeys::Pressure))->size());
            datalength = map.at(getExportKey(ConfKeys::Pressure))->size();
        } else if (!aircraftIndicatedAltitudeQuery_.empty())
        { 
            aircraftaltitudearray.reserve(map.at(getExportKey(ConfKeys::AircraftIndicatedAltitude))->size());
            datalength = map.at(getExportKey(ConfKeys::Pressure))->size();
        } else if (!pressureAltitudeRelativeToMeanSeaLevelQuery_.empty())
        { 
            aircraftaltitudearray.reserve(map.at(getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))->size());
            datalength = map.at(getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))->size();
        } else if (!flightLevelQuery_.empty())
        { 
            aircraftaltitudearray.reserve(map.at(getExportKey(ConfKeys::FlightLevel))->size());
            datalength = map.at(getExportKey(ConfKeys::FlightLevel))->size();
        } else if (!heightQuery_.empty())
        { 
            aircraftaltitudearray.reserve(map.at(getExportKey(ConfKeys::Height))->size());
            datalength = map.at(getExportKey(ConfKeys::Height))->size();
        } else if (!heightOrAltitudeQuery_.empty())
        { 
            aircraftaltitudearray.reserve(map.at(getExportKey(ConfKeys::HeightOrAltitude))->size());
            datalength = map.at(getExportKey(ConfKeys::HeightOrAltitude))->size();
        } else if (!flightLevelSTQuery_.empty())
        { 
            aircraftaltitudearray.reserve(map.at(getExportKey(ConfKeys::FlightLevelST))->size());
            datalength = map.at(getExportKey(ConfKeys::FlightLevelST))->size();
        }

        for (unsigned int idx = 0; idx < datalength; idx++)
        {
//            float latitude = map.at(getExportKey(ConfKeys::Latitude))->getAsFloat(idx);
            float acftalt = DataObject<float>::missingValue();
//            if(latitude != DataObject<float>::missingValue())
//            {
/*                float PRLCdata = FLT_MAX;
                float IALTdata = FLT_MAX;
                float PSALdata = FLT_MAX;
                float FLVLdata = FLT_MAX;
                float HEITdata = FLT_MAX;
                float HMSLdata = FLT_MAX;
                float FLVLSTdata = FLT_MAX; */
                if (!pressureQuery_.empty()) 
                {
//                    auto prestype = map.at(getExportKey(ConfKeys::Pressure)).target->typeInfo;
//                    auto prestype = map.at(getExportKey(ConfKeys::Pressure)).target->typeInfo;
                    //float PRLCdata = map.at(getExportKey(ConfKeys::Pressure))->getAsFloat(idx);
                    float PRLCdata = map.at(getExportKey(ConfKeys::Pressure))->getAsFloat(idx);
                    bool PRLCdataMissing = map.at(getExportKey(ConfKeys::Pressure))->isMissing(idx); 
                    
                    if (PRLCdataMissing == false) 

                    //if (pressureQuery_.isMissing(idx)) 
                    {
//                      If pressure exists, derive height from US standard atmosphere
                        if (PRLCdata < 22630)
                        {
                            acftalt = 11000 - (std::log1p(PRLCdata/22630)/0.0001576106);
                        } else {
                            acftalt = (1.0-pow((PRLCdata/101325),(1.0/5.256)))*(288.15/0.0065);
                        }
                    } else if (!aircraftIndicatedAltitudeQuery_.empty())
                    {
                        float IALTdata = map.at(getExportKey(ConfKeys::AircraftIndicatedAltitude))->getAsFloat(idx);
                        bool IALTdataMissing = map.at(getExportKey(ConfKeys::AircraftIndicatedAltitude))->isMissing(idx);
                        if (IALTdataMissing == false)
                        // If IALTdata exists (it should if PRLC doesn't), set as the aircraftAltitude
                        {
                            acftalt = IALTdata;
                        }
                    } //else {
                    //    acftalt = FLT_MAX;
                    //}
                }
                if (!pressureAltitudeRelativeToMeanSeaLevelQuery_.empty())
                {
                    float PSALdata = map.at(getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))->getAsFloat(idx);
                    bool PSALdataMissing = map.at(getExportKey(ConfKeys::PressureAltitudeRelativeToMeanSeaLevel))->isMissing(idx);
                    if (PSALdataMissing == false)
                    {
                        acftalt = PSALdata;
                    }
                } 
                if (!flightLevelQuery_.empty())
                {
                    float FLVLdata = map.at(getExportKey(ConfKeys::FlightLevel))->getAsFloat(idx);
                    bool FLVLdataMissing = map.at(getExportKey(ConfKeys::FlightLevel))->isMissing(idx);
                    if (FLVLdataMissing == false) 
                    {
                        acftalt = FLVLdata;
                    }
                } 
                if (!heightQuery_.empty())
                {
                    float HEITdata = map.at(getExportKey(ConfKeys::Height))->getAsFloat(idx);
                    bool HEITdataMissing = map.at(getExportKey(ConfKeys::Height))->isMissing(idx);
                    if (HEITdataMissing == false)
                    {
                        acftalt = HEITdata;
                    }
                } 
                if (!heightOrAltitudeQuery_.empty())
                {
                    float HMSLdata = map.at(getExportKey(ConfKeys::HeightOrAltitude))->getAsFloat(idx);
                    bool HMSLdataMissing = map.at(getExportKey(ConfKeys::HeightOrAltitude))->isMissing(idx);
                    if (HMSLdataMissing == false) 
                    {
                        acftalt = HMSLdata;
                    }
                } 
                if (!flightLevelSTQuery_.empty())
                {
                    float FLVLSTdata = map.at(getExportKey(ConfKeys::FlightLevelST))->getAsFloat(idx);
                    bool FLVLSTdataMissing = map.at(getExportKey(ConfKeys::FlightLevelST))->isMissing(idx);
                    if (FLVLSTdataMissing == false)
                    {
                        acftalt = FLVLSTdata;
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

//        std::vector<std::string> requiredKeys  = {getExportKey(ConfKeys::Latitude)};
        std::vector<std::string> requiredKeys  = {};

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
//        unsigned int datalength = map.at(requiredKeys.front())->size();

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
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Pressure);
            info.query = pressureQuery_;
            info.groupByField = groupByField_;
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
