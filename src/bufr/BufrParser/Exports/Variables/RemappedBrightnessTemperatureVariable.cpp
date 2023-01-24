/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <cmath>
#include <iomanip>
#include <memory>
#include <ostream>
#include <time.h>
#include <unordered_map>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "oops/util/Logger.h"

#include "DataObject.h"
#include "DatetimeVariable.h"
#include "RemappedBrightnessTemperatureVariable.h"
#include "Transforms/atms/atms_spatial_average_interface.h"


namespace
{
    namespace ConfKeys
    {
        const char* FieldOfViewNumber = "fieldOfViewNumber";
        const char* SensorChannelNumber = "sensorChannelNumber";
        const char* BrightnessTemperature = "brightnessTemperature";
        const char* Year = "year";
        const char* Month = "month";
        const char* Day = "day";
        const char* Hour = "hour";
        const char* Minute = "minute";
        const char* Second = "second";
        const char* HoursFromUtc = "hoursFromUtc";
    }  // namespace ConfKeys

    const std::vector<std::string> FieldNames = {ConfKeys::FieldOfViewNumber,
                                                 ConfKeys::SensorChannelNumber,
                                                 ConfKeys::BrightnessTemperature,
                                                 ConfKeys::Year,
                                                 ConfKeys::Month,
                                                 ConfKeys::Day,
                                                 ConfKeys::Hour,
                                                 ConfKeys::Minute,
                                                 ConfKeys::Second};
}  // namespace


namespace Ingester
{
    RemappedBrightnessTemperatureVariable::RemappedBrightnessTemperatureVariable(const std::string& exportName,
                                                       const std::string& groupByField,
                                                       const eckit::LocalConfiguration &conf) :
      Variable(exportName, groupByField, conf)
    {
        initQueryMap();
        oops::Log::info() << "RemappedBrightnessTemperatureVariable ..." << std::endl;
        oops::Log::info() << "emily checking exportName   = " << exportName << std::endl;
        oops::Log::info() << "emily checking groupByField = " << groupByField << std::endl;
        oops::Log::info() << "emily checking conf         = " << conf << std::endl;
    }

    std::shared_ptr<DataObjectBase> RemappedBrightnessTemperatureVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        // Read the variables from the map
        auto& radObj = map.at(getExportKey(ConfKeys::BrightnessTemperature));
        auto& sensorChanObj = map.at(getExportKey(ConfKeys::SensorChannelNumber));
        auto& fovnObj = map.at(getExportKey(ConfKeys::FieldOfViewNumber));

        // Declare and initialize data arrays 
        std::vector<float> outData((radObj->size()), DataObject<float>::missingValue());
        std::vector<int> fovn((fovnObj->size()), DataObject<int>::missingValue());
        std::vector<int> scanline(fovnObj->size(), DataObject<int>::missingValue()); // scanline has the same dimension as fovn
        std::vector<float> btobs((radObj->size()), DataObject<float>::missingValue());
        std::vector<int> channel((sensorChanObj->size()), DataObject<int>::missingValue());
        std::vector<int64_t> obstime((fovnObj->size()), DataObject<int64_t>::missingValue());

        // Get dimensions
        int nchns = (radObj->getDims())[1];
        int dim1 = (radObj->getDims())[1];
        int dim0 = (radObj->getDims())[0];
        int nobs = (fovnObj->getDims())[0];

        oops::Log::info() << "emily checking nchs = " << nchns << std::endl;
        oops::Log::info() << "emily checking nobs = " << nobs  << std::endl;
        oops::Log::info() << "emily checking dim0 = " << dim0 << std::endl;
        oops::Log::info() << "emily checking dim1 = " << dim1  << std::endl;
        oops::Log::info() << "emily checking radObj size  = "       << radObj->size()  << std::endl;
        oops::Log::info() << "emily checking fovnObj size = "       << fovnObj->size() << std::endl;
        oops::Log::info() << "emily checking sensorChanObj size = " << sensorChanObj->size() << std::endl;

        // Get observation time (obstime) variable
        oops::Log::info() << "emily checking conf.getSubConfiguration = " << conf_.getSubConfiguration("obstime") << std::endl;
        DatetimeVariable("obstime", "", conf_.getSubConfiguration("obstime"));

        // Fill in fov values from map  
        for (size_t idx = 0; idx < fovnObj->size(); idx++)
        {
            fovn[idx] =  fovnObj->getAsInt(idx);
        }

        // Fill in obs values from map  
        for (size_t idx = 0; idx < radObj->size(); idx++)
        {
            size_t iloc = static_cast<size_t>(floor(idx / nchns));
            size_t ichn = static_cast<size_t>(floor(idx % nchns));
            btobs[idx] = radObj->getAsFloat(idx);
            channel[idx] = sensorChanObj->getAsInt(idx);
//          oops::Log::info()  << std::setw(10) << "idx     " << std::setw(10) << idx   
//                             << std::setw(10) << "iloc    " << std::setw(10) << iloc
//                             << std::setw(10) << "fovn    " << std::setw(10) << fovn[iloc]   
//                             << std::setw(10) << "channel " << std::setw(10) << channel[idx]
//                             << std::setw(10) << "btobs   " << std::setw(10) << btobs[idx]   
//                             << std::endl;   
        }

        int error_status; 
//      int scanline[nobs];

//      ATMS_Spatial_Average_f(nobs, nchns, &fovnObj, &radObj, &scanline, &error_status); 
        ATMS_Spatial_Average_f(nobs, nchns, &fovn, &channel, &btobs, &scanline, &error_status);
       
        // Perform FFT image remapping 
        for (size_t idx = 0; idx < radObj->size(); idx++)
        {
            auto channel = sensorChanObj->getAsInt(idx);

            if (!radObj->isMissing(idx))
            {
                outData[idx] = radObj->getAsFloat(idx);
            }
        }

        return std::make_shared<DataObject<float>>(outData,
                                                   getExportName(),
                                                   groupByField_,
                                                   radObj->getDims(),
                                                   radObj->getPath(),
                                                   radObj->getDimPaths());
    }

    void RemappedBrightnessTemperatureVariable::checkKeys(const BufrDataMap& map)
    {
        std::vector<std::string> requiredKeys;
        for (const auto& fieldName : FieldNames)
        {
            if (conf_.has(fieldName))
            {
                requiredKeys.push_back(getExportKey(fieldName));
            }
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

        errStr << " could not be found during export of datetime object.";

        if (isKeyMissing)
        {
            throw eckit::BadParameter(errStr.str());
        }
    }

    QueryList RemappedBrightnessTemperatureVariable::makeQueryList() const
    {
        auto queries = QueryList();

        for (const auto& fieldName : FieldNames)
        {
            if (conf_.has(fieldName))
            {
                QueryInfo info;
                info.name = getExportKey(fieldName);
                info.query = conf_.getString(fieldName);
                info.groupByField = groupByField_;
                queries.push_back(info);
            }
        }

        return queries;
    }

    std::string RemappedBrightnessTemperatureVariable::getExportKey(const std::string& name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
