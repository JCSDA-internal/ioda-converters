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
//      const char* ObsTime = "obsTime";
    }  // namespace ConfKeys

    const std::vector<std::string> FieldNames = {ConfKeys::FieldOfViewNumber,
                                                 ConfKeys::SensorChannelNumber,
                                                 ConfKeys::BrightnessTemperature,
//                                               ConfKeys::ObsTime
                                                };
}  // namespace


namespace Ingester
{
    RemappedBrightnessTemperatureVariable::RemappedBrightnessTemperatureVariable(const std::string& exportName,
                                                       const std::string& groupByField,
                                                       const eckit::LocalConfiguration &conf) :
      Variable(exportName, groupByField, conf), 
      datetime_(exportName, groupByField, conf_.getSubConfiguration("obsTime"))
    {
        initQueryMap();
        // Diagnostic output (will remove it later)
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
        auto& fovnumObj = map.at(getExportKey(ConfKeys::FieldOfViewNumber));

        // Get dimensions
        int nchns = (radObj->getDims())[1];
        int dim1 = (radObj->getDims())[1];
        int dim0 = (radObj->getDims())[0];
        int nobs = (fovnumObj->getDims())[0];

        // Diagnostic output (will remove it later)
        oops::Log::info() << "emily checking nchs = " << nchns << std::endl;
        oops::Log::info() << "emily checking nobs = " << nobs  << std::endl;
        oops::Log::info() << "emily checking dim0 = " << dim0 << std::endl;
        oops::Log::info() << "emily checking dim1 = " << dim1  << std::endl;
        oops::Log::info() << "emily checking radObj size  = "       << radObj->size()  << std::endl;
        oops::Log::info() << "emily checking fovnumObj size = "     << fovnumObj->size() << std::endl;
        oops::Log::info() << "emily checking sensorChanObj size = " << sensorChanObj->size() << std::endl;

        // Declare and initialize scanline arrays 
        std::vector<int> scanline(fovnumObj->size(), DataObject<int>::missingValue()); // scanline has the same dimension as fovn

        // Get observation time (obstime) variable
        auto datetimeObj = datetime_.exportData(map);
        std::vector<int64_t> obstime;
        if (auto obstimeObj = std::dynamic_pointer_cast<DataObject<int64_t>>(datetimeObj))
        {
           obstime = obstimeObj->getRawData();
        } 

        // Get field-of-view number
        std::vector<int> fovn;
        if (auto fovnObj = std::dynamic_pointer_cast<DataObject<int>>(fovnumObj))
        {
           fovn = fovnObj->getRawData();
        } 

        // Get sensor channel
        std::vector<int> channel;
        if (auto channelObj = std::dynamic_pointer_cast<DataObject<int>>(sensorChanObj))
        {
           channel = channelObj->getRawData();
        } 

        // Get brightness temperature (observation)
        std::vector<float> btobs;
        if (auto btobsObj = std::dynamic_pointer_cast<DataObject<float>>(radObj))
        {
           btobs = btobsObj->getRawData();
        } 

        // Diagnostic output (will remove it later)
//        for (size_t idx = 0; idx < radObj->size(); idx++)
//        {
//            size_t iloc = static_cast<size_t>(floor(idx / nchns));
//            size_t ichn = static_cast<size_t>(floor(idx % nchns));
//            oops::Log::info()  << std::setw(10) << "idx     " << std::setw(10) << idx   
//                               << std::setw(10) << "iloc    " << std::setw(10) << iloc
//                               << std::setw(10) << "fovn    " << std::setw(10) << fovn[iloc]   
//                               << std::setw(10) << "obstime " << std::setw(20) << obstime[iloc]   
//                               << std::setw(10) << "channel " << std::setw(10) << channel[idx]
//                               << std::setw(10) << "btobs   " << std::setw(10) << btobs[idx]   
//                               << std::endl;   
//        }

        // Perform FFT image remapping 
        // input only variables: nobs, nchns obstime, fovn, channel
        // input & output variables: btobs, scanline, error_status
        int error_status; 
        ATMS_Spatial_Average_f(nobs, nchns, &obstime, &fovn, &channel, &btobs, &scanline, &error_status);
      
        // Export remapped observation (btobs)
        return std::make_shared<DataObject<float>>(btobs,
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

        auto datetimequerys = datetime_.makeQueryList();
        queries.insert(queries.end(), datetimequerys.begin(), datetimequerys.end());

//        for (const auto& fieldName : DatetimeFieldNames)
//        {
//            if (conf_.has(fieldName))
//            {
//                QueryInfo info;
//                info.name = getExportKey(fieldName);
//                info.query = conf_.getString(fieldName);
//                queries.push_back(info);
//            }
//        }

        return queries;
    }

    std::string RemappedBrightnessTemperatureVariable::getExportKey(const std::string& name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
