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
        const char* ObsTime = "obsTime";
    }  // namespace ConfKeys

    const std::vector<std::string> FieldNames = {ConfKeys::FieldOfViewNumber,
                                                 ConfKeys::SensorChannelNumber,
                                                 ConfKeys::BrightnessTemperature,
                                                };
}  // namespace


namespace Ingester
{
    RemappedBrightnessTemperatureVariable::RemappedBrightnessTemperatureVariable(
                                                       const std::string& exportName,
                                                       const std::string& groupByField,
                                                       const eckit::LocalConfiguration &conf) :
      Variable(exportName, groupByField, conf),
      datetime_(exportName, groupByField, conf_.getSubConfiguration(ConfKeys::ObsTime))
    {
        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> RemappedBrightnessTemperatureVariable::exportData(
                                                       const BufrDataMap& map)
    {
        checkKeys(map);

        // Read the variables from the map
        auto& radObj = map.at(getExportKey(ConfKeys::BrightnessTemperature));
        auto& sensorChanObj = map.at(getExportKey(ConfKeys::SensorChannelNumber));
        auto& fovnObj = map.at(getExportKey(ConfKeys::FieldOfViewNumber));

        // Get dimensions
        if (radObj->getDims().size() != 2)
        {
           oops::Log::info()  << "Observartion dimension shoule be 2 " << std::endl;
           oops::Log::error() << "Incorrect observartion dimension : " << radObj->getDims().size()
                                                                       << std::endl;
        }
        int nobs = (radObj->getDims())[0];
        int nchn = (radObj->getDims())[1];

        oops::Log::info()  << "RemappedBrightnessTemperatureVariable: nobs =  " << nobs << std::endl;
        oops::Log::info()  << "RemappedBrightnessTemperatureVariable: nchn =  " << nchn << std::endl;

        // Declare and initialize scanline array
        // scanline has the same dimension as fovn
        std::vector<int> scanline(fovnObj->size(), DataObject<int>::missingValue());

        // Get observation time (obstime) variable
        auto datetimeObj = datetime_.exportData(map);
        std::vector<int64_t> obstime;
        obstime = std::dynamic_pointer_cast<DataObject<int64_t>>(datetimeObj)->getRawData();

        // Get field-of-view number
        std::vector<int> fovn(fovnObj->size(), DataObject<int>::missingValue());
        for (size_t idx = 0; idx < fovnObj->size(); idx++)
        {
           fovn[idx] = fovnObj->getAsInt(idx);
        }

        // Get sensor channel
        std::vector<int> channel(sensorChanObj->size(), DataObject<int>::missingValue());
        for (size_t idx = 0; idx < sensorChanObj->size(); idx++)
        {
           channel[idx] = sensorChanObj->getAsInt(idx);
        }

        // Get brightness temperature (observation)
        std::vector<float> btobs(radObj->size(), DataObject<float>::missingValue());
        for (size_t idx = 0; idx < radObj->size(); idx++)
        {
           btobs[idx] = radObj->getAsFloat(idx);
        }

        // Perform FFT image remapping
        // input only variables: nobs, nchn obstime, fovn, channel
        // input & output variables: btobs, scanline, error_status
        if (nobs > 0) {
            int error_status;
            ATMS_Spatial_Average_f(nobs, nchn, &obstime, &fovn, &channel, &btobs,
                                               &scanline, &error_status);
        }

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

        return queries;
    }

    std::string RemappedBrightnessTemperatureVariable::getExportKey(const std::string& name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
