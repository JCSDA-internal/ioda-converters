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
#include "SensorScanPositionVariable.h"


namespace
{
    namespace ConfKeys
    {
        const char* FieldOfViewNumber = "fieldOfViewNumber";
        const char* Sensor = "sensor";
    }  // namespace ConfKeys

    const std::vector<std::string> FieldNames = {ConfKeys::FieldOfViewNumber,
                                                };
}  // namespace


namespace Ingester
{
    SensorScanPositionVariable::SensorScanPositionVariable(const std::string& exportName,
                                                     const std::string& groupByField,
                                                     const eckit::LocalConfiguration &conf) :
      Variable(exportName, groupByField, conf)
    {
        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> SensorScanPositionVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        // Get input parameters for sensor scan position calculation
        std::string sensor;
        if (conf_.has(ConfKeys::Sensor) )
        {
             sensor = conf_.getString(ConfKeys::Sensor);
        }
        else
        {
            throw eckit::BadParameter("Missing required parameters: sensor"
                                      "Check your configuration.");
        }

        // Read the variables from the map
        auto& fovnObj = map.at(getExportKey(ConfKeys::FieldOfViewNumber));

        // Declare and initialize scanline array
        // scanline has the same dimension as fovn
        std::vector<int> scanpos(fovnObj->size(), DataObject<int>::missingValue());

        // Get field-of-view number
        std::vector<int> fovn(fovnObj->size(), DataObject<int>::missingValue());
        if (sensor == "iasi")
        {
           for (size_t idx = 0; idx < fovnObj->size(); idx++)
           {
              scanpos[idx] = (fovnObj->getAsInt(idx) - 1) / 2 + 1;
           }
        }
        else
        {
           for (size_t idx = 0; idx < fovnObj->size(); idx++)
           {
              scanpos[idx] = fovnObj->getAsInt(idx);
           }
        }

        // Export sensor scan angle (view angle)
        return std::make_shared<DataObject<int>>(scanpos,
                                                   getExportName(),
                                                   groupByField_,
                                                   fovnObj->getDims(),
                                                   fovnObj->getPath(),
                                                   fovnObj->getDimPaths());
    }

    void SensorScanPositionVariable::checkKeys(const BufrDataMap& map)
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

        errStr << " could not be found during export of scanpos object.";

        if (isKeyMissing)
        {
            throw eckit::BadParameter(errStr.str());
        }
    }

    QueryList SensorScanPositionVariable::makeQueryList() const
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

    std::string SensorScanPositionVariable::getExportKey(const std::string& name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
