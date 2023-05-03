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
#include "SensorScanAngleVariable.h"


namespace
{
    namespace ConfKeys
    {
        const char* FieldOfViewNumber = "fieldOfViewNumber";
        const char* ScanStart = "scanStart";
        const char* ScanStep = "scanStep";
    }  // namespace ConfKeys

    const std::vector<std::string> FieldNames = {ConfKeys::FieldOfViewNumber,
                                                };
}  // namespace


namespace Ingester
{
    SensorScanAngleVariable::SensorScanAngleVariable(const std::string& exportName,
                                                     const std::string& groupByField,
                                                     const eckit::LocalConfiguration &conf) :
      Variable(exportName, groupByField, conf)
    {
        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> SensorScanAngleVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        // Get input parameters for sensor scan angle calculation
        float start;
        float step;
        if (conf_.has(ConfKeys::ScanStart) && conf_.has(ConfKeys::ScanStep))
        {
             start = conf_.getFloat(ConfKeys::ScanStart);
             step = conf_.getFloat(ConfKeys::ScanStep);
        }
        else
        {
            throw eckit::BadParameter("Missing required parameters: scan starting angle and step "
                                      "Check your configuration.");
        }

        // Read the variables from the map

        auto& fovnObj = map.at(getExportKey(ConfKeys::FieldOfViewNumber));

        // Declare and initialize scanline array
        // scanline has the same dimension as fovn
        std::vector<float> scanang(fovnObj->size(), DataObject<float>::missingValue());

        // Get field-of-view number
        std::vector<int> fovn(fovnObj->size(), DataObject<int>::missingValue());
        for (size_t idx = 0; idx < fovnObj->size(); idx++)
        {
           fovn[idx] = fovnObj->getAsInt(idx);
        }

        // Calculate sensor scan angle
        for (size_t idx = 0; idx < fovnObj->size(); idx++)
        {
           scanang[idx] = start + static_cast<float>(fovn[idx]-1) * step;
        }

        // Export sensor scan angle (view angle)
        return std::make_shared<DataObject<float>>(scanang,
                                                   getExportName(),
                                                   groupByField_,
                                                   fovnObj->getDims(),
                                                   fovnObj->getPath(),
                                                   fovnObj->getDimPaths());
    }

    void SensorScanAngleVariable::checkKeys(const BufrDataMap& map)
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

        errStr << " could not be found during export of scanang object.";

        if (isKeyMissing)
        {
            throw eckit::BadParameter(errStr.str());
        }
    }

    QueryList SensorScanAngleVariable::makeQueryList() const
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

    std::string SensorScanAngleVariable::getExportKey(const std::string& name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
