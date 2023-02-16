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
        const char* HeightOfStation = "heightOfStation";
    }  // namespace ConfKeys

    const std::vector<std::string> FieldNames = {ConfKeys::FieldOfViewNumber,
                                                 ConfKeys::HeightOfStation,
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
        // Diagnostic output (will remove it later)
        oops::Log::info() << "SensorScanAngleVariable ..." << std::endl;
        oops::Log::info() << "emily checking exportName   = " << exportName << std::endl;
        oops::Log::info() << "emily checking groupByField = " << groupByField << std::endl;
        oops::Log::info() << "emily checking conf         = " << conf << std::endl;
    }

    std::shared_ptr<DataObjectBase> SensorScanAngleVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        // Read the variables from the map
        auto& satghtObj = map.at(getExportKey(ConfKeys::HeightOfStation));
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
        float start = -52.725;
        float step = 1.110; 

        for (size_t idx = 0; idx < fovnObj->size(); idx++)
        {
           scanang[idx] = start + float(fovn[idx]-1) * step;
        } 

        // Diagnostic output (will remove it later)
        for (size_t idx = 0; idx < fovnObj->size(); idx++)
        {
            oops::Log::info()  << std::setw(10) << "idx     " << std::setw(10) << idx   
                               << std::setw(10) << "fovn    " << std::setw(10) << fovn[idx]   
                               << std::setw(10) << "scanang " << std::setw(10) << scanang[idx]   
                               << std::endl;   
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
