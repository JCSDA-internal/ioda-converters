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
        const char* Pressure = "pressure";
        const char* AircraftIndicatedAltitude = "aircraftIndicatedAltitude"; 
        const char* PressureAltitudeRelativeToMeanSeaLevel = "pressureAltitudeRelativeToMeanSeaLevel";
        const char* FlightLevel = "flightLevel";
        const char* Height = "height";
        const char* HeightOrAltitude = "heightOrAltitude";
        const char* FlightLevelST = "flightLevelST";
    }  // namespace ConfKeys

    const std::vector<std::string> FieldNames = {ConfKeys::Pressure,
                                                 ConfKeys::AircraftIndicatedAltitude,
                                                 ConfKeys::PressureAltitudeRelativeToMeanSeaLevel,
                                                 ConfKeys::FlightLevel,
                                                 ConfKeys::Height,
                                                 ConfKeys::HeightOrAltitude,
                                                 ConfKeys::FlightLevelST};
}  // namespace


namespace Ingester
{
    AircraftAltitudeVariable::AircraftAltitudeVariable(const std::string& exportName,
                                                       const std::string& groupByField,
                                                       const eckit::LocalConfiguration &conf) :
      Variable(exportName),
      groupByField_(groupByField),
      conf_(conf)
    {
        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> AircraftAltitudeVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        std::unordered_map<std::string, std::shared_ptr<DataObjectBase>> includedFields;

        std::shared_ptr<DataObjectBase> referenceObj = nullptr;
        for (const auto& fieldName : FieldNames)
        {
            if (conf_.has(fieldName))
            {
                includedFields.insert({fieldName, map.at(getExportKey(fieldName))});
            }
        }

        referenceObj = (*includedFields.begin()).second;

        // Validation: make sure the dimensions are consistent
        auto path = referenceObj->getPath();
        for (const auto& fieldName : FieldNames)
        {
            if (includedFields.find(fieldName) != includedFields.end() &&
                includedFields[fieldName]->getPath() != path)
            {
                std::ostringstream errStr;
                errStr << "Inconsistent dimensions found in source data.";
                throw eckit::BadParameter(errStr.str());
            }
        }

        std::vector<float> aircraftAlts;
        aircraftAlts.resize(referenceObj->size(), DataObject<float>::missingValue());

        for (size_t idx = 0; idx < referenceObj->size(); idx++)
        {
            for (const auto &field: includedFields)
            {
                if (field.first == ConfKeys::Pressure)
                {
                    if (!field.second->isMissing(idx))
                    {
                        auto value = field.second->getAsFloat(idx);
                        if (value < 22630)
                        {
                            aircraftAlts[idx]  = 11000 - (std::log1p(value/22630)/0.0001576106);
                        }
                        else
                        {
                            aircraftAlts[idx]  = (1.0-pow((value/101325),(1.0/5.256)))*(288.15/0.0065);
                        }
                    }
                    else if (includedFields.find(ConfKeys::AircraftIndicatedAltitude) != includedFields.end())
                    {
                        if (!includedFields[ConfKeys::AircraftIndicatedAltitude]->isMissing(idx))
                        {
                            aircraftAlts[idx] = includedFields[ConfKeys::AircraftIndicatedAltitude]->getAsFloat(idx);
                        }
                    }
                }
                else if (!field.second->isMissing(idx))
                {
                     aircraftAlts[idx] = field.second->getAsFloat(idx);
                }
            }
        }

        return std::make_shared<DataObject<float>>(aircraftAlts,
                                                    getExportName(),
                                                    groupByField_,
                                                    referenceObj->getDims(),
                                                    referenceObj->getPath(),
                                                    referenceObj->getDimPaths());
    }

    void AircraftAltitudeVariable::checkKeys(const BufrDataMap& map)
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

        errStr << " could not be found during export of AircraftAltitude object.";

        if (isKeyMissing)
        {
            throw eckit::BadParameter(errStr.str());
        }
    }

    QueryList AircraftAltitudeVariable::makeQueryList() const
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

    std::string AircraftAltitudeVariable::getExportKey(const std::string& name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
