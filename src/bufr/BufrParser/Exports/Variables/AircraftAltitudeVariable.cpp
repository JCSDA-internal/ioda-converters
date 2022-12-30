/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <string>

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
        const char* PressureAltitudeRelativeToMeanSeaLevel =
            "pressureAltitudeRelativeToMeanSeaLevel";
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
      Variable(exportName, groupByField, conf)
    {
        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> AircraftAltitudeVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        std::unordered_map<std::string, std::shared_ptr<DataObjectBase>> includedFieldMap;
        std::vector<std::string> includedFields;

        std::shared_ptr<DataObjectBase> referenceObj = nullptr;
        for (const auto& fieldName : FieldNames)
        {
            if (conf_.has(fieldName))
            {
                includedFieldMap.insert({fieldName, map.at(getExportKey(fieldName))});
                includedFields.push_back(fieldName);
            }
        }

        referenceObj = (*includedFieldMap.begin()).second;

        // Validation: make sure the dimensions are consistent
        auto path = referenceObj->getPath();
        for (const auto& fieldName : FieldNames)
        {
            if (includedFieldMap.find(fieldName) != includedFieldMap.end() &&
                includedFieldMap[fieldName]->getPath() != path)
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
            std::cout << "idx = " << idx << std::endl;
            for (auto nameIt = includedFields.rbegin(); nameIt != includedFields.rend(); ++nameIt)
            {
                const auto& fieldName = *nameIt;
                const auto& fieldValues = includedFieldMap.at(fieldName);
                if (fieldName == ConfKeys::Pressure)
                {
                    if (!fieldValues->isMissing(idx))
                    {
                        auto value = fieldValues->getAsFloat(idx);
                        if (value < 22630.0f)
                        {
                            aircraftAlts[idx]  =
                                11000.0f - (std::log1p(value / 22630.0f) / 0.0001576106f);
                            std::cout << "section 1" << std::endl;
                        }
                        else
                        {
                            aircraftAlts[idx]  =
                                (1.0f - powf((value / 101325.0f),
                                             (1.0f / 5.256f))) * (288.15f / 0.0065f);
                            std::cout << "section 2" << std::endl;
                        }
                    }
                    else if (includedFieldMap.find(ConfKeys::AircraftIndicatedAltitude)
                             != includedFieldMap.end())
                    {
                        if (!includedFieldMap[ConfKeys::AircraftIndicatedAltitude]->isMissing(idx))
                        {
                            aircraftAlts[idx] =
                             includedFieldMap[ConfKeys::AircraftIndicatedAltitude]->getAsFloat(idx);
                            std::cout << "section 3" << std::endl;
                        }
                    }
                }
                else if (fieldName == ConfKeys::AircraftIndicatedAltitude)
                {
                    // This variable is only used in conjunction with pressure.
                    continue;
                    std::cout << "section 4" << std::endl;
                }
                else if (!fieldValues->isMissing(idx))
                {
                    std::cout << " fieldValues = " << fieldValues << std::endl;
                    aircraftAlts[idx] = fieldValues->getAsFloat(idx);
                    std::cout << "section 5" << std::endl;
                }
            }
        std::cout << "AA = " << aircraftAlts[idx] << std::endl;
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
