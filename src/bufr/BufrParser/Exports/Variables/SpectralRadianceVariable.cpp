/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <cmath>
#include <bits/stdc++.h>
#include <climits>
#include <iostream>
#include <iomanip>
#include <unordered_map>

#include <ostream>
#include <time.h>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "oops/util/Logger.h"

#include "DataObject.h"
#include "SpectralRadianceVariable.h"


namespace
{
    namespace ConfKeys
    {
        const char* SensorChannelNumber = "sensorChannelNumber";
        const char* StartChannel = "startChannel";
        const char* EndChannel = "endChannel";
        const char* ScaleFactor = "scaleFactor";
        const char* ScaledSpectralRadiance = "scaledSpectralRadiance";
    }  // namespace ConfKeys

    const std::vector<std::string> FieldNames = {ConfKeys::SensorChannelNumber,
                                                 ConfKeys::StartChannel,
                                                 ConfKeys::EndChannel,
                                                 ConfKeys::ScaleFactor,
                                                 ConfKeys::ScaledSpectralRadiance};
}  // namespace


namespace Ingester
{
    SpectralRadianceVariable::SpectralRadianceVariable(const std::string& exportName,
                                                       const std::string& groupByField,
                                                       const eckit::Configuration &conf) :
      Variable(exportName),
      groupByField_(groupByField),
      conf_(conf)
    {
        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> SpectralRadianceVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        static const float missingFloat = DataObject<float>::missingValue();
        static const int missingInt = DataObject<int>::missingValue();

        // Get scaled spectral radiance from BufrDataMap
        auto& radObj = map.at(getExportKey(ConfKeys::ScaledSpectralRadiance));

        // Declare unscale spectral radiance data array from scaled spectral radiance
        std::vector<float> outData((radObj->size()), missingFloat);

        // Get dimensions
        size_t nchns = (radObj->getDims())[1];

        // Get start channels from BufrDataMap
        auto& startChanObj = map.at(getExportKey(ConfKeys::StartChannel));
        std::vector<float> begChannel(startChanObj->size());
        size_t nbands = (startChanObj->getDims())[1];

        // Get end channels from BufrDataMap
        auto& endChanObj = map.at(getExportKey(ConfKeys::EndChannel));
        std::vector<float> endChannel(endChanObj->size());

        // Get scale factors from BufrDataMap
        auto& scaleFactorObj = map.at(getExportKey(ConfKeys::ScaleFactor));
        std::vector<float> scaleFactor(scaleFactorObj->size());

        // Get sensor channel number from BufrDataMap
        auto& sensorChanObj = map.at(getExportKey(ConfKeys::SensorChannelNumber));
        std::vector<int> Channel(sensorChanObj->size());

        // Get scale factor
        for (size_t idx = 0; idx < startChanObj->size(); idx++)
        {
            begChannel[idx] = startChanObj-> getAsInt(idx);
            endChannel[idx] = endChanObj-> getAsInt(idx);
            scaleFactor[idx] = scaleFactorObj-> getAsInt(idx);
            // convert W/m2 to mW/m2
            scaleFactor[idx] = -(scaleFactor[idx] - 5.0);
            scaleFactor[idx] = pow(10.0, scaleFactor[idx]);
        }

        for (size_t idx = 0; idx < radObj->size(); idx++)
        {
            Channel[idx] = sensorChanObj->getAsInt(idx);
            size_t iloc = static_cast<int>(idx / nchns);
            size_t idx2;

            for (size_t ibnd = 0; ibnd < nbands; ibnd++)
            {
               idx2 = iloc * nbands + ibnd;
               if (Channel[idx] >= begChannel[idx2] && Channel[idx] <= endChannel[idx2]) break;
            }
            if (radObj->getAsFloat(idx) != missingFloat && scaleFactor[idx2] !=missingInt)
                outData[idx] = radObj->getAsFloat(idx) * scaleFactor[idx2];
        }

        return std::make_shared<DataObject<float>>(outData,
                                                   getExportName(),
                                                   groupByField_,
                                                   radObj->getDims(),
                                                   radObj->getPath(),
                                                   radObj->getDimPaths());
    }

    void SpectralRadianceVariable::checkKeys(const BufrDataMap& map)
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

    QueryList SpectralRadianceVariable::makeQueryList() const
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

    std::string SpectralRadianceVariable::getExportKey(const std::string& name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
