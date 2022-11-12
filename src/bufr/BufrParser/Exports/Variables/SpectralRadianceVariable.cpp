/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
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
        auto& refObj = map.at(getExportKey(ConfKeys::ScaledSpectralRadiance));

        // Declare unscale spectral radiance data array from scaled spectral radiance: nlocs * nchns
        std::vector<float> outData((refObj->size()), missingFloat); 

        // Get dimensions
        size_t nlocs = (refObj->getDims())[0];
        size_t nchns = (refObj->getDims())[1];

        // Get start channels from BufrDataMap: nlocs * nbands 
        auto& refObj1 = map.at(getExportKey(ConfKeys::StartChannel));
        std::vector<float> begChannel(refObj1->size()); 
        size_t nbands = (refObj1->getDims())[1];

        // Get end channels from BufrDataMap: nlocs * nbands
        auto& refObj2 = map.at(getExportKey(ConfKeys::EndChannel));
        std::vector<float> endChannel(refObj2->size()); 

        // Get scale factors from BufrDataMap: nlocs * bands
        auto& refObj3 = map.at(getExportKey(ConfKeys::ScaleFactor));
        std::vector<float> scaleFactor(refObj3->size()); 

        // Get sensor channel number from BufrDataMap: nlocs * nbands
        auto& refObj4 = map.at(getExportKey(ConfKeys::SensorChannelNumber));
        std::vector<int> Channel(refObj4->size()); 

        // Get scale factor: nlocs * nbands  
        for (size_t idx = 0; idx < refObj1->size(); idx++) 
        {
            begChannel[idx] = refObj1-> getAsInt(idx);
            endChannel[idx] = refObj2-> getAsInt(idx);
            scaleFactor[idx] = refObj3-> getAsInt(idx);
            scaleFactor[idx] = -(scaleFactor[idx] - 5); 
            scaleFactor[idx] = pow(10.0, scaleFactor[idx]); 
        } 

        for (size_t idx = 0; idx < refObj->size(); idx++) 
        {
            Channel[idx] = refObj4->getAsInt(idx);
            size_t iloc = int(idx / nchns);
            size_t idx2;

            for (size_t ibnd = 0; ibnd < nbands; ibnd++) 
            {
               idx2 = iloc * nbands + ibnd;  
               if (Channel[idx] >= begChannel[idx2] && Channel[idx] <= endChannel[idx2]) break;  
            } 
            if (refObj->getAsFloat(idx) != missingFloat && scaleFactor[idx2] !=missingInt)
                outData[idx] = refObj->getAsFloat(idx) * scaleFactor[idx2]; 
        } 

        return std::make_shared<DataObject<float>>(outData,
                                                   getExportName(),
                                                   groupByField_,
                                                   refObj->getDims(),
                                                   refObj->getPath(),
                                                   refObj->getDimPaths());
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
