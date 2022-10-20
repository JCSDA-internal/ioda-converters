/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <climits>
#include <iostream>
#include <iomanip>
#include <locale>
#include <unordered_map>

#include <ostream>
#include <sstream>
#include <time.h>
#include <vector>

#include "eckit/exception/Exceptions.h"
#include "oops/util/Logger.h"

#include "DataObject.h"
#include "TimeoffsetVariable.h"


namespace
{
    namespace ConfKeys
    {
        const char* Timeoffset = "time_offset";
        const char* Referencetime = "reference_time";
        const char* GroupByField = "group_by";
    }  // namespace ConfKeys
}  // namespace


namespace Ingester
{
    TimeoffsetVariable::TimeoffsetVariable(const std::string& exportName,
                                       const std::string& groupByField,
                                       const eckit::Configuration &conf) :
      Variable(exportName),
      referenceTime_(conf.getString(ConfKeys::Referencetime)),
      timeOffsetQuery_(conf.getString(ConfKeys::Timeoffset)),
      groupByField_(groupByField)
    {

        if (conf.has(ConfKeys::GroupByField))
        {
            groupByField_ = conf.getString(ConfKeys::GroupByField);
        }

        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> TimeoffsetVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        std::tm tm{};                // zero initialise
        tm.tm_year = 1970 - 1900;      // 1970
        tm.tm_mon = 0;               // Jan=0, Feb=1, ...
        tm.tm_mday = 1;              // 1st
        tm.tm_hour = 0;              // midnight
        tm.tm_min = 0;
        tm.tm_sec = 0;
        tm.tm_isdst = 0;             // Not daylight saving
        std::time_t epochDt = std::mktime(&tm);

        // Convert the reference time (ISO8601 string) to time struct
        std::tm ref_time = {};
        std::istringstream ss(referenceTime_);
        ss.imbue(std::locale("en_US.utf-8"));
        ss >> std::get_time(&ref_time, "%Y-%m-%dT%H:%M:%S");
        if (ss.fail())
        {
            std::ostringstream errStr;
            errStr << "Reference time MUST be formatted like 2021-11-29T22:43:51Z";
            throw eckit::BadParameter(errStr.str());
        }

//        // Validation
//        size_t ndims = map.at(getExportKey(ConfKeys::Timeoffset))->getDims().size();
//        if (ndims > 2)
//        {
//            std::ostringstream errStr;
//            errStr << "Timeoffset variable cannot be more than 2 dimensions.";
//            throw eckit::BadParameter(errStr.str());
//        }
//
//        // Falsify a 2D variable with second dimension size=1 if Timeoffset is 1D
//        size_t nlocs = map.at(getExportKey(ConfKeys::Timeoffset))->getDims().at(0);
//        size_t nlevs = 1;
//        if (ndims == 2)
//        {
//            nlevs = map.at(getExportKey(ConfKeys::Timeoffset))->getDims().at(1);
//        }
//
//        std::vector<std::vector<int64_t>> timeOffsets(nlocs, std::vector<float> (nlevs));
//
//        for (size_t nlev = 0; nlev < nlevs; nlev++)
//        {
//            for (size_t nloc = 0; nloc < nlocs; nloc++)
//            {
//                // This next line will fail since variable (BUFR mnemonic) is 2D, not 1D
//                float offset = map.at(getExportKey(ConfKeys::Timeoffset))->getAsFloat(idx);
//
//                auto diff_time = DataObject<int64_t>::missingValue();
//                if (offset != missingVal)
//                {
//                    ref_time.tm_sec += offset;
//                    auto thisTime = std::mktime(&ref_time);
//                    diff_time = static_cast<int64_t>(difftime(thisTime, epochDt));
//                }
//                timeOffsets[nloc][nlev] = diff_time;
//            }
//        }

        auto timeOffsets = map.at(getExportKey(ConfKeys::Timeoffset));
        auto timeDiffs = std::vector<int64_t> (timeOffsets->size());
        for (size_t idx = 0; idx < timeOffsets->size(); ++idx)
        {
            auto diff_time = DataObject<int64_t>::missingValue();
            if (!timeOffsets->isMissing(idx))
            {
                ref_time.tm_sec += timeOffsets->getAsInt(idx);
                auto thisTime = std::mktime(&ref_time);
                diff_time = static_cast<int64_t>(difftime(thisTime, epochDt));
            }

            timeDiffs[idx] = diff_time;
        }

        return std::make_shared<DataObject<int64_t>>(timeDiffs,
                                                     getExportName(),
                                                     groupByField_,
                                                     timeOffsets->getDims(),
                                                     timeOffsets->getPath(),
                                                     timeOffsets->getDimPaths());
    }

    void TimeoffsetVariable::checkKeys(const BufrDataMap& map)
    {
        std::vector<std::string> requiredKeys = {getExportKey(ConfKeys::Timeoffset)};

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

    QueryList TimeoffsetVariable::makeQueryList() const
    {
        auto queries = QueryList();

        {  // Timeoffset
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Timeoffset);
            info.query = timeOffsetQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        // The reference time string is a single scalar variable in YAML, not a query variable.

        return queries;
    }

    std::string TimeoffsetVariable::getExportKey(const char* name) const
    {
        // return getExportName() + "_" + name;
        return name;
    }
}  // namespace Ingester
