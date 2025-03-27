/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iomanip>
#include <iostream>
#include <locale>
#include <ostream>
#include <ctime>
#include <vector>
#include <unordered_map>

#include "eckit/exception/Exceptions.h"
#include "oops/util/Logger.h"

#include "DataObject.h"
#include "TimeoffsetVariable.h"
#include "Transforms/TransformBuilder.h"


namespace
{
    namespace ConfKeys
    {
        const char* Timeoffset = "timeOffset";
        const char* Referencetime = "referenceTime";
        const char* Transforms = "transforms";
    }  // namespace ConfKeys
}  // namespace


namespace Ingester
{
    TimeoffsetVariable::TimeoffsetVariable(const std::string& exportName,
                                           const std::string& groupByField,
                                           const eckit::LocalConfiguration &conf) :
      Variable(exportName, groupByField, conf)
    {
        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> TimeoffsetVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        std::tm tm{};                // zero initialise
        tm.tm_year = 1970 - 1900;    // 1970
        tm.tm_mon = 0;               // Jan=0, Feb=1, ...
        tm.tm_mday = 1;              // 1st
        tm.tm_hour = 0;              // midnight
        tm.tm_min = 0;
        tm.tm_sec = 0;
        tm.tm_isdst = 0;             // Not daylight saving
        std::time_t epochDt = timegm(&tm);

        // Convert the reference time (ISO8601 string) to time struct
        std::tm ref_time = {};
        std::string time_str = conf_.getString(ConfKeys::Referencetime);
        if (strptime(time_str.c_str(), "%Y-%m-%dT%H:%M:%S", &ref_time) == NULL)
        {
            std::ostringstream errStr;
            errStr << "Reference time MUST be formatted like 2021-11-29T22:43:51Z";
            throw eckit::BadParameter(errStr.str());
        }

        auto timeOffsets = map.at(getExportKey(ConfKeys::Timeoffset));
        if (conf_.has(ConfKeys::Transforms))
        {
            auto transforms = TransformBuilder::makeTransforms(conf_);
            for (const auto &transform : transforms)
            {
                transform->apply(timeOffsets);
            }
        }

        auto timeDiffs = std::vector<int64_t> (timeOffsets->size());
        for (size_t idx = 0; idx < timeOffsets->size(); ++idx)
        {
            auto diff_time = DataObject<int64_t>::missingValue();
            if (!timeOffsets->isMissing(idx))
            {
                auto obs_tm = ref_time;
                obs_tm.tm_sec = ref_time.tm_sec + timeOffsets->getAsInt(idx);
                auto thisTime = timegm(&obs_tm);
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
            info.query = conf_.getString(ConfKeys::Timeoffset);
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        // The reference time string is a single scalar variable in YAML, not a query variable.

        return queries;
    }

    std::string TimeoffsetVariable::getExportKey(const char* name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
