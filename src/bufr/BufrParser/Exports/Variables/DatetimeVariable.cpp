/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <climits>
#include <iostream>
#include <iomanip>
#include <unordered_map>

#include <ostream>
#include <time.h>
#include <vector>

#include "eckit/exception/Exceptions.h"

#ifndef BUILD_PYTHON_BINDING
    #include "oops/util/Logger.h"
#endif

#include "DataObject.h"
#include "DatetimeVariable.h"


namespace
{
    namespace ConfKeys
    {
        const char* Year = "year";
        const char* Month = "month";
        const char* Day = "day";
        const char* Hour = "hour";
        const char* Minute = "minute";
        const char* Second = "second";
        const char* HoursFromUtc = "hoursFromUtc";
    }  // namespace ConfKeys
}  // namespace


namespace Ingester
{
    DatetimeVariable::DatetimeVariable(const std::string& exportName,
                                       const std::string& groupByField,
                                       const eckit::LocalConfiguration& conf) :
      Variable(exportName, groupByField, conf),
      yearQuery_(conf.getString(ConfKeys::Year)),
      monthQuery_(conf.getString(ConfKeys::Month)),
      dayQuery_(conf.getString(ConfKeys::Day)),
      hourQuery_(conf.getString(ConfKeys::Hour)),
      minuteQuery_(conf.getString(ConfKeys::Minute)),
      hoursFromUtc_(0)
    {
        if (conf.has(ConfKeys::Second))
        {
            secondQuery_ = conf.getString(ConfKeys::Second);
        }

        if (conf.has(ConfKeys::HoursFromUtc))
        {
            hoursFromUtc_ = conf.getInt(ConfKeys::HoursFromUtc);
        }

        initQueryMap();
    }

    std::shared_ptr<DataObjectBase> DatetimeVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);
        static const int missingInt = DataObject<int>::missingValue();

        setenv("TZ", "UTC", 1);      // Force UTC time zone
        std::tm tm{};                // zero initialise
        tm.tm_year = 1970-1900;      // 1970
        tm.tm_mon = 0;               // Jan=0, Feb=1, ...
        tm.tm_mday = 1;              // 1st
        tm.tm_hour = 0;              // midnight
        tm.tm_min = 0;
        tm.tm_sec = 0;
        tm.tm_isdst = 0;             // Not daylight saving
        std::time_t epochDt = std::mktime(&tm);

        std::vector<int64_t> timeOffsets;
        timeOffsets.reserve(map.at(getExportKey(ConfKeys::Year))->size());

        auto yearVar = map.at(getExportKey(ConfKeys::Year));

        // Validation
        if (!yearVar->hasSamePath(map.at(getExportKey(ConfKeys::Month))) ||
            !yearVar->hasSamePath(map.at(getExportKey(ConfKeys::Day))) ||
            (!minuteQuery_.empty() &&
                !yearVar->hasSamePath(map.at(getExportKey(ConfKeys::Minute)))) ||
            (!secondQuery_.empty() &&
                !yearVar->hasSamePath(map.at(getExportKey(ConfKeys::Second)))))
        {
            std::ostringstream errStr;
            errStr << "Datetime variables are not all from the same path.";
            throw eckit::BadParameter(errStr.str());
        }

        for (unsigned int idx = 0; idx < map.at(getExportKey(ConfKeys::Year))->size(); idx++)
        {
            int year = map.at(getExportKey(ConfKeys::Year))->getAsInt(idx);
            int month = map.at(getExportKey(ConfKeys::Month))->getAsInt(idx);
            int day = map.at(getExportKey(ConfKeys::Day))->getAsInt(idx);
            int hour = map.at(getExportKey(ConfKeys::Hour))->getAsInt(idx);
            int minutes = 0;
            int seconds = 0;

            auto diff_time = DataObject<int64_t>::missingValue();
            if (year != missingInt &&
                month != missingInt &&
                day != missingInt &&
                hour != missingInt)
            {
                tm.tm_year = year - 1900;
                tm.tm_mon = month - 1;
                tm.tm_mday = day;
                tm.tm_hour = hour;
                tm.tm_min = 0;
                tm.tm_sec = 0;
                tm.tm_isdst = 0;

                if (!minuteQuery_.empty())
                {
                    minutes = map.at(getExportKey(ConfKeys::Minute))->getAsInt(idx);

                    if (minutes >= 0 && minutes < 60)
                    {
                        tm.tm_min = minutes;
                    }
                }

                if (!secondQuery_.empty())
                {
                    seconds = map.at(getExportKey(ConfKeys::Second))->getAsInt(idx);

                    if (seconds >= 0 && seconds < 60)
                    {
                        tm.tm_sec = seconds;
                    }
                }

                // Be careful with mktime as it can be very slow.
                auto thisTime = std::mktime(&tm);
                if (thisTime < 0)
                {
#ifndef BUILD_PYTHON_BINDING
                     oops::Log::warning() << "Caution, date suspicious date (year, month, day): "
                                          << year << ", "
                                          << month << ", "
                                          << day << std::endl;
#endif

#ifdef BUILD_PYTHON_BINDING
                    std::cout << "Caution, date suspicious date (year, month, day): "
                              << year << ", "
                              << month << ", "
                              << day << std::endl;
#endif
                }

                diff_time = static_cast<int64_t>(difftime(thisTime, epochDt)
                    + hoursFromUtc_ * 3600);
            }

            timeOffsets.push_back(diff_time);
        }

        return std::make_shared<DataObject<int64_t>>(
                timeOffsets,
                getExportName(),
                groupByField_,
                map.at(getExportKey(ConfKeys::Year))->getDims(),
                map.at(getExportKey(ConfKeys::Year))->getPath(),
                map.at(getExportKey(ConfKeys::Year))->getDimPaths());
    }

    void DatetimeVariable::checkKeys(const BufrDataMap& map)
    {
        std::vector<std::string> requiredKeys = {getExportKey(ConfKeys::Year),
                                                 getExportKey(ConfKeys::Month),
                                                 getExportKey(ConfKeys::Day),
                                                 getExportKey(ConfKeys::Hour),
                                                 getExportKey(ConfKeys::Minute)};

        if (!secondQuery_.empty())
        {
            requiredKeys.push_back(getExportKey(ConfKeys::Second));
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

    QueryList DatetimeVariable::makeQueryList() const
    {
        auto queries = QueryList();

        {  // Year
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Year);
            info.query = yearQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        {  // Month
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Month);
            info.query = monthQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        {  // Day
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Day);
            info.query = dayQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        {  // Hour
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Hour);
            info.query = hourQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        {  // Minute
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Minute);
            info.query = minuteQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        if (!secondQuery_.empty())  // Second
        {
            QueryInfo info;
            info.name = getExportKey(ConfKeys::Second);
            info.query = secondQuery_;
            info.groupByField = groupByField_;
            queries.push_back(info);
        }

        return queries;
    }

    std::string DatetimeVariable::getExportKey(const char* name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
