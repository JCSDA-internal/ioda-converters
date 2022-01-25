/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>
#include <iomanip>
#include <ostream>
#include <time.h>
#include <vector>

#include "eckit/exception/Exceptions.h"

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
        const char* Utc = "isUTC";  // deprecated
    }  // namespace ConfKeys
}  // namespace


namespace Ingester
{
    DatetimeVariable::DatetimeVariable(const eckit::Configuration& conf) :
      yearKey_(conf.getString(ConfKeys::Year)),
      monthKey_(conf.getString(ConfKeys::Month)),
      dayKey_(conf.getString(ConfKeys::Day)),
      hourKey_(conf.getString(ConfKeys::Hour)),
      minuteKey_(conf.getString(ConfKeys::Minute)),
      secondKey_(""),
      hoursFromUtc_(0)
    {
        if (conf.has(ConfKeys::Second))
        {
            secondKey_ = conf.getString(ConfKeys::Second);
        }

        if (conf.has(ConfKeys::HoursFromUtc))
        {
            hoursFromUtc_ = conf.getInt(ConfKeys::HoursFromUtc);
        }

        if (conf.has(ConfKeys::Utc))
        {
            std::cout << "WARNING: usage of " \
                      << ConfKeys::Utc \
                      << " in datetime is depricated!" \
                      << std::endl;
            std::cout << "Use the optional parameter " << ConfKeys::HoursFromUtc << " instead.";
        }
    }

    std::shared_ptr<DataObject> DatetimeVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        std::vector<int64_t> timeOffsets(map.at(yearKey_).size());

        std::tm tm{};                // zero initialise
        tm.tm_year = 1970-1900;      // 1970
        tm.tm_mon = 0;               // Jan=0, Feb=1, ...
        tm.tm_mday = 1;              // 1st
        tm.tm_hour = 0;              // midnight
        tm.tm_min = 0;
        tm.tm_sec = 0;
        tm.tm_isdst = 0;             // Not daylight saving
        std::time_t epochDt = std::mktime(&tm);
        std::time_t this_time = std::mktime(&tm);
        int64_t diff_time;

        for (unsigned int idx = 0; idx < map.at(yearKey_).size(); idx++)
        {
            tm.tm_year = map.at(yearKey_)(idx) - 1900;
            tm.tm_mon = map.at(monthKey_)(idx) - 1;
            tm.tm_mday = map.at(dayKey_)(idx);
            tm.tm_hour = map.at(hourKey_)(idx);
            tm.tm_min = map.at(minuteKey_)(idx);
            tm.tm_sec = 0;
            tm.tm_isdst = 0;

            if (!secondKey_.empty())
            {
                if (map.at(secondKey_)(idx) >= 0 && map.at(secondKey_)(idx) < 60)
                {
                    tm.tm_sec = map.at(secondKey_)(idx);
                }
            }

            this_time = std::mktime(&tm);
            diff_time = static_cast<std::int64_t>(difftime(this_time, epochDt)
                                                  + hoursFromUtc_*3600);
            timeOffsets.push_back(diff_time);
        }
        return std::make_shared<Int64VecDataObject>(timeOffsets);
    }

    void DatetimeVariable::checkKeys(const BufrDataMap& map)
    {
        std::vector<std::string> requiredKeys = {yearKey_,
                                                 monthKey_,
                                                 dayKey_,
                                                 hourKey_,
                                                 minuteKey_};

        if (!secondKey_.empty())
        {
            requiredKeys.push_back(secondKey_);
        }

        std::stringstream errStr;
        errStr << "Mnemonic ";

        bool isKeyMissing = false;
        for (auto key : requiredKeys)
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
}  // namespace Ingester
