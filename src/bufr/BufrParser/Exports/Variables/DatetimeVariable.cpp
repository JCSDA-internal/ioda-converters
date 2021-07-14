/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>
#include <iomanip>
#include <ostream>
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
    DatetimeVariable::DatetimeVariable(const std::string& exportName, const eckit::Configuration &conf) :
      Variable(exportName),
      yearKey_(conf.getString(ConfKeys::Year)),
      monthKey_(conf.getString(ConfKeys::Month)),
      dayKey_(conf.getString(ConfKeys::Day)),
      hourKey_(conf.getString(ConfKeys::Hour)),
      minuteKey_(conf.getString(ConfKeys::Minute)),
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

        initQueryMap();
    }

    std::shared_ptr<DataObject> DatetimeVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        auto datetimes = std::vector<std::string>();

        datetimes.reserve(map.at(yearKey_).size());
        for (unsigned int idx = 0; idx < map.at(yearKey_).size(); idx++)
        {
            // YYYY-MM-DDThh:mm:ssZ
            std::ostringstream datetimeStr;
            datetimeStr << std::setfill('0')
                        << std::setw(4) << map.at(yearKey_)(idx) << "-" \
                        << std::setw(2) << map.at(monthKey_)(idx) << "-" \
                        << std::setw(2) << map.at(dayKey_)(idx) << "T" \
                        << std::setw(2) << map.at(hourKey_)(idx) - hoursFromUtc_ << ":" \
                        << std::setw(2) << map.at(minuteKey_)(idx) << ":";

            if (!secondKey_.empty())
            {
                datetimeStr << std::setw(2) << map.at(secondKey_)(idx);
            }
            else
            {
                datetimeStr << std::setw(2) << 0;
            }

            datetimeStr << "Z";

            datetimes.push_back(datetimeStr.str());
        }

        return std::make_shared<StrVecDataObject>(datetimes);
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

    QueryMap DatetimeVariable::makeQueryMap() const
    {
        auto queries = QueryMap();
        queries[getExportName() + "_" + ConfKeys::Year] = yearKey_;
        queries[getExportName() + "_" + ConfKeys::Month] = monthKey_;
        queries[getExportName() + "_" + ConfKeys::Day] = dayKey_;
        queries[getExportName() + "_" + ConfKeys::Hour] = hourKey_;
        queries[getExportName() + "_" + ConfKeys::Minute] = minuteKey_;
        if (!secondKey_.empty()) queries[getExportName() + "_" + ConfKeys::Second] = secondKey_;

        return queries;
    }
}  // namespace Ingester
