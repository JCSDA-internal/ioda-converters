/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>
#include <ostream>
#include <iomanip>

#include "DatetimeExport.h"


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
        const char* Utc = "isUTC";
    }  // ConfKeys
}


namespace Ingester
{
    DatetimeExport::DatetimeExport(const eckit::Configuration& conf) :
      yearKey_(conf.getString(ConfKeys::Year)),
      monthKey_(conf.getString(ConfKeys::Month)),
      dayKey_(conf.getString(ConfKeys::Day)),
      hourKey_(conf.getString(ConfKeys::Hour)),
      minuteKey_(conf.getString(ConfKeys::Minute)),
      secondKey_(conf.getString(ConfKeys::Second)),
      isUTC_(conf.getBool(ConfKeys::Utc))
    {
    }

    std::shared_ptr<DataObject> DatetimeExport::exportData(const BufrDataMap& map)
    {
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
                        << std::setw(2) << map.at(hourKey_)(idx) << ":" \
                        << std::setw(2) << map.at(minuteKey_)(idx) << ":" \
                        << std::setw(2) << map.at(secondKey_)(idx);

            if (isUTC_)
            {
                datetimeStr << "Z";
            }

            datetimes.push_back(datetimeStr.str());
        }

        return std::make_shared<StrVecDataObject>(datetimes);
    }
}  // namespace Ingester
