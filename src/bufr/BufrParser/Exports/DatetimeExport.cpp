//
// Created by Ronald McLaren on 9/2/20.
//

#include <iostream>
#include <ostream>
#include <iomanip>

#include "DatetimeExport.h"

static const char* YEAR_KEY = "year";
static const char* MONTH_KEY = "month";
static const char* DAY_KEY = "day";
static const char* HOUR_KEY = "hour";
static const char* MINUTE_KEY = "minute";
static const char* SECOND_KEY = "second";
static const char* UTC_KEY = "isUTC";

namespace Ingester
{
    DatetimeExport::DatetimeExport(const eckit::Configuration& conf) :
      yearKey_(conf.getString(YEAR_KEY)),
      monthKey_(conf.getString(MONTH_KEY)),
      dayKey_(conf.getString(DAY_KEY)),
      hourKey_(conf.getString(HOUR_KEY)),
      minuteKey_(conf.getString(MINUTE_KEY)),
      secondKey_(conf.getString(SECOND_KEY)),
      isUTC_(conf.getBool(UTC_KEY))
    {
    }

    IngesterStrVector DatetimeExport::exportData(BufrDataMap map)
    {
        auto datetimes = std::vector<std::string>();

        datetimes.reserve(map.at(yearKey_).size());
        for (unsigned int idx = 0; idx < map.at(yearKey_).size(); idx++)
        {
            //YYYY-MM-DDThh:mm:ssZ
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

        return datetimes;
    }
}  // namespace Ingester