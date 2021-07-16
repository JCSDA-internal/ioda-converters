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

        datetimes.reserve(map.at(getExportKey(ConfKeys::Year)).size());
        for (unsigned int idx = 0; idx < map.at(getExportKey(ConfKeys::Year)).size(); idx++)
        {
            // YYYY-MM-DDThh:mm:ssZ
            std::ostringstream datetimeStr;
            datetimeStr << std::setfill('0')
                        << std::setw(4) << map.at(getExportKey(ConfKeys::Year))(idx) << "-" \
                        << std::setw(2) << map.at(getExportKey(ConfKeys::Month))(idx) << "-" \
                        << std::setw(2) << map.at(getExportKey(ConfKeys::Day))(idx) << "T" \
                        << std::setw(2) << map.at(getExportKey(ConfKeys::Hour))(idx) - hoursFromUtc_ << ":" \
                        << std::setw(2) << map.at(getExportKey(ConfKeys::Minute))(idx) << ":";

            if (!secondQuery_.empty())
            {
                datetimeStr << std::setw(2) << map.at(getExportKey(ConfKeys::Second))(idx);
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

    QueryMap DatetimeVariable::makeQueryMap() const
    {
        auto queries = QueryMap();
        queries[getExportKey(ConfKeys::Year)] = yearQuery_;
        queries[getExportKey(ConfKeys::Month)] = monthQuery_;
        queries[getExportKey(ConfKeys::Day)] = dayQuery_;
        queries[getExportKey(ConfKeys::Hour)] = hourQuery_;
        queries[getExportKey(ConfKeys::Minute)] = minuteQuery_;
        if (!secondQuery_.empty()) queries[getExportKey(ConfKeys::Second)] = secondQuery_;

        return queries;
    }

    std::string DatetimeVariable::getExportKey(const char* name) const
    {
        return getExportName() + "_" + name;
    }
}  // namespace Ingester
