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

#include "DataObject/ArrayDataObject.h"
#include "DataObject/DataObject.h"
#include "DataObject/StrVecDataObject.h"


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
        const char* GroupByField = "group_by";
    }  // namespace ConfKeys
}  // namespace


namespace Ingester
{
    DatetimeVariable::DatetimeVariable(const std::string& exportName, 
                                       const eckit::Configuration &conf) :
      Variable(exportName),
      yearQuery_(conf.getString(ConfKeys::Year)),
      monthQuery_(conf.getString(ConfKeys::Month)),
      dayQuery_(conf.getString(ConfKeys::Day)),
      hourQuery_(conf.getString(ConfKeys::Hour)),
      minuteQuery_(conf.getString(ConfKeys::Minute)),
      groupByField_(""),
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

        if (conf.has(ConfKeys::GroupByField))
        {
            groupByField_ = conf.getString(ConfKeys::GroupByField);
        }

        initQueryMap();
    }

    std::shared_ptr<DataObject> DatetimeVariable::exportData(const BufrDataMap& map)
    {
        checkKeys(map);

        auto datetimes = std::vector<std::string>();

        auto years = map.at(getExportKey(ConfKeys::Year))->getFloats();
        auto months = map.at(getExportKey(ConfKeys::Month))->getFloats();
        auto days = map.at(getExportKey(ConfKeys::Day))->getFloats();
        auto hours = map.at(getExportKey(ConfKeys::Hour))->getFloats();
        auto minutes = map.at(getExportKey(ConfKeys::Minute))->getFloats();

        datetimes.resize(years.size());
        for (unsigned int idx = 0; idx < years.size(); idx++)
        {
            // YYYY-MM-DDThh:mm:ssZ
            std::ostringstream datetimeStr;
            datetimeStr << std::setfill('0')
                        << std::setw(4) << years[idx] << "-" \
                        << std::setw(2) << months[idx] << "-" \
                        << std::setw(2) << days[idx] << "T" \
                        << std::setw(2) << hours[idx] - hoursFromUtc_ << ":" \
                        << std::setw(2) << minutes[idx] << ":";

            if (!secondQuery_.empty())
            {
                datetimeStr << std::setw(2)
                            << map.at(getExportKey(ConfKeys::Second))->getFloat(idx, 0)
                            << ":";
            }
            else
            {
                datetimeStr << std::setw(2) << 0;
            }

            datetimeStr << "Z";
            datetimes[idx] = datetimeStr.str();
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

        if (!secondQuery_.empty()) // Second
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
