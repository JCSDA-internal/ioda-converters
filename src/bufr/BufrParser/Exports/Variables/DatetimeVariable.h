/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "ResultSet.h"

#include "DataObject/StrVecDataObject.h"
#include "Variable.h"


namespace Ingester
{
    /// \brief Exports parsed data as datetimes using speciefied Mnemonics
    class DatetimeVariable final : public Variable
    {
     public:
        DatetimeVariable() = delete;
        DatetimeVariable(const std::string& exportName, const eckit::Configuration &conf);
        ~DatetimeVariable() final = default;

        /// \brief Get the configured mnemonics and turn them into datetime strings
        /// \param map BufrDataMap that contains the parsed data for each mnemonic
        std::shared_ptr<DataObject> exportData(const BufrDataMap& map) final;

        /// \brief Get a list of queries for this variable
        std::map<std::string, std::string> makeQueryMap() const final;

     private:
        /// \brief Query for year
        const std::string yearKey_;

        /// \brief Query for month
        const std::string monthKey_;

        /// \brief Query for day
        const std::string dayKey_;

        /// \brief Query for hour
        const std::string hourKey_;

        /// \brief Query for minute
        const std::string minuteKey_;

        /// \brief Query for second (optional)
        std::string secondKey_;

        /// \brief Hours to offset from UTC (optional)
        int hoursFromUtc_;

        /// \brief makes sure the bufr data map has all the required keys.
        void checkKeys(const BufrDataMap& map);
    };
}  // namespace Ingester
