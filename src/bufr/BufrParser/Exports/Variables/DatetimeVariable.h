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

#include "BufrParser/BufrTypes.h"
#include "DataObject/StrVecDataObject.h"
#include "Variable.h"


namespace Ingester
{
    /// \brief Exports parsed data as datetimes using speciefied Mnemonics
    class DatetimeVariable final : public Variable
    {
     public:
        explicit DatetimeVariable(const eckit::Configuration& conf);
        ~DatetimeVariable() final = default;

        /// \brief Get the configured mnemonics and turn them into datetime strings
        /// \param map BufrDataMap that contains the parsed data for each mnemonic
        std::shared_ptr<DataObject> exportData(const BufrDataMap& map) final;

     private:
        /// \brief Mnemonic for year
        const std::string yearKey_;

        /// \brief Mnemonic for month
        const std::string monthKey_;

        /// \brief Mnemonic for day
        const std::string dayKey_;

        /// \brief Mnemonic for hour
        const std::string hourKey_;

        /// \brief Mnemonic for minute
        const std::string minuteKey_;

        /// \brief Mnemonic for second
        const std::string secondKey_;

        /// \brief Is it UTC time or not
        const bool isUTC_;
    };
}  // namespace Ingester
