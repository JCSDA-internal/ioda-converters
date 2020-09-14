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
#include "Export.h"


namespace Ingester
{
    class DatetimeExport final : public Export
    {
     public:
        explicit DatetimeExport(const eckit::Configuration& conf);
        ~DatetimeExport() final = default;

        std::shared_ptr<DataObject> exportData(BufrDataMap map) final;

     private:
        const std::string yearKey_;
        const std::string monthKey_;
        const std::string dayKey_;
        const std::string hourKey_;
        const std::string minuteKey_;
        const std::string secondKey_;
        const bool isUTC_;
    };
}  // namespace Ingester
