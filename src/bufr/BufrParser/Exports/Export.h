/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <vector>
#include <string>
#include <memory>

#include "BufrTypes.h"
#include "DataObject/DataObject.h"


namespace BufrParser
{
    /// \brief Abstract base class for all Exports.
    class Export
    {
     public:
        virtual ~Export() = default;

        /// \brief Export data objects for previously parsed data from BufrDataMap.
        virtual std::shared_ptr<IodaEncoder::DataObject> exportData(const BufrDataMap& map) = 0;
    };
}  // namespace BufrParser


