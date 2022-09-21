/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#pragma once

#include "IngesterTypes.h"

namespace Ingester
{
    /// \brief Base class for all the supported filters.
    class Filter
    {
     public:
        /// \brief Apply the filter to the data
        /// \param dataMap Map to modify by filtering out relevant data.
        virtual void apply(BufrDataMap& dataMap) = 0;
    };
}  // namespace Ingester
