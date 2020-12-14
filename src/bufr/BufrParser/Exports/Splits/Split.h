/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>

#include "BufrParser/BufrTypes.h"

namespace Ingester
{
    /// \brief Base class for all Split objects that split data into sub-parts
    class Split
    {
     public:
        Split() = default;

        /// \brief Get list of sub categories this split will create
        virtual std::vector<std::string> subCategories() = 0;

        /// \brief Split the data according to internal rules
        /// \param dataMap Data to be split
        /// \result map of split data where the category is the key
        virtual std::map<std::string, BufrDataMap> split(const BufrDataMap& dataMap) = 0;
    };
}  //namespace Ingester


