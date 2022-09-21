/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <unordered_map>
#include <vector>

#include "IngesterTypes.h"

namespace Ingester
{
    /// \brief Base class for all Split objects that split data into sub-parts
    class Split
    {
     public:
        explicit Split(const std::string& name);
        virtual ~Split() = default;

        /// \brief Get set of sub categories this split will create
        /// \param dataMap The data we will split on.
        /// \result set of unique strings
        virtual std::vector<std::string> subCategories(const BufrDataMap& dataMap) = 0;

        /// \brief Split the data according to internal rules
        /// \param dataMap Data to be split
        /// \result map of split data where the category is the key
        virtual std::unordered_map<std::string, BufrDataMap> split(const BufrDataMap& dataMap) = 0;

        /// \brief Get the split name
        inline std::string getName() const { return name_; }

     private:
        /// \brief The name of the split as defined by the key in the YAML file.
        const std::string name_;
    };
}  // namespace Ingester
