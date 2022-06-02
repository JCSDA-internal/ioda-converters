/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "Split.h"

#include <string>
#include <vector>
#include <unordered_map>


namespace Ingester
{
    /// \brief Data splitter class that splits data according to a predefined categories.
    /// \details This class sub-divides data into sub-categories depending on the value of a
    ///          variable. It is assumed that the variable values are integers which represent
    ///          separate categories of data. An example is Satellite ID (variable: SAID) where each
    ///          possible satellite has its own unique integer ID.
    ///          The subcategories this Split divides into can either be manually specified by a
    ///          NameMap (map<integer, string>) or be automatically determined (if the given NameMap
    ///          is found to be empty). An example NameMap might look like this:
    ///            { 257 : GEOS-13,
    ///              259 : GEOS-15 }
    ///          This NameMap tells the splitter to divide by the values of the given variable
    ///          (SAID for this example) into two named groups (GEOS-13 and GEOS-15). Data
    ///          associated with variable values not specified in the map are discarded. If the
    ///          NameMap were empty (unspecified) then this splitter will use the data to to
    ///          determine all all the possible values to split on automatically. Each split would
    ///          then be named according to its integer value (ex: 257, 259, 270, 271, ....).
    class CategorySplit : public Split
    {
     public:
        /// \brief Map of integers to strings where the key represents the split mnemonics integer
        ///        value and the value is a human readable name for the key.
        typedef  std::map<int, std::string> NameMap;

        /// \brief constructor
        /// \param variable Variable to base the split on.
        /// \param map Name of the created categories from the integer BUFR values. May be an
        ///        empty map in which case subcategories are automatically determined from the
        ///        data.
        CategorySplit(const std::string& name, const std::string& variable, const NameMap& map);

        /// \brief Get list of sub categories this split will create
        /// \result Set of unique strings.
        std::vector<std::string> subCategories(const BufrDataMap& dataMap) final;

        /// \brief Split the data according to internal rules
        /// \param dataMap Data to be split
        /// \result map of split data where the category is the key
        std::unordered_map<std::string, BufrDataMap> split(const BufrDataMap& dataMap) final;

     private:
        const std::string variable_;

        NameMap nameMap_;


        /// \brief Adds values to nameMap_ using the data if nameMap_ is empty.
        /// \param dataMap Data to be split
        void updateNameMap(const BufrDataMap& dataMap);
    };
}  // namespace Ingester


