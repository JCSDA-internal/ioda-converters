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
#include <map>


namespace Ingester
{
    /// \brief Data splitter class that splits data according to a predefined categories.
    class CategorySplit : public Split
    {
     public:
        /// \brief Map of integers to strings where the key represents the split mnemonics integer
        ///        value and the value is a human readable name for the key.
        typedef  std::map<size_t, std::string> NameMap;

        /// \brief constructor
        /// \param mnemonic BUFR mnemonic to base the split on.
        /// \param map Name of the created categories from the integer BUFR values.
        CategorySplit(const std::string& mnemonic, const NameMap& map);

        /// \brief Get list of sub categories this split will create
        std::vector<std::string> subCategories() final;

        /// \brief Split the data according to internal rules
        /// \param dataMap Data to be split
        /// \result map of split data where the category is the key
        std::map<std::string, BufrDataMap> split(const BufrDataMap& dataMap) final;

        // Getters
        inline std::string getMnemonic() { return mnemonic_; }

     private:
        const NameMap nameMap_;
        const std::string mnemonic_;
    };
}  // namespace Ingester


