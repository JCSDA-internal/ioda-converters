/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "Filter.h"

#include <string>

#include "BufrParser/BufrTypes.h"


namespace Ingester
{
    /// \brief Class that filters data out that is greater than a given value.
    class GreaterFilter : public Filter
    {
     public:
        /// \brief Constructor
        /// \param mnemonic BUFR Mnemonic to filter on
        /// \param value to compare against
        GreaterFilter(const std::string& mnemonic, float value);

        /// \brief Apply the filter to the data
        /// \param dataMap Map to modify by filtering out relevant data.
        void apply(BufrDataMap& dataMap) final;

     private:
        const std::string mnemonic_;
        const float value_;
    };
}  // namespace Ingester


