/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "Filter.h"

#include <memory>
#include <string>
#include <vector>


namespace Ingester
{
    /// \brief Class that filter data given optional upper and lower bounds.
    class BoundingFilter : public Filter
    {
     public:
        /// \brief Constructor
        /// \param variable Variable to filter
        /// \param lowerBound Lowest allowable value
        /// \param upperBound Highest allowable value
        BoundingFilter(const std::string& variable,
                       std::shared_ptr<float> lowerBound,
                       std::shared_ptr<float> upperBound);

        virtual ~BoundingFilter() = default;

        /// \brief Apply the filter to the dataevant data.
        void apply(BufrDataMap& dataMap) final;

        /// \param dataMap Map to modify by filtering out rel
     private:
         const std::string variable_;
         const std::shared_ptr<float> lowerBound_;
         const std::shared_ptr<float> upperBound_;
    };
}  // namespace Ingester
