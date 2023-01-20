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
        /// \param conf The configuration for this filter
        explicit BoundingFilter(const eckit::LocalConfiguration& conf);

        virtual ~BoundingFilter() = default;

        /// \brief Apply the filter to the dataevant data.
        void apply(BufrDataMap& dataMap) final;

        /// \param dataMap Map to modify by filtering out rel
     private:
         const std::string variable_;
         std::shared_ptr<float> lowerBound_;
         std::shared_ptr<float> upperBound_;
    };
}  // namespace Ingester
