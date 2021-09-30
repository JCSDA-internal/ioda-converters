/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory.h>

#include "eckit/config/LocalConfiguration.h"

#include "Transform.h"


namespace Ingester
{
    /// \brief Convenience class used to create transforms from configuration data.
    class TransformBuilder
    {
     public:
        /// \brief Create transform given the configuration
        static std::shared_ptr<Transform> makeTransform(const eckit::Configuration& conf);

        /// \brief Uses makeTransform to loop through the list of transforms in the configuration
        static Transforms makeTransforms(const eckit::Configuration& conf);
    };
}  // namespace Ingester
