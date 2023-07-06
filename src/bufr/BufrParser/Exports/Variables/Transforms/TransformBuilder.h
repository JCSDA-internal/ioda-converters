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

    /// \brief Convenience class used to make new transforms from config file data.
    class TransformBuilder
    {
     public:
        /// \brief Create a transform for the config data given. Used by makeTransforms.
        /// \param conf ECKit config data for the tranform.
        static std::shared_ptr<Transform> makeTransform(const eckit::Configuration& conf);

        /// \brief Create a transforms for the config data given.
        /// \param conf ECKit config data for the list of transforms.
        static Transforms makeTransforms(const eckit::Configuration& conf);
    };
}  // namespace Ingester
