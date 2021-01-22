/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory>
#include <vector>

#include "IngesterTypes.h"

namespace Ingester
{
    /// \brief Base class for Transforms which are used to transform data. Transforms are useful
    ///        for getting data into the right units (for example you can convert Kelvin to Celsius)
    class Transform
    {
     public:
        ~Transform() = default;

        /// \brief Apply transform to the given data.
        virtual void apply(IngesterArray& array) = 0;
    };

    typedef std::vector <std::shared_ptr<Transform>> Transforms;
}  // namespace Ingester
