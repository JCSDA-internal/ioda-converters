/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory>
#include <vector>

#include "DataObject.h"

namespace Ingester
{
    /// \brief Base class of all transform classes. Classes are used to transform data.
    class Transform
    {
     public:
        virtual ~Transform() = default;

        /// \brief Modify data according to the rules of the transform.
        /// \param array Array of data to modify.
        virtual void apply(std::shared_ptr<DataObjectBase>& dataObject) = 0;
    };

    typedef std::vector <std::shared_ptr<Transform>> Transforms;
}  // namespace Ingester
