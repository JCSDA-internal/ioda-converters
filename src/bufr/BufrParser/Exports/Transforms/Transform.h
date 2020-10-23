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
    class Transform
    {
    public:
        ~Transform() = default;
        virtual void apply(IngesterArray& array) = 0;
    };

    typedef std::vector <std::shared_ptr<Transform>> Transforms;
}  // namespace Ingester
