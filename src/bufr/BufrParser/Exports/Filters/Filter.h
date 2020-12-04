/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#pragma once

#include "IngesterTypes.h"


namespace Ingester
{
    class Filter
    {
        virtual void apply(IngesterArray& array) = 0;
    };
}  // namespace Ingester
