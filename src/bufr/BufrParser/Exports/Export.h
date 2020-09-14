/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <vector>
#include <string>
#include <memory>

#include "BufrParser/BufrTypes.h"
#include "DataObject/DataObject.h"

namespace Ingester
{
    class Export
    {
     public:
        virtual ~Export() = default;

        virtual std::shared_ptr<DataObject> exportData(BufrDataMap map) = 0;
    };
}  // namespace Ingester


