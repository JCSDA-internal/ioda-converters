/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <iostream>
#include <memory>

#include "IngesterData.h"


namespace Ingester
{
    bool IngesterData::hasKey(const std::string& fieldName) const
    {
        bool hasKey = false;
        if (dataMap_.find(fieldName) != dataMap_.end())
        {
            hasKey = true;
        }

        return hasKey;
    }
}  // namespace Ingester
