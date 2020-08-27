/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <map>
#include <string>
#include <vector>

#include "Eigen/Dense"

#include "IngesterTypes.h"


namespace Ingester
{
    class IngesterData
    {
     public:
        IngesterData() = default;

        void add(const std::string& fieldName, const IngesterArray& data);
        IngesterArray get(const std::string& fieldName);
        size_t size() const;

     private:
        IngesterArrayMap dataMap_;
    };
}  // namespace Ingester
