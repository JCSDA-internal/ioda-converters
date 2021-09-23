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
#include <memory>

#include "Eigen/Dense"

#include "DataObject/DataObject.h"

namespace Ingester
{
    typedef float FloatType;
    typedef Eigen::Array<FloatType, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> IngesterArray;
    typedef std::map<std::string, std::shared_ptr<DataObject>> BufrDataMap;
}
