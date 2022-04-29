/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <unordered_map>
#include <string>
#include <vector>
#include <memory>

#include "DataObject.h"
#include "Eigen/Dense"

namespace Ingester
{
    typedef Eigen::Array<float, Eigen::Dynamic, 1, Eigen::RowMajor> RawData;
    typedef std::unordered_map<std::string, std::shared_ptr<DataObjectBase> > BufrDataMap;
}
