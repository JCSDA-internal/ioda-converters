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

namespace Ingester
{
    typedef Eigen::Array<double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> IngesterArray;
    typedef std::map<std::string, IngesterArray> IngesterArrayMap;
    typedef std::vector<std::string> IngesterStrVector;
    typedef std::map<std::string, IngesterStrVector> IngesterStringMap;
}
