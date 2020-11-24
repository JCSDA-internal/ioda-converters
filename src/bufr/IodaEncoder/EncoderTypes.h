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

namespace IodaEncoder
{
    typedef float FloatType;
    typedef Eigen::Array<FloatType, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor> EncoderArray;
    typedef std::map<std::string, EncoderArray> EncoderArrayMap;
    typedef std::vector<std::string> EncoderStrVector;
    typedef std::map<std::string, EncoderStrVector> EncoderStringMap;
}
