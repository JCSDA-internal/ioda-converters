/*
 *  * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
 *   *
 *    * This software is licensed under the terms of the Apache Licence Version 2.0
 *     * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *      */

#pragma once

#include <string>
#include <vector>
#include <fstream>
#include <iostream>

#include <Eigen/Dense>

/// Number of predictors in GSI aircraft bias file
constexpr size_t gsi_npredictors = 3;

/// Find all tail ids in file
std::vector<std::string> findTailIds(const std::string& filename);

/// Read bias coefficients from GSI bias coefficients files
void readObsBiasCoefficients(const std::string& inputFilePath,
                             Eigen::ArrayXXf & biascoeffs);
