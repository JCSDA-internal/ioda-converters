/*
 *  * (C) Copyright 2021 UCAR
 *   *
 *    * This software is licensed under the terms of the Apache Licence Version 2.0
 *     * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *      */

#pragma once

#include <string>
#include <vector>
#include <fstream>

#include <Eigen/Dense>


void findTailIds(const std::string& filename);


void ReadObsBiasCoefficients(const std::string& inputFilePath, std::vector<std::vector<std::string>>& biascoeffs); 
