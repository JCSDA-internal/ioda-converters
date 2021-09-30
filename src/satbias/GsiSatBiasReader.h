/*
 * (C) Copyright 2021 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>
#include <fstream>

#include <Eigen/Dense>

/// Number of predictors in GSI satbias file
constexpr size_t gsi_npredictors = 12;

/// Finds all sensors in the file (returned in \p sensors) and number of channels
/// for all the sensors (returned in \p nchannels)
void findSensorsChannels(const std::string & filename, std::vector<std::string> & sensors,
                         std::vector<int> & nchannels);

/// Read bias coefficients from the GSI bias coefficients file (satbias_in)
/// \param filename file with bias coefficients (GSI style)
/// \param sensor instrument+satellite that is to be read from the file
/// \param(out) channels channels for \p sensor
/// \param(out) coeffs bias coefficients
void readObsBiasCoefficients(const std::string & filename, const std::string & sensor,
                             std::vector<int> & channels, Eigen::ArrayXXf & coeffs);

/// Read bias coefficients errors from the GSI file (satbias_pc)
/// \param filename file with bias coefficients (GSI style)
/// \param sensor instrument+satellite that is to be read from the file
/// \param(out) channels channels for \p sensor
/// \param(out) errs bias coefficients error variances
/// \param(out) nobs number of observations
void readObsBiasCoeffErrors(const std::string & filename, const std::string & sensor,
                           std::vector<int> & channels, Eigen::ArrayXXf & errs,
                           Eigen::ArrayXf & nobs);

