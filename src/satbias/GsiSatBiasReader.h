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

/// GSI predictor names
namespace gsi_predictors {
  /// Default predictors; some predictors are different for
  /// goessndr .or. goes_img .or. ahi .or. seviri .or. ssmi .or. ssmis .or. gmi .or. abi
  const std::vector<std::string> default_predictors = {"constant",
                                                       "zenith_angle",
                                                       "cloud_liquid_water",
                                                       "lapse_rate_order_2",
                                                       "lapse_rate",
                                                       "cosine_of_latitude_times_orbit_node",
                                                       "sine_of_latitude",
                                                       "emissivity",
                                                       "scan_angle_order_4",
                                                       "scan_angle_order_3",
                                                       "scan_angle_order_2",
                                                       "scan_angle"
                                                      };
  const size_t npredictors = default_predictors.size();
}  // namespace gsi_predictors

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

#
