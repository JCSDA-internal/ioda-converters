/*
 * (C) Copyright 2021 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "GsiSatBiasReader.h"

#include <string>
#include <vector>
#include <fstream>

#include <Eigen/Dense>

/// GSI predictor names
/// Default predictors; some sensors use different predictors, see scanpos_predictors
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

/// Predictors for
/// goessndr .or. goes_img .or. ahi .or. seviri .or. ssmi .or. ssmis .or. gmi .or. abi
/// (scan position instead of scan angle)
const std::vector<std::string> scanpos_predictors = {"constant",
                                                     "zenith_angle",
                                                     "cloud_liquid_water",
                                                     "lapse_rate_order_2",
                                                     "lapse_rate",
                                                     "cosine_of_latitude_times_orbit_node",
                                                     "sine_of_latitude",
                                                     "emissivity",
                                                     "scan_position_order_4",
                                                     "scan_position_order_3",
                                                     "scan_position_order_2",
                                                     "scan_position"
                                                    };
const size_t npredictors = default_predictors.size();

/// Returns predictors for this sensor (either default_predictors or scanpos_predictors)
const std::vector<std::string> & getGsiPredictors(const std::string & sensor) {
  bool is_sndr   = (sensor.find("sndr") != std::string::npos);
  bool is_imgr   = (sensor.find("imgr") != std::string::npos);
  bool is_ahi    = (sensor.find("ahi") != std::string::npos);
  bool is_seviri = (sensor.find("seviri") != std::string::npos);
  bool is_ssmi   = (sensor.find("ssmi") != std::string::npos);
  bool is_gmi    = (sensor.find("gmi") != std::string::npos);
  bool is_abi    = (sensor.find("abi") != std::string::npos);
  if (is_sndr || is_imgr || is_ahi || is_seviri || is_ssmi || is_gmi || is_abi) {
    return scanpos_predictors;
  } else {
    return default_predictors;
  }
}

/// Read info for one channel from the GSI text file
/// Returns sensor (instrument + satellite) name and channel index
void readOneChannel(std::ifstream & infile, std::string & sensor, size_t & channel) {
  float tlap, tsum;
  std::size_t ntlapupdate;
  float par;
  infile >> sensor;
  infile >> channel;
  infile >> tlap;
  infile >> tsum;
  infile >> ntlapupdate;
  for (size_t ii = 0; ii < npredictors; ++ii) {
    infile >> par;
  }
}

/// Finds all sensors in the file (returned in \p sensors) and number of channels
/// for all the sensors (returned in \p nchannels)
void findSensorsChannels(const std::string & filename, std::vector<std::string> & sensors,
                         std::vector<int> & nchannels) {
  std::ifstream infile(filename);

  std::size_t ich;     //  sequential number
  std::string sensor;  //  sensor/instrument/satellite
  std::size_t channel;  //  channel number

  if (infile.is_open())
  {
    /// Read the first entry, save first sensor
    infile >> ich;
    readOneChannel(infile, sensor, channel);
    sensors.push_back(sensor);
    size_t nsensors = 0;
    nchannels.push_back(1);

    while (infile >> ich)
    {
      readOneChannel(infile, sensor, channel);
      if (sensor == sensors[nsensors]) {
        // still processing the same sensor, just update the number of channels
        nchannels[nsensors]++;
      } else {
        // new sensor: move on
        sensors.push_back(sensor);
        nsensors++;
        nchannels.push_back(1);
      }
    }
  }
  infile.close();
}

/// Read bias coefficients for the \p sensor instrument+satellite in \p filename GSI coeff file
/// Returns coefficients in \p coeffs and channel indices in \p channels
void readObsBiasCoefficients(const std::string & filename, const std::string & sensor,
                             std::vector<int> & channels, Eigen::ArrayXXf & coeffs ) {
  std::ifstream infile(filename);

  std::size_t ich;     //  sequential number
  std::string nusis;   //  sensor/instrument/satellite
  std::size_t nuchan;  //  channel number
  float tlap, tsum;
  std::size_t ntlapupdate;

  if (infile.is_open())
  {
    float par;
    size_t jchan = 0;
    while (infile >> ich)
    {
      infile >> nusis;
      infile >> nuchan;
      infile >> tlap;
      infile >> tsum;
      infile >> ntlapupdate;
      if (nusis == sensor) {
        /// it's the sensor we're interested in; read in coefficients and channel
        /// indices
        for (size_t jpred = 0; jpred < npredictors; ++jpred) {
          infile >> par;
          coeffs(jpred, jchan) = par;
          channels[jchan] = nuchan;
        }
        jchan++;
      } else {
        /// not interested in this channel; passing
        for (size_t jpred = 0; jpred < npredictors; ++jpred) {
          infile >> par;
        }
      }
    }
  }
  infile.close();
}
