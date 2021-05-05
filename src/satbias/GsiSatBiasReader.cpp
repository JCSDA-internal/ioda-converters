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
  for (size_t ii = 0; ii < gsi_npredictors; ++ii) {
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
    infile.close();
  }
}

//---------------------------------------------------------------------------------------
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
        for (size_t jpred = 0; jpred < gsi_npredictors; ++jpred) {
          infile >> par;
          coeffs(jpred, jchan) = par;
          channels[jchan] = nuchan;
        }
        jchan++;
      } else {
        /// not interested in this channel; passing
        for (size_t jpred = 0; jpred < gsi_npredictors; ++jpred) {
          infile >> par;
        }
      }
    }
    infile.close();
  }
}

//---------------------------------------------------------------------------------------
void readObsBiasCoeffErrors(const std::string & filename, const std::string & sensor,
                           std::vector<int> & channels, Eigen::ArrayXXf & errs,
                           Eigen::ArrayXf & nobs) {
  std::ifstream infile(filename);

  std::size_t ich;     //  sequential number
  std::string nusis;   //  sensor/instrument/satellite
  std::size_t nuchan;  //  channel number

  if (infile.is_open())
  {
    float par;
    size_t jchan = 0;
    while (infile >> ich)
    {
      infile >> nusis;
      infile >> nuchan;
      infile >> par;
      if (nusis == sensor) {
        nobs(jchan) = par;
        /// it's the sensor we're interested in; read in coefficients and channel
        /// indices
        for (size_t jpred = 0; jpred < gsi_npredictors; ++jpred) {
          infile >> par;
          errs(jpred, jchan) = par;
          channels[jchan] = nuchan;
        }
        jchan++;
      } else {
        /// not interested in this channel; passing
        for (size_t jpred = 0; jpred < gsi_npredictors; ++jpred) {
          infile >> par;
        }
      }
    }
    infile.close();
  }
}
