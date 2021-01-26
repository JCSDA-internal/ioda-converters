/*
 * (C) Copyright 2021 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <memory>
#include <string>
#include <vector>

#include <Eigen/Dense>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "ioda/ObsGroup.h"
#include "ioda/Engines/HH.h"

#include "oops/util/Logger.h"
#include "oops/util/missingValues.h"

#include "GsiSatBiasReader.h"

// Return ObsGroup with bias coefficients for a given sensor
ioda::ObsGroup makeObsBiasObject(ioda::Group &empty_base_object, const std::string & filename,
                                 const std::string & sensor, const size_t nchannels) {
  // Channels & predictors
  const std::vector<std::string> & predictors = getGsiPredictors(sensor);
  std::vector<int> channels(nchannels);
  long numPreds = predictors.size();
  long numChans = channels.size();

  // Allocate space for bias coefficients and read them from GSI satbias file
  Eigen::ArrayXXf biascoeffs(numPreds, numChans);
  readObsBiasCoefficients(filename, sensor, channels, biascoeffs);

  // Creating dimensions: npredictors & nchannels
  ioda::NewDimensionScales_t newDims;
  newDims.push_back(std::make_shared<ioda::NewDimensionScale<int>>("npredictors",
                    numPreds, numPreds, numPreds));
  newDims.push_back(std::make_shared<ioda::NewDimensionScale<int>>("nchannels",
                    numChans, numChans, numChans));

  // Construct an ObsGroup object, with 2 dimensions npred, nchans
  ioda::ObsGroup ogrp = ioda::ObsGroup::generate(empty_base_object, newDims);

  // Save the predictors and the channels values
  ioda::Variable predsVar = ogrp.vars.create<std::string>("predictors", {numPreds});
  predsVar.write(predictors);
  ioda::Variable chansVar = ogrp.vars.create<int>("channels", {numChans});
  chansVar.write(channels);

  // Set up the creation parameters for the bias coefficients variable
  ioda::VariableCreationParameters float_params;
  float_params.chunk = true;               // allow chunking
  float_params.compressWithGZIP();         // compress using gzip
  float missing_value = util::missingValue(missing_value);
  float_params.setFillValue<float>(missing_value);

  // Create a variable for bias coefficients, save bias coeffs to the variable
  ioda::Variable biasVar = ogrp.vars.createWithScales<float>("bias_coefficients",
                     {ogrp.vars["npredictors"], ogrp.vars["nchannels"]}, float_params);
  biasVar.writeWithEigenRegular(biascoeffs);

  return ogrp;
}

int main(int argc, char** argv) {
  /// Open yaml with configuration for this converter
  ASSERT(argc >= 2);
  eckit::PathName configfile = argv[1];
  eckit::YAMLConfiguration config(configfile);
  const std::string input_filename = config.getString("input file");

  std::vector<std::string> sensors;
  std::vector<int> nchannels;

  /// Find all sensors and number of channels in the GSI bias coefficients file
  findSensorsChannels(input_filename, sensors, nchannels);

  std::cout << "Found " << sensors.size() << " sensors:" << std::endl;
  for (size_t jj = 0; jj < sensors.size(); ++jj) {
    std::cout << "-- " << sensors[jj] << ", " << nchannels[jj] << " channels." << std::endl;
  }

  std::vector<eckit::LocalConfiguration> configs = config.getSubConfigurations("output");
  for (size_t jj = 0; jj < configs.size(); ++jj) {
    const std::string sensor = configs[jj].getString("sensor");
    const std::string output_filename = configs[jj].getString("output file");
    auto it = find(sensors.begin(), sensors.end(), sensor);
    if (it != sensors.end()) {
      int index = it - sensors.begin();
      ioda::Group group = ioda::Engines::HH::createFile(output_filename,
                          ioda::Engines::BackendCreateModes::Truncate_If_Exists);
      makeObsBiasObject(group, input_filename, sensor, nchannels[index]);
    } else {
      const std::string error = "No " + sensor + " sensor in the input file";
      oops::Log::error() << error << std::endl;
    }
  }
  return 0;
}
