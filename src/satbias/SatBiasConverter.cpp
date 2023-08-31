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

#include "oops/util/missingValues.h"

#include "GsiSatBiasReader.h"

// Return ObsGroup with bias coefficients for a given sensor
ioda::ObsGroup makeObsBiasObject(ioda::Group &empty_base_object,
                                 const std::string & coeffile, const std::string & errfile,
                                 const std::string & sensor,
                                 const std::vector<std::string> & predictors,
                                 const size_t nchannels) {
  // Channels & predictors
  std::vector<int> channels(nchannels), channels_in_errorfile(nchannels);
  long numPreds = predictors.size();
  long numChans = channels.size();
  long numRecs = 1;

  // Allocate space for bias coefficients and read them from GSI satbias file
  Eigen::ArrayXXf biascoeffs(numPreds, numChans);
  Eigen::ArrayXXf biascoefferrs(numPreds, numChans);
  Eigen::ArrayXf  nobs(numChans);
  readObsBiasCoefficients(coeffile, sensor, channels, biascoeffs);
  readObsBiasCoeffErrors(errfile, sensor, channels_in_errorfile, biascoefferrs, nobs);
  ASSERT(channels == channels_in_errorfile);

  // Creating dimensions: npredictors & nchannels
  ioda::NewDimensionScales_t newDims {
      ioda::NewDimensionScale<int>("nrecs", numRecs),
      ioda::NewDimensionScale<int>("nvars", numChans)
  };

  // Construct an ObsGroup object, with 2 dimensions nrecs, nvars
  ioda::ObsGroup ogrp = ioda::ObsGroup::generate(empty_base_object, newDims);

  // Save the dimension variable values
  ioda::Variable chansVar = ogrp.vars.createWithScales<int>("channels", {ogrp.vars["nvars"]});
  chansVar.write(channels);
  ioda::Variable recsVar = ogrp.vars.createWithScales<std::string>("records", {ogrp.vars["nrecs"]});

  // Set up the creation parameters for the bias coefficients variable
  ioda::VariableCreationParameters float_params;
  float_params.chunk = true;               // allow chunking
  float_params.compressWithGZIP();         // compress using gzip
  float missing_value = util::missingValue(missing_value);
  float_params.setFillValue<float>(missing_value);

  // Loop over the predictors and create variables for each
  for (int ipred = 0; ipred < numPreds; ipred++) {
    // create and write the bias coeffs
    ioda::Variable biasVar = ogrp.vars.createWithScales<float>("biasCoefficients/"+predictors[ipred],
    //                   {recsVar, chansVar}, float_params);
                        {chansVar}, float_params);
    Eigen::ArrayXf  biascoeff(numChans);
    biascoeff = biascoeffs(ipred);
    biasVar.writeWithEigenRegular(biascoeff);
    // create and write the error variances
    ioda::Variable biaserrVar = ogrp.vars.createWithScales<float>("biasCoeffErrors/"+predictors[ipred],
    //                   {recsVar, chansVar}, float_params);
                        {chansVar}, float_params);
    Eigen::ArrayXf  biascoefferr(numChans);
    biascoefferr = biascoefferrs(ipred);
    biaserrVar.writeWithEigenRegular(biascoefferr);
  }

  // Create a variable for number of obs (used in the bias coeff error covariance)
  ioda::Variable nobsVar = ogrp.vars.createWithScales<float>("number_obs_assimilated",
                     {recsVar, chansVar}, float_params);
  nobsVar.writeWithEigenRegular(nobs);
  return ogrp;
}

int main(int argc, char** argv) {
  /// Open yaml with configuration for this converter
  ASSERT(argc >= 2);
  eckit::PathName configfile = argv[1];
  eckit::YAMLConfiguration config(configfile);
  const std::string coeffile = config.getString("input coeff file");
  const std::string errfile  = config.getString("input err file");

  std::vector<std::string> sensors;
  std::vector<int> nchannels;

  /// Find all sensors and number of channels in the GSI bias coefficients file
  findSensorsChannels(coeffile, sensors, nchannels);

  std::cout << "Found " << sensors.size() << " sensors:" << std::endl;
  for (size_t jj = 0; jj < sensors.size(); ++jj) {
    std::cout << "-- " << sensors[jj] << ", " << nchannels[jj] << " channels." << std::endl;
  }

  std::vector<eckit::LocalConfiguration> configs = config.getSubConfigurations("output");
  for (size_t jj = 0; jj < configs.size(); ++jj) {
    const std::string sensor = configs[jj].getString("sensor");
    const std::string output_filename = configs[jj].getString("output file");
    const std::vector<std::string> predictors = configs[jj].getStringVector("predictors");
    if (predictors.size() != gsi_npredictors) {
      const std::string error = "Number of predictors specified in yaml must be " +
            std::to_string(gsi_npredictors) + " (same as number of predictors in GSI satinfo)";
      throw eckit::BadValue(error, Here());
    }
    auto it = find(sensors.begin(), sensors.end(), sensor);
    if (it != sensors.end()) {
      int index = it - sensors.begin();
      ioda::Group group = ioda::Engines::HH::createFile(output_filename,
                          ioda::Engines::BackendCreateModes::Truncate_If_Exists);
      makeObsBiasObject(group, coeffile, errfile, sensor, predictors, nchannels[index]);
    } else {
      const std::string error = "No " + sensor + " sensor in the input file";
      throw eckit::BadValue(error, Here());
    }
  }
  return 0;
}
