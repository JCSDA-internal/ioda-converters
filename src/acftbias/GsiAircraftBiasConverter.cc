/*
 *  * (C) Copyright 2021 UCAR
 *   *
 *    * This software is licensed under the terms of the Apache Licence Version 2.0
 *     * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *      */

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

oda::ObsGroup make ObsBiasObject(ioda::Group &empty_base_object,
                                  const std::string & coeffile,
                                  const std::string & tailIds,
                                  const std::vector<std::string> & predictors) {
  /// Predictors
  long numPreds = predictors.size();
  long numIds = tailIds.size();

  /// Creating dimensions: n
  ioda::NewDimensionScales_t newDims {
      ioda::NewDimensionScale<int>("nvars", 1),
      ioda::NewDimensionScale<int>("ntailids", numIds)
  };
  
  /// Construct an ObsGroup object, with 2 dimensions npred, nobs
  ioda::ObsGroup ogrp = ioda::ObsGroup::generate(empty_base_object, newDims)
    
    
  /// Create 2D bias coefficient variable
  Eigen::ArrayXXf biascoeffs(predictor.size(), numIds);

  ReadObsBiasCoefficients(coeffile, biascoeffs);
    
  for (size_t i = 0; i < numPreds; ++i) {
    /// Set up the creation parameters for the bias coefficients variable
    ioda::VariableCreationParameters float_params;
    float_params.chunk = true;               // allow chunking
    float_params.compressWithGZIP();         // compress using gzip
    float missing_value = util::missingValue(missing_value);
    float_params.setFillValue<float>(missing_value);
    
    // Access first predictor and
    const std::vector<std::string>& subVar = biascoeffs[i];
      
    // Create a variable for bias coefficients, save bias coeffs to the variable
    ioda::Variable biasVar = ogrp.vars.createWithScales<float>(predictors[i],
                       {ogrp.vars["nvars"], ogrp.vars["ntailids"]}, float_params);
    biasVar.writeWithEigenRegular(subVar);
    
  return ogrp;
}


int main(int argc, char** argv) {
  /// Open yaml with configuration for this converter
  ASSERT(argc >= 2);
  eckit::PathName configfile = argv[1];
  eckit::YAMLConfiguration config(configfile);
  
  /// Grab input coeff file
  const std::string coeffile = config.getString("input coeff file");

  /// Use function to grab tail IDs from input coeff file
  std::vector<std::string> tailIds = findTailIds(coeffile);

  std::cout << "Found " << tailIds.size() << " tail IDs." << std::endl;

  /// Read from config file "output"
  std::vector<eckit::LocalConfiguration> configs = config.getSubConfigurations("output");

  const std::string output_filename = configs[0].getString("output file");
  const std::vector<std::string> predictors = configs[0].getStringVector("predictors");
  
  /// I am defining this because the other code is not and I dont know where it came from
  const int gsi_npredictors = 9;
  
  /// check if predictors are the same length as what the gsi predictors are
  if (predictors.size() != gsi_npredictors) {
    const std::string error = "Number of predictors specified in yaml must be " +
          std::to_string(gsi_npredictors) + " (same as number of predictors in GSI satinfo)";
    throw eckit::BadValue(error, Here());
  }

  /// No loop necessary, lets just create the ncfile with this function
  ioda::Group group = ioda::Engines::HH::createFile(output_filename,
                      ioda::Engines::BackendCreateModes::Truncate_If_Exists);
  makeObsBiasObject(group, coeffile, tailIds, predictors);

}
