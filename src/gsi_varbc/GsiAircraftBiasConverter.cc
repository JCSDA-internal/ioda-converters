/*
 *  * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
 *   *
 *    * This software is licensed under the terms of the Apache Licence Version 2.0
 *     * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *      */

#include <memory>
#include <string>
#include <vector>
#include <iostream>

#include <Eigen/Dense>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "ioda/ObsGroup.h"
#include "ioda/Engines/HH.h"

#include "oops/util/missingValues.h"

#include "GsiAircraftBiasReader.h"

ioda::ObsGroup makeObsBiasObject(ioda::Group &empty_base_object,
                                 const std::string & coeffile,
                                 const std::vector<std::string> & tailIds,
                                 const std::vector<int> & lastCycleUpdated,
                                 const std::vector<std::string> & predictors) {
  /// Predictors
  int numPreds = predictors.size();
  int numIds = tailIds.size();

  /// Creating dimensions: n
  ioda::NewDimensionScales_t newDims {
      ioda::NewDimensionScale<int>("nvars", 1),
      ioda::NewDimensionScale<int>("nrecs", numIds)
  };

  /// Construct an ObsGroup object, with 2 dimensions nrecs, nvars
  ioda::ObsGroup ogrp = ioda::ObsGroup::generate(empty_base_object, newDims);

  /// Create tail IDs and cycles variable
  ioda::Variable tailIdsVar = ogrp.vars.createWithScales<std::string>("records",
                                    {ogrp.vars["nrecs"]});
  tailIdsVar.write(tailIds);

  ioda::Variable lastCycleUpdatedVar = ogrp.vars.createWithScales<int>("datetimes",
                                            {ogrp.vars["nrecs"]});
  lastCycleUpdatedVar.write(lastCycleUpdated);

  /// Create 2D bias coefficient variable
  Eigen::ArrayXXf biascoeffs(numIds, numPreds*3);

  readObsBiasCoefficients(coeffile, biascoeffs);

  for (int i = 0; i < numPreds; ++i) {
    /// Set up the creation parameters for the bias coefficients variable
    ioda::VariableCreationParameters float_params;
    float_params.chunk = true;               // allow chunking
    float_params.compressWithGZIP();         // compress using gzip
    float missing_value = util::missingValue(missing_value);
    float_params.setFillValue<float>(missing_value);

    // Access predictor coefficient values column and create variable
    Eigen::ArrayXXf subVar = biascoeffs.col(i);

    ioda::Variable biasVar = ogrp.vars.createWithScales<float>("biasCoefficients/"+predictors[i],
                       {ogrp.vars["nvars"], ogrp.vars["nrecs"]}, float_params);
    biasVar.writeWithEigenRegular(subVar);

    // Access predictor observation error values column and create variable
    Eigen::ArrayXXf subVarObError = biascoeffs.col(i+3);

    ioda::Variable biasVarObError = ogrp.vars.createWithScales<float>(
                                                "biasCoeffObErrors/"+predictors[i],
                                                {ogrp.vars["nvars"], ogrp.vars["nrecs"]},
                                                float_params);
    biasVarObError.writeWithEigenRegular(subVarObError);

    // Access predictor background error values column and create variable
    Eigen::ArrayXXf subVarBkgError = biascoeffs.col(i+6);

    ioda::Variable biasVarBkgError = ogrp.vars.createWithScales<float>(
                                                "biasCoeffErrors/"+predictors[i],
                                                {ogrp.vars["nvars"], ogrp.vars["nrecs"]},
                                                float_params);
    biasVarBkgError.writeWithEigenRegular(subVarBkgError);
  }

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
  const std::vector<std::string> tailIds = findTailIds(coeffile);

  /// Use function to grab datetimes
  const std::vector<int> lastCycleUpdated = findDatetimes(coeffile);

  /// Read from config file "output"
  std::vector<eckit::LocalConfiguration> configs = config.getSubConfigurations("output");

  const std::string output_filename = configs[0].getString("output file");
  const std::vector<std::string> predictors = configs[0].getStringVector("predictors");

  /// Check if predictors are the same length as what the gsi predictors are
  if (predictors.size() != gsi_npredictors) {
    const std::string error = "Number of predictors specified in yaml must be " +
          std::to_string(gsi_npredictors) +
          " (same as number of predictors in GSI aircraft bias file)";
    throw eckit::BadValue(error, Here());
  }

  /// Create ncfile
  ioda::Group group = ioda::Engines::HH::createFile(output_filename,
                      ioda::Engines::BackendCreateModes::Truncate_If_Exists);
  makeObsBiasObject(group, coeffile, tailIds, lastCycleUpdated, predictors);
}
