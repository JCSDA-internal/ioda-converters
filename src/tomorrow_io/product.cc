/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include "product.h"

#include <exception>
#include <map>
#include <string>

#include "ioda/Engines/HH.h"

namespace tio_converter {

Converter_Params::Converter_Params(const ioda::Group &src, ioda::ObsGroup &dest, int locationOffset)
    : in{src}, out{dest} {
  // Query the input file for the sizes of key dimensions
  this->inNumLocs
    = in.vars["scans"].getDimensions().dimsCur[0] * in.vars["spots"].getDimensions().dimsCur[0];
  this->inNumChans = in.vars["channels"].getDimensions().dimsCur[0];
  this->inNumScans = in.vars["scans"].getDimensions().dimsCur[0];
  this->inNumSpots = in.vars["spots"].getDimensions().dimsCur[0];

  this->locationOffset = locationOffset;
}

ioda::ObsGroup prepare_output_file(TMS_Type typ, int sat_bufr_id, int sat_sub_bufr_id,
                                   int inst_bufr_id, const std::vector<std::string> &input_files,
                                   const std::string &output_file) {
  // Open each of the input files and get the number of locations within the file.
  using namespace ioda;
  const size_t numChans = 12;
  size_t numLocs        = 0;
  for (const auto &input_filename : input_files) {
    Group in = Engines::HH::openFile(input_filename, Engines::BackendOpenModes::Read_Only);
    const size_t inNumScans = in.vars["scans"].getDimensions().dimsCur[0];
    const size_t inNumSpots = in.vars["spots"].getDimensions().dimsCur[0];
    numLocs += inNumScans * inNumSpots;
  }

  // Create the output file.
  // Make a new observation space and add in appropriate dimensions and variables
  NewDimensionScales_t newDims;
  // 81 scan positions per line. 400 lines per file is typical. Each file should have multiple chunks
  // to avoid excessive open / close operations when constructing the ObsGroup.
  const size_t maxNumLocsPerChunk = 10000;
  newDims.push_back(NewDimensionScale<int>(
    "Location", numLocs, numLocs, (numLocs > maxNumLocsPerChunk) ? maxNumLocsPerChunk : numLocs));
  newDims.push_back(NewDimensionScale<int>("Channel", numChans, numChans, numChans));

  Group out_group
    = Engines::HH::createFile(output_file, Engines::BackendCreateModes::Truncate_If_Exists,
                              {Engines::HH::HDF5_Version::V18, Engines::HH::HDF5_Version::Latest});

  ioda::ObsGroup out      = ObsGroup::generate(out_group, newDims);
  ioda::Variable Location = out.vars["Location"];
  ioda::Variable Channel  = out.vars["Channel"];
  create_vars(typ, sat_bufr_id, sat_sub_bufr_id, inst_bufr_id, out, Location, Channel);

  return out;
}

}  // end namespace tio_converter
