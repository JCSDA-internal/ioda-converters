#pragma once
/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include <map>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "ioda/ObsGroup.h"

/// @brief The namespace for the Tomorrow.io TMS converter.
namespace tio_converter {

/// @brief Convenience structure passed to the data copying functions.
struct Converter_Params {
  const ioda::Group &in;  ///< The input file
  ioda::ObsGroup out;     ///< The output file

  ioda::Variable Location;  ///< The output file's Location dimension scale.
  ioda::Variable Channel;   ///< The output file's Channel dimension scale.
  int inNumLocs;            ///< Number of locations in the input file (numScans * numSpots).
  int inNumChans;           ///< Number of channels in the input file.
  int inNumScans;           ///< Number of scan lines in the input file.
  int inNumSpots;           ///< Number of scan spots per line in the input file.
  int locationOffset;       ///< An offset value used when concatenating multiple inputs.

  Converter_Params(const ioda::Group &src, ioda::ObsGroup &dest, int locationOffset = 0);
};

/// @brief Describes whether the input is a Tio TMS instrument or TROPICS.
enum class TMS_Type { TIO, TROPICS };

/// @brief Convenience function to read an input file and infer some basic information.
/// @param input_file is the path to the L1B file to be tested.
/// @param[out] typ indicates whether the data are from Tio or TROPICS.
/// @param[out] sat_bufr_id is the WMO BUFR id for this satellite.
/// @param[out] sat_sub_bufr_id is the sub-satellite id for this satellite.
/// @param[out] inst_bufr_id is the WMO BUFR id for this instrument.
void detect_tms(const std::string &input_file, TMS_Type &typ, int &sat_bufr_id,
                int &sat_sub_bufr_id, int &inst_bufr_id);

/// @brief Create the skeleton of the output IODA file. This will be an ObsGroup
///   with the corrrect number of Locations, plus the correct information regarding
///   satellite / instrument IDs.
/// @param typ indicates whether the data are from Tio or TROPICS.
/// @param sat_bufr_id is the WMO BUFR id for this satellite.
/// @param sat_sub_bufr_id is the sub-satellite id for this satellite.
/// @param inst_bufr_id is the WMO BUFR id for this instrument.
/// @param input_files are the paths of the input L1B files.
/// @param output_file is the path to the output file.
/// @see detect_tms to get the ID fields.
ioda::ObsGroup prepare_output_file(TMS_Type typ, int sat_bufr_id, int sat_sub_bufr_id,
                                   int inst_bufr_id, const std::vector<std::string> &input_files,
                                   const std::string &output_file);

/// @brief Function to copy data from an input file to the destination.
void convert(TMS_Type typ, Converter_Params &);

// Private functions

/// @brief Function called by prepare_output_file to create the variables in an output file.
void create_vars(TMS_Type typ, int sat_bufr_id, int sat_sub_bufr_id, int inst_bufr_id,
                 ioda::ObsGroup &out, ioda::Variable Location, ioda::Variable Channel);
/// @brief Function called by prepare_output_file to fill in more variables in the output file.
void create_sat_inst_specs(TMS_Type typ, int sat_bufr_id, int sat_sub_bufr_id, int inst_bufr_id,
                           ioda::ObsGroup &out, ioda::Variable Location, ioda::Variable Channel,
                           size_t numScans);
/// @brief Function to set datetimes in the IODA file. Called by "convert".
void convert_datetime(TMS_Type typ, Converter_Params &p);

}  // end namespace tio_converter
