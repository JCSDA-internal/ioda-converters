/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include <exception>
#include <map>
#include <string>

#include "ioda/Engines/HH.h"
#include "ioda/Group.h"
#include "product.h"
namespace tio_converter {

void detect_tms(const std::string &input_file, TMS_Type &typ, int &sat_bufr_id,
                int &sat_sub_bufr_id, int &inst_bufr_id) {
  using namespace ioda;
  using namespace std;
  typ             = TMS_Type::TIO;
  sat_bufr_id     = 999;
  sat_sub_bufr_id = 999;
  inst_bufr_id    = 999;

  // Query the input file to get the satellite identifier.
  Group in = Engines::HH::openFile(input_file, Engines::BackendOpenModes::Read_Only);

  if (in.atts.exists("Source")) {
    string source_sat = in.atts["Source"].read<string>();
    if (source_sat.substr(0, 7) == "TROPICS") {
      typ          = TMS_Type::TROPICS;
      inst_bufr_id = 433;  // See https://codes.ecmwf.int/odb/satelliteinstrument/.
      const map<string, int> tropics_ids = {{"TROPICS01", 709},
                                            {"TROPICS03", 228},
                                            {"TROPICS05", 263},
                                            {"TROPICS06", 264},
                                            {"TROPICS07", 284}};
      if (tropics_ids.count(source_sat)) sat_bufr_id = tropics_ids.at(source_sat);
    }
  } else if (in.atts.exists("Filename")) {
    string source_filename = in.atts["Filename"].read<string>();
    if (source_filename.substr(0, 3) == "TMS") {
      // NOTE (RH): WMO BUFR values are not yet assigned. Placeholder values
      // are used instead. These values were conveyed to Ben Ruston by Jeff Ator,
      // and are consistent with usage in the JCSDA converter.
      sat_bufr_id  = 769;   // Placeholder!
      inst_bufr_id = 1100;  // Purely a placeholder value.
      // TMS-S02 is the fourth T.io satellite and the only one used with this converter
      // so far. Expect code updates once more instruments launch.
      sat_sub_bufr_id = 4;  // Placeholder!
    }
  } else {
    throw domain_error("Input file is not a TROPICS or TMS L1B data file.");
  }
}

}  // namespace tio_converter
