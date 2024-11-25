/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include <exception>
#include <iostream>
#include <string>

#include "ioda/Engines/HH.h"
#include "ioda/ObsGroup.h"
#include "ioda/Units.h"
#include "product.h"

void doHelp() {
  using namespace std;
  cout << "Usage: convert_tio_tms.x input_file_1 [input_file_2 ...] output_file\n";
  exit(1);
}

int main(int argc, char **argv) {
  using namespace std;
  using namespace ioda;
  using namespace tio_converter;
  try {
    if (argc < 3) doHelp();
    vector<string> input_files;
    for (int i = 1; i + 1 < argc; ++i) input_files.push_back(argv[i]);
    if (!input_files.size()) doHelp();

    TMS_Type typ;
    int sat_bufr_id, sat_sub_bufr_id, inst_bufr_id;
    detect_tms(input_files[0], typ, sat_bufr_id, sat_sub_bufr_id, inst_bufr_id);

    string sOutFile(argv[argc - 1]);

    ioda::ObsGroup out
      = prepare_output_file(typ, sat_bufr_id, sat_sub_bufr_id, inst_bufr_id, input_files, sOutFile);
    size_t location_start = 0;
    for (const auto &input_file : input_files) {
      Group in = Engines::HH::openFile(input_file, Engines::BackendOpenModes::Read_Only);
      Converter_Params cp(in, out, location_start);
      convert(typ, cp);
      location_start += cp.inNumLocs;
    }
    return 0;
  } catch (const std::exception &e) {
    std::cerr << e.what() << endl;
    return 1;
  }
}
