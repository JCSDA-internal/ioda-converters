/*
 *  * (C) Copyright 2023 NOAA/NWS/NCEP/EMC
 *   *
 *    * This software is licensed under the terms of the Apache Licence Version 2.0
 *     * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *      */

#include "GsiAircraftBiasReader.h"

#include <string>
#include <vector>
#include <fstream>
#include <iostream>

#include <Eigen/Dense>

std::vector<std::string> findTailIds(const std::string& filename) {
    std::vector<std::string> tailIds;

    // Open the input file
    std::ifstream fin(filename);

    if (!fin.is_open()) {
        std::cout << "Could not open the file." << std::endl;
        return tailIds;
    }

    std::string line;
    while (std::getline(fin, line)) {
        std::istringstream ss(line);
        std::string value;

        // Read the first column value (using default white space delimiter)
        if (ss >> value) {
            tailIds.push_back(value);
        }
    }

    fin.close();

    return tailIds;
}

//---------------------------------------------------------------------------------------
void readObsBiasCoefficients(const std::string &filename, Eigen::ArrayXXf &coeffs) {
  std::ifstream infile(filename);

  if (!infile.is_open()) {
    std::cerr << "Error: Unable to open file." << std::endl;
    return;
  }

  // Grab total rows of file
  std::size_t nrows = coeffs.rows();

  // Define vars and loop through rows
  std::string tailIds;
  int ich;
  int datetime;
  for (std::size_t row = 0; row < nrows; ++row) {
    infile >> tailIds;  // Skip the first two columns
    infile >> ich;
    for (std::size_t col = 0; col < 9; ++col) {
      infile >> coeffs(row, col);  // Read data from columns 3 to 11
    }
    infile >> datetime;
  }

  infile.close();
}
