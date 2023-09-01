/*
 *  * (C) Copyright 2021 UCAR
 *   *
 *    * This software is licensed under the terms of the Apache Licence Version 2.0
 *     * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 *      */

#include "GsiSatBiasReader.h"

#include <string>
#include <vector>
#include <fstream>

#include <Eigen/Dense>

std::vector<std::string> findTailIds(const std::string& filename) {
    std::vector<std::string> tailIds;

    // Open the input file
    std::ifstream fin(filename);

    if (!fin.is_open()) {
        std::cerr << "Could not open the file." << std::endl;
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
void ReadObsBiasCoefficients(const std::string& inputFilePath, std::vector<std::vector<std::string>>& biascoeffs) {
    // Open the input file
    std::ifstream fin(inputFilePath);

    if (!fin.is_open()) {
        std::cerr << "Could not open the input file." << std::endl;
        return;
    }

    std::string line;
    while (std::getline(fin, line)) {
        std::istringstream ss(line);
        std::string value;
        int col = 1;

        std::vector<std::string> rowData;
        while (ss >> value) {
            if (col >= 3 && col <= 11) {
                rowData.push_back(value);
            }
            ++col;
        }

        extractedData.push_back(rowData);
    }

    fin.close();
}
