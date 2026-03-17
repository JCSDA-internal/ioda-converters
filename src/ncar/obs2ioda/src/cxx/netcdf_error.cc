/*
 * (C) Copyright 2026 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <sstream>
#include "netcdf_error.h"

namespace Obs2Ioda {

    int netcdfErrorMessage(
            const netCDF::exceptions::NcException &e,
            int lineNumber,
            const std::string& fileName
    ) {
        std::stringstream message;
        message << "NetCDF Error: Code: " << e.errorCode();
        if (!fileName.empty()) {
            message << " File: " << fileName;
            if (lineNumber > 0) {
                message << " Line: " << lineNumber;
            }
        }
        message << std::endl << "Message: " << e.what() << std::endl;
        std::cerr << message.str();
        return e.errorCode() == 0 ? -1 : e.errorCode();
    }
}  // namespace Obs2Ioda
