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
        if (not fileName.empty()) {
            message << " File: " << fileName;
            if (lineNumber > 0) {
                message << " Line: " << lineNumber;
            }
        }
        message << std::endl << "Message: " << e.what() << std::endl;
        std::cerr << message.str();
        return e.errorCode();
    }
}
