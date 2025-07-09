#ifndef OBS2IODA_NETCDF_ERROR_H
#define OBS2IODA_NETCDF_ERROR_H


#include <netcdf>

namespace Obs2Ioda {

/**
 * @brief Logs detailed information about a NetCDF exception.
 *
 * This function captures and logs detailed error information from a
 * NetCDF exception, including the error code, file name, line number,
 * and error message. The error message is output to the standard error
 * stream, and the NetCDF error code is returned.
 *
 * @param e The NetCDF exception to log, of type `netCDF::exceptions::NcException`.
 * @param lineNumber The line number in the source file where the exception occurred.
 *                   Defaults to -1 if not provided.
 * @param fileName The name of the source file where the exception occurred.
 *                 Defaults to an empty string if not provided.
 * @return The error code from the NetCDF exception. If the error code is 0,
 * which is the case for NcNullGrp, NcNullDim, and NcNullType exceptions,
 * -1 is returned instead.
 *
 * @note If `fileName` is empty, the file name and line number are not included in the log.
 *       If `lineNumber` is less than or equal to 0, it is ignored in the log.
 *
 * @example
 * @code
 * try {
 *     std::vector<size_t> data = {1, 2, 3, 4, 5};
 *     std::vector<size_t> index = {0};
 *     auto var = file.getVar("foo");
 *     var.putVar(index, data.data());
 * }
 * catch (const netCDF::exceptions::NcException &e) {
 *     Obs2Ioda::netcdfErrorMessage(e, __LINE__, __FILE__);
 * }
 * @endcode
 */
    int netcdfErrorMessage(
            const netCDF::exceptions::NcException &e,
            int lineNumber = -1,
            const std::string& fileName = ""
    );

}

#endif //OBS2IODA_NETCDF_ERROR_H
