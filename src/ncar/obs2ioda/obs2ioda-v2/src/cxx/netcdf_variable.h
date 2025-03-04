#ifndef NETCDF_VARIABLE_H
#define NETCDF_VARIABLE_H
#include <netcdf>

namespace Obs2Ioda {

    extern "C" {
    /**
     * @brief Adds a variable to a NetCDF file.
     *
     * @param netcdfID The identifier of the NetCDF file where the variable will be added.
     * @param groupName The name of the group in which the variable should be created.
     *                  If nullptr, the variable is added to the root group.
     * @param varName The name of the variable to be created.
     * @param netcdfDataType The NetCDF data type of the variable (e.g., NC_INT, NC_FLOAT).
     * @param numDims The number of dimensions associated with the variable.
     * @param dimNames An array of dimension names specifying the shape of the variable.
     * @return int A status code indicating the outcome of the operation:
     *         - 0: Success.
     *         - Non-zero: Failure, with an error message logged.
     */
    int netcdfAddVar(
        int netcdfID,
        const char *groupName,
        const char *varName,
        nc_type netcdfDataType,
        int numDims,
        const char **dimNames
    );

    /**
    * @brief Writes data to a variable in a NetCDF file.
    *
    * @param netcdfID The identifier of the NetCDF file where the data will be written.
    * @param groupName The name of the group containing the variable. If nullptr, the variable is assumed to be in the root group.
    * @param varName The name of the variable to which data will be written.
    * @param values A pointer to the data to be written to the variable.
    * @return int A status code indicating the outcome of the operation:
    *         - 0: Success.
    *         - Non-zero: Failure, with an error message logged.
    */
    int netcdfPutVarInt(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const int *values
    );

    int netcdfPutVarInt64(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const long long *values
    );

    int netcdfPutVarReal(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const float *values
    );

    int netcdfPutVarDouble(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const double *values
    );

    int netcdfPutVarString(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const char **values
    );

    int netcdfPutVarChar(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const char **values
    );

    /**
    * @brief Sets the fill mode and fill value for a variable in a NetCDF file.
    *
    * @param netcdfID The identifier of the NetCDF file containing the variable.
    * @param groupName The name of the group containing the variable. If nullptr, the variable is assumed to be in the root group.
    * @param varName The name of the variable for which the fill mode is set.
    * @param fillMode The fill mode to be applied:
    *         - 0: Disable fill mode (use uninitialized values).
    *         - 1: Enable fill mode (use the specified fill value).
    * @param fillValue The fill value to be applied when fill mode is enabled. Must match the data type of the variable.
    * @return int A status code indicating the outcome of the operation:
    *         - 0: Success.
    *         - Non-zero: Failure, with an error message logged.
    */
    int netcdfSetFillInt(
        int netcdfID,
        const char *groupName,
        const char *varName,
        int fillMode,
        int fillValue
    );

    int netcdfSetFillReal(
        int netcdfID,
        const char *groupName,
        const char *varName,
        int fillMode,
        float fillValue
    );

    int netcdfSetFillInt64(
        int netcdfID,
        const char *groupName,
        const char *varName,
        int fillMode,
        long long fillValue
    );

    int netcdfSetFillString(
        int netcdfID,
        const char *groupName,
        const char *varName,
        int fillMode,
        const char *fillValue
    );
    }
}

#endif //NETCDF_VARIABLE_H
