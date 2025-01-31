#ifndef NETCDF_DIMENSION_H
#define NETCDF_DIMENSION_H


namespace Obs2Ioda {
    extern "C" {
    /**
* @brief Adds a new dimension to a NetCDF file.
*
* This function adds a dimension to a NetCDF file, supporting both global dimensions
* and dimensions within a specific group.
*
* @param netcdfID
*     The unique identifier for the NetCDF file. This ID is used to retrieve the
*     corresponding file object from the internal file map.
* @param groupName
*     A null-terminated string specifying the name of the group in which the dimension
*     will be created. If `NULL`, the dimension will be added to the root group.
* @param dimName
*     A null-terminated string specifying the name of the new dimension. The name must
*     be unique within the target group.
* @param len
*     The length of the dimension.
*
* @return
*     - 0 on success.
*     - A non-zero error code if an exception is encountered.
*/
    int netcdfAddDim(
        int netcdfID,
        const char *groupName,
        const char *dimName,
        int len
    );
    }
}

#endif //NETCDF_DIMENSION_H
