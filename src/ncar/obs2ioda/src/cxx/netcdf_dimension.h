/*
 * (C) Copyright 2026 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef NCAR_OBS2IODA_SRC_CXX_NETCDF_DIMENSION_H_
#define NCAR_OBS2IODA_SRC_CXX_NETCDF_DIMENSION_H_


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
* @param dimID
*     A pointer to an integer that will be set to the ID of the new dimension.
*
* @return
*     - 0 on success.
*     - A non-zero error code if an exception is encountered.
*/
    int netcdfAddDim(
        int netcdfID,
        const char *groupName,
        const char *dimName,
        int len,
        int *dimID);
    }
}  // namespace Obs2Ioda

#endif  // NCAR_OBS2IODA_SRC_CXX_NETCDF_DIMENSION_H_
