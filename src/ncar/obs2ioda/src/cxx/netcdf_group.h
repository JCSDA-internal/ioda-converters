/*
 * (C) Copyright 2026 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef NCAR_OBS2IODA_SRC_CXX_NETCDF_GROUP_H_
#define NCAR_OBS2IODA_SRC_CXX_NETCDF_GROUP_H_

namespace Obs2Ioda {

    extern "C" {
/**
 * @brief Adds a new group to a NetCDF file, under a specified parent group.
 *
 * This function provides an interface for adding a new group to a NetCDF file
 * using its unique identifier (`netcdfID`)
 *
 * @param netcdfID
 *     The unique identifier for the NetCDF file, used to retrieve the associated file
 *     object from the internal file map.
 *  @param parentGroupName
 *      The name of the parent group under which the new group will be added.
 *      - If `parentGroupName` is `nullptr`, the root group of the file (represented by
 *        the `netCDF::NcFile` object) is used as the parent group.
 *      - If `parentGroupName` specifies the name of an existing group, that group will
 *        be used as the parent.
 *      - If the specified parent group does not exist, a `netCDF::exceptions::NcNullGrp`
 *        exception is raised, and the function returns `-1`.
 * @param groupName
 *     The name of the new group to be created within the specified parent group.
 *
 * @return
 *     - 0 on success.
 *     - A non-zero error code if an exception is encountered (e.g., invalid group name,
 *       parent group not found, or other NetCDF-related errors).
 */
        int netcdfAddGroup(
                int netcdfID,
                const char *parentGroupName,
                const char *groupName);
    }
}  // namespace Obs2Ioda

#endif  // NCAR_OBS2IODA_SRC_CXX_NETCDF_GROUP_H_
