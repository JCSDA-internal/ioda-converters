/*
 * (C) Copyright 2026 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#ifndef NCAR_OBS2IODA_SRC_CXX_NETCDF_UTILS_H_
#define NCAR_OBS2IODA_SRC_CXX_NETCDF_UTILS_H_
#include <ncGroup.h>
#include <ncFile.h>
#include <memory>


namespace Obs2Ioda {
    std::shared_ptr<netCDF::NcGroup>
    setNetcdfGroup(
            const std::shared_ptr<netCDF::NcFile> &file,
            const char *groupName);
}

#endif  // NCAR_OBS2IODA_SRC_CXX_NETCDF_UTILS_H_
