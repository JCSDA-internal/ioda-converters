/*
 * (C) Copyright 2026 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "netcdf_group.h"
#include "netcdf_file.h"
#include "netcdf_error.h"
#include "netcdf_utils.h"

namespace Obs2Ioda {
    int netcdfAddGroup(
        int netcdfID,
        const char *parentGroupName,
        const char *groupName
    ) {
        try {
            auto file = FileMap::getInstance().getFile(netcdfID);
            const auto parentGroup = setNetcdfGroup(file, parentGroupName);
            auto iodaGroupName = iodaSchema.getGroup(groupName)->getValidName();
            const auto group = parentGroup->addGroup(iodaGroupName);
            return 0;
        } catch (netCDF::exceptions::NcException &e) {
            return netcdfErrorMessage(
                e,
                __LINE__,
                __FILE__);
        }
    }
}  // namespace Obs2Ioda
