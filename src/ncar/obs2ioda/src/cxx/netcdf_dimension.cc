/*
 * (C) Copyright 2026 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "netcdf_utils.h"
#include "netcdf_dimension.h"
#include "netcdf_file.h"
#include "netcdf_error.h"

namespace Obs2Ioda {
    int netcdfAddDim(
        const int netcdfID,
        const char *groupName,
        const char *dimName,
        const int len,
        int *dimID
    ) {
        try {
            const auto file = FileMap::getInstance().getFile(netcdfID);
            const auto group = setNetcdfGroup(file, groupName);
            auto iodaDimName = iodaSchema.getDimension(dimName)->getValidName();
            auto dim = group->addDim(iodaDimName, len);
            *dimID = dim.getId();
            return 0;
        } catch (netCDF::exceptions::NcException &e) {
            return netcdfErrorMessage(e, __LINE__, __FILE__);
        }
    }
}  // namespace Obs2Ioda
