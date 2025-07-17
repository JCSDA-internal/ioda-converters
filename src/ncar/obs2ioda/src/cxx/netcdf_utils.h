#ifndef OBS2IODA_NETCDF_UTILS_H
#define OBS2IODA_NETCDF_UTILS_H
#include <ncGroup.h>
#include <ncFile.h>


namespace Obs2Ioda {
    std::shared_ptr<netCDF::NcGroup>
    setNetcdfGroup(
            const std::shared_ptr<netCDF::NcFile> &file,
            const char *groupName
    );
}

#endif //OBS2IODA_NETCDF_UTILS_H
