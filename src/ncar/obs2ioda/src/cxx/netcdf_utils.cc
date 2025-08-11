#include "netcdf_utils.h"

std::shared_ptr<netCDF::NcGroup> Obs2Ioda::setNetcdfGroup(
        const std::shared_ptr<netCDF::NcFile> &file,
        const char *groupName
) {
    if (groupName == nullptr) {
        std::string msg = "Group name cannot be null";
        throw netCDF::exceptions::NcBadGroupId(
                msg.c_str(), __FILE__, __LINE__);
    }
    if (std::string(groupName).empty()) {
        return file;
    }
    auto group = file->getGroup(groupName);
    if (group.isNull()) {
        std::string msg = "Group '" + std::string(groupName) + "' does not exist in file.";
        throw netCDF::exceptions::NcBadGroupId(
                msg.c_str(), __FILE__, __LINE__);
    }
    return std::make_shared<netCDF::NcGroup>(group);
}