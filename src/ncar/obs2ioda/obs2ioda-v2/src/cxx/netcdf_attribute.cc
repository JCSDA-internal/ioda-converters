#include "netcdf_attribute.h"
#include "netcdf_file.h"
#include "netcdf_error.h"

namespace Obs2Ioda {
    template<typename T> int netcdfPutAtt(
        int netcdfID, const char *attName, T values,
        const char *varName, const char *groupName,
        const netCDF::NcType &netcdfDataType, size_t len
    ) {
        try {
            auto file = FileMap::getInstance().getFile(netcdfID);
            std::shared_ptr<netCDF::NcGroup> group = (groupName && *
                groupName)
                ? std::make_shared<netCDF::NcGroup>(
                    file->getGroup(groupName)
                ) : file;

            if (varName) {
                auto var = group->getVar(varName);
                if (netcdfDataType == netCDF::ncString) {
                    var.putAtt(
                        attName, std::string(
                            reinterpret_cast<const char *>(values)
                        )
                    );
                } else {
                    var.putAtt(attName, netcdfDataType, len, values);
                }
            } else {
                if (netcdfDataType == netCDF::ncString) {
                    group->putAtt(
                        attName, std::string(
                            reinterpret_cast<const char *>(values)
                        )
                    );
                } else {
                    group->putAtt(attName, netcdfDataType, len, values);
                }
            }

            return 0;
        } catch (const netCDF::exceptions::NcException &e) {
            return netcdfErrorMessage(e, __LINE__, __FILE__);
        }
    }


    int netcdfPutAttInt(
        int netcdfID, const char *attName, const int *attValue,
        const char *varName, const char *groupName
    ) {
        return netcdfPutAtt(
            netcdfID, attName, attValue, varName, groupName,
            netCDF::NcType(netCDF::ncInt), 1
        );
    }

    int netcdfPutAttString(
        const int netcdfID, const char *attName, const char *attValue,
        const char *varName, const char *groupName
    ) {
        return netcdfPutAtt(
            netcdfID, attName, attValue, varName, groupName,
            netCDF::NcType(netCDF::ncString), strlen(attValue)
        );
    }
}
