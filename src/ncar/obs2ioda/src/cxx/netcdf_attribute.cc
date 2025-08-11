#include "netcdf_attribute.h"
#include "netcdf_file.h"
#include "netcdf_error.h"
#include "netcdf_utils.h"

namespace Obs2Ioda {
    template<typename T, bool netcdfString = false> int netcdfPutAtt(
        int netcdfID, const char *attName, T values,
        const char *varName, const char *groupName,
        const netCDF::NcType &netcdfDataType, size_t len
    ) {
        try {
            auto file = FileMap::getInstance().getFile(netcdfID);
            const auto group = setNetcdfGroup(file, groupName);
            if (varName == nullptr) {
                std::string msg = "Variable name cannot be null";
                throw netCDF::exceptions::NcBadName(
                    msg.c_str(), __FILE__, __LINE__
                );
            }
            if (!std::string(varName).empty()) {
                auto iodaVarName = iodaSchema.getVariable(varName)->getValidName();
                auto var = group->getVar(iodaVarName);
                if constexpr(std::is_same_v<const char *, T> && netcdfString) {
                    var.putAtt(
                        attName, std::string(
                            reinterpret_cast<const char *>(values)
                        )
                    );
                } else {
                    var.putAtt(attName, netcdfDataType, len, values);
                }
            } else {
                if constexpr(std::is_same_v<const char *, T> && netcdfString) {
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

    int netcdfPutAttIntArray(
        int netcdfID, const char *attName, const int *attValue,
        const int attLen, const char *varName, const char *groupName
    ) {
        return netcdfPutAtt(
            netcdfID, attName, attValue, varName, groupName,
            netCDF::NcType(netCDF::ncInt), attLen
        );
    }

    int netcdfPutAttRealArray(
        int netcdfID, const char *attName, const float *attValue,
        const int attLen, const char *varName, const char *groupName
    ) {
        return netcdfPutAtt(
            netcdfID, attName, attValue, varName, groupName,
            netCDF::NcType(netCDF::ncFloat), attLen
        );
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
        return netcdfPutAtt<const char *, true>(
            netcdfID, attName, attValue, varName, groupName,
            netCDF::NcType(netCDF::ncString), strlen(attValue)
        );
    }
}
