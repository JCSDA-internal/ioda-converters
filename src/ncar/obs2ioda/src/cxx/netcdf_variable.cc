#include "netcdf_variable.h"
#include "netcdf_file.h"
#include "netcdf_error.h"
#include <algorithm>
#include <cstring>

namespace Obs2Ioda {

    std::vector<char>
    flattenCharArray(const char * const* values, size_t numStrings,
                     size_t stringLen) {
        std::vector<char> flattened(numStrings * stringLen,
                                    ' ');  // default to space padding

        for (size_t i = 0; i < numStrings; ++i) {
            size_t len = std::min(std::strlen(values[i]), stringLen -
                                                          1);  // leave room for null terminator
            std::memcpy(&flattened[i * stringLen], values[i], len);
            flattened[i * stringLen +
                      len] = '\0';  // explicitly null-terminate
        }
        return flattened;
    }

    int netcdfAddVar(
        int netcdfID,
        const char *groupName,
        const char *varName,
        nc_type netcdfDataType,
        int numDims,
        const char **dimNames
    ) {
        try {
            auto file = FileMap::getInstance().getFile(netcdfID);
            const auto group = !groupName
                                   ? file
                                   : std::make_shared<
                                       netCDF::NcGroup>(
                                       file->getGroup(
                                           iodaSchema.getGroup(groupName)->getValidName()));
            std::vector<netCDF::NcDim> dims;
            dims.reserve(numDims);
            for (int i = 0; i < numDims; i++) {
                dims.push_back(file->getDim(iodaSchema.getDimension(dimNames[i])->getValidName()));;
            }
            auto iodaVarName = iodaSchema.getVariable(varName)->getValidName();
            auto var = group->addVar(
                iodaVarName,
                netCDF::NcType(netcdfDataType),
                dims
            );
            return 0;
        } catch (netCDF::exceptions::NcException &e) {
            return netcdfErrorMessage(
                e,
                __LINE__,
                __FILE__
            );
        }
    }

    template<typename T, bool netcdfChar = false>
    int netcdfPutVar(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const T *values
    ) {
        try {
            auto file = FileMap::getInstance().getFile(netcdfID);
            const auto group = !groupName
                                   ? file
                                   : std::make_shared<
                                       netCDF::NcGroup>(
                                       file->getGroup(
                                           iodaSchema.getGroup(groupName)->getValidName()));
            auto iodaVarName = iodaSchema.getVariable(varName)->getValidName();
            const auto var = group->getVar(iodaVarName);
            if constexpr (std::is_same<T, const char *>::value && netcdfChar) {
                auto numStrings = var.getDims()[0].getSize();
                auto stringLen = var.getDims()[1].getSize();
                auto flattenedCharValues = flattenCharArray(values, numStrings, stringLen);
                var.putVar(flattenedCharValues.data());
                return 0;
            }
            var.putVar(values);
            return 0;
        } catch (netCDF::exceptions::NcException &e) {
            return netcdfErrorMessage(
                e,
                __LINE__,
                __FILE__
            );
        }
    }

    int netcdfPutVarInt(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const int *values
    ) {
        return netcdfPutVar(
            netcdfID,
            groupName,
            varName,
            values
        );
    }

    int netcdfPutVarInt64(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const long long *values
    ) {
        return netcdfPutVar(
            netcdfID,
            groupName,
            varName,
            values
        );
    }

    int netcdfPutVarReal(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const float *values
    ) {
        return netcdfPutVar(
            netcdfID,
            groupName,
            varName,
            values
        );
    }

    int netcdfPutVarDouble(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const double *values
    ) {
        return netcdfPutVar(
            netcdfID,
            groupName,
            varName,
            values
        );
    }

    int netcdfPutVarChar(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const char **values
    ) {
        return netcdfPutVar<const char *, true>(
            netcdfID,
            groupName,
            varName,
            values
        );
    }

    int netcdfPutVarString(
        int netcdfID,
        const char *groupName,
        const char *varName,
        const char **values
    ) {
        return netcdfPutVar(
            netcdfID,
            groupName,
            varName,
            values
        );
    }

    template<typename T>
    int netcdfSetFill(
        int netcdfID,
        const char *groupName,
        const char *varName,
        int fillMode,
        T fillValue
    ) {
        try {
            auto file = FileMap::getInstance().getFile(netcdfID);
            const auto group = !groupName
                                   ? file
                                   : std::make_shared<
                                       netCDF::NcGroup>(
                                       file->getGroup(
                                           iodaSchema.getGroup(groupName)->getValidName()));
            auto iodaVarName = iodaSchema.getVariable(varName)->getValidName();
            auto var = group->getVar(iodaVarName);
            var.setFill(
                fillMode != 0,  // true if fillMode is non-zero
                fillValue
            );
            return 0;
        } catch (netCDF::exceptions::NcException &e) {
            return netcdfErrorMessage(
                e,
                __LINE__,
                __FILE__
            );
        }
    }

    int netcdfSetFillInt(
        int netcdfID,
        const char *groupName,
        const char *varName,
        int fillMode,
        int fillValue
    ) {
        return netcdfSetFill(
            netcdfID,
            groupName,
            varName,
            fillMode,
            fillValue
        );
    }

    int netcdfSetFillReal(
        int netcdfID,
        const char *groupName,
        const char *varName,
        int fillMode,
        float fillValue
    ) {
        return netcdfSetFill(
            netcdfID,
            groupName,
            varName,
            fillMode,
            fillValue
        );
    }

    int netcdfSetFillInt64(
        int netcdfID,
        const char *groupName,
        const char *varName,
        int fillMode,
        long long fillValue
    ) {
        return netcdfSetFill(
            netcdfID,
            groupName,
            varName,
            fillMode,
            fillValue
        );
    }

    int netcdfSetFillString(
        int netcdfID,
        const char *groupName,
        const char *varName,
        int fillMode,
        const char *fillValue
    ) {
        return netcdfSetFill(
            netcdfID,
            groupName,
            varName,
            fillMode,
            fillValue

        );
    }
}
