#include "netcdf_variable.h"
#include "netcdf_file.h"
#include "netcdf_error.h"
#include <algorithm>
#include <cstring>

namespace Obs2Ioda {

    std::vector<char> flattenCharPtrArray(const char *const *values, const int numStrings, const int stringSize) {
        std::vector<char> contiguousValues(numStrings * stringSize + numStrings, ' ');

        for (int i = 0; i < numStrings; ++i) {
            int len = std::strlen(values[i]);
            std::copy_n(values[i], std::min(len, stringSize), contiguousValues.begin() + i * stringSize);
            contiguousValues[i * stringSize + stringSize] = '\0';
        }
        return contiguousValues;
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
                                           groupName));
            std::vector<netCDF::NcDim> dims;
            dims.reserve(numDims);
            for (int i = 0; i < numDims; i++) {
                dims.push_back(file->getDim(dimNames[i]));;
            }
            auto var = group->addVar(
                varName,
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

    template<typename T>
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
                                           groupName));
            const auto var = group->getVar(varName);
            auto varType = var.getType();
            // Special handling for char arrays
            if (varType == netCDF::ncChar) {
                const auto contiguousValues = flattenCharPtrArray(
                    reinterpret_cast<const char * const *>(values),
                    static_cast<int>(var.getDims()[0].getSize()),
                    static_cast<int>(var.getDims()[1].getSize())
                );
                var.putVar(contiguousValues.data());
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
        return netcdfPutVar(
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
                                           groupName));
            auto var = group->getVar(varName);
            var.setFill(
                fillMode,
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
