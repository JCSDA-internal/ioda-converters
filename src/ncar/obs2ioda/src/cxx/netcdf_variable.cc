/*
 * (C) Copyright 2026 UCAR
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "netcdf_variable.h"
#include "netcdf_file.h"
#include "netcdf_error.h"
#include "netcdf_utils.h"
#include <algorithm>
#include <cstring>

namespace Obs2Ioda {

    std::vector<char>
    flattenCharArray(const char *const *values, size_t numStrings,
                     size_t stringLen) {
        std::vector<char> flattened(numStrings * stringLen, ' ');  // default to space padding

        for (size_t i = 0; i < numStrings; ++i) {
            size_t len = std::min(std::strlen(values[i]), stringLen - 1);
            // leave room for null terminator
            std::memcpy(&flattened[i * stringLen], values[i], len);
            flattened[i * stringLen + len] = '\0';  // explicitly null-terminate
        }
        return flattened;
    }

    int netcdfAddVar(
            int netcdfID,
            const char *groupName,
            const char *varName,
            nc_type netcdfDataType,
            int numDims,
            const char **dimNames,
            const ZlibSettings *zlibSettings
    ) {
        try {
            auto file = FileMap::getInstance().getFile(netcdfID);
            const auto group = setNetcdfGroup(file, groupName);
            std::vector<netCDF::NcDim> dims;
            dims.reserve(numDims);
            for (int i = 0; i < numDims; i++) {
                dims.push_back(file->getDim(
                    iodaSchema.getDimension(dimNames[i])->getValidName()));;
            }
            auto iodaVarName = iodaSchema.getVariable(varName)->getValidName();
            auto var = group->addVar(iodaVarName,
                                     netCDF::NcType(netcdfDataType),
                                     dims);
            // Skip types not supported by deflate
            bool compressible = netcdfDataType != NC_STRING && netcdfDataType != NC_CHAR;

            if (compressible && !dims.empty() && zlibSettings->enabled) {
                std::vector<size_t> chunks;
                chunks.reserve(dims.size());

                for (const auto &d : dims) {
                    size_t n = d.getSize();

                    // avoid huge chunks (important for performance)
                    if (n > 256) n = 256;
                    if (n == 0) n = 1;
                    chunks.push_back(n);
                }

                // Required before compression
                var.setChunking(netCDF::NcVar::nc_CHUNKED, chunks);
                // shuffle filter + deflate level 4
                var.setCompression(zlibSettings->shuffle, zlibSettings->deflate,
                                   zlibSettings->deflateLevel);
            }
            return 0;
        } catch (netCDF::exceptions::NcException &e) {
            return netcdfErrorMessage(e, __LINE__, __FILE__);
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
            const auto group = setNetcdfGroup(file, groupName);
            auto iodaVarName = iodaSchema.getVariable(varName)->getValidName();
            const auto var = group->getVar(iodaVarName);
            // Validate the data type of the variable
            validateNetcdfDataType<T>(
                    var.getType().getId(),
                    "Invalid data type for NetCDF variable '" +
                    std::string(varName) +
                    "': expected " + std::string(typeid(T).name()) +
                    ", got NetCDF type ID " + var.getType().getName());
            if constexpr (std::is_same<T, const char *>::value && netcdfChar) {
                if (var.getDims().size() != 2) {
                    std::string msg =
                            "Expected a 2D char variable for NetCDF variable '" +
                            std::string(varName) + "', but got " +
                            std::to_string(var.getDims().size()) +
                            " dimensions.";
                    throw netCDF::exceptions::NcBadDim(msg.c_str(), __FILE__, __LINE__);
                }
                auto numStrings = var.getDims()[0].getSize();
                auto stringLen = var.getDims()[1].getSize();
                auto flattenedCharValues = flattenCharArray(values, numStrings, stringLen);
                var.putVar(flattenedCharValues.data());
                return 0;
            }
            var.putVar(values);
            return 0;
        } catch (netCDF::exceptions::NcException &e) {
            return netcdfErrorMessage(e, __LINE__, __FILE__);
        }
    }

    int netcdfPutVarInt(
            int netcdfID,
            const char *groupName,
            const char *varName,
            const int *values
    ) {
        return netcdfPutVar(netcdfID, groupName, varName, values);
    }

    int netcdfPutVarInt64(
            int netcdfID,
            const char *groupName,
            const char *varName,
            const int64_t *values
    ) {
        return netcdfPutVar(netcdfID, groupName, varName, values);
    }

    int netcdfPutVarReal(
            int netcdfID,
            const char *groupName,
            const char *varName,
            const float *values
    ) {
        return netcdfPutVar(netcdfID, groupName, varName, values);
    }

    int netcdfPutVarDouble(
            int netcdfID,
            const char *groupName,
            const char *varName,
            const double *values
    ) {
        return netcdfPutVar(netcdfID, groupName, varName, values);
    }

    int netcdfPutVarChar(
            int netcdfID,
            const char *groupName,
            const char *varName,
            const char **values
    ) {
        return netcdfPutVar<const char *, true>(netcdfID, groupName, varName, values);
    }

    int netcdfPutVarString(
            int netcdfID,
            const char *groupName,
            const char *varName,
            const char **values
    ) {
        return netcdfPutVar(netcdfID, groupName, varName, values);
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
            const auto group = setNetcdfGroup(file, groupName);
            auto iodaVarName = iodaSchema.getVariable(varName)->getValidName();
            auto var = group->getVar(iodaVarName);
            // Validate the data type of the variable
            validateNetcdfDataType<T>(
                    var.getType().getId(),
                    "Invalid data type for NetCDF variable '" +
                    std::string(varName) +
                    "': expected " + std::string(typeid(T).name()) +
                    ", got NetCDF type ID " + var.getType().getName());
            var.setFill(fillMode != 0,  // true if fillMode is non-zero
                        fillValue);
            return 0;
        } catch (netCDF::exceptions::NcException &e) {
            return netcdfErrorMessage(e, __LINE__, __FILE__);
        }
    }

    int netcdfSetFillInt(
            int netcdfID,
            const char *groupName,
            const char *varName,
            int fillMode,
            int fillValue
    ) {
        return netcdfSetFill(netcdfID, groupName, varName, fillMode, fillValue);
    }

    int netcdfSetFillReal(
            int netcdfID,
            const char *groupName,
            const char *varName,
            int fillMode,
            float fillValue
    ) {
        return netcdfSetFill(netcdfID, groupName, varName, fillMode, fillValue);
    }

    int netcdfSetFillInt64(
            int netcdfID,
            const char *groupName,
            const char *varName,
            int fillMode,
            int64_t fillValue
    ) {
        return netcdfSetFill(netcdfID, groupName, varName, fillMode, fillValue);
    }

    int netcdfSetFillString(
            int netcdfID,
            const char *groupName,
            const char *varName,
            int fillMode,
            const char *fillValue
    ) {
        return netcdfSetFill(netcdfID, groupName, varName, fillMode, fillValue);
    }
}  // namespace Obs2Ioda
