#ifndef OBS2IODA_NETCDF_ERROR_H
#define OBS2IODA_NETCDF_ERROR_H


#include <netcdf>
#include <variant>
#include <ncChar.h>

namespace Obs2Ioda {

    /**
* @brief Compile-time check to determine if a type T is included in a std::variant.
*
* This type trait evaluates to `true` if the type `T` is one of the types listed
* in the given `std::variant`, and `false` otherwise. It can be used in `if constexpr`
* conditions or `static_assert`s to restrict templates to a fixed set of types.
*
* @tparam T The type to check for membership in the variant.
* @tparam Variant A `std::variant` type listing all allowed types.
*
* @note This trait works by expanding the types in the variant and checking for
*       a match using `std::is_same` and `std::disjunction`.
*/
    template<typename T, typename Variant>
    struct is_in_variant;

    template<typename T, typename... Types>
    struct is_in_variant<T, std::variant<Types...>>
            : std::disjunction<std::is_same<T, Types>...> {
    };


    /**
 * @brief Validates that the NetCDF variable type matches the expected C++ type T.
 *
 * This function ensures that the given NetCDF type (`nc_type`) is compatible with
 * the template type `T`. If the types are not compatible, an `NcBadType` exception
 * is thrown with the provided error message.
 *
 * Supported type mappings:
 * - `int`         → `NC_INT`
 * - `float`       → `NC_FLOAT`
 * - `double`      → `NC_DOUBLE`
 * - `long long`   → `NC_INT64`
 * - `const char*` → `NC_CHAR` or `NC_STRING`
 *
 * @tparam T The expected C++ type for the NetCDF variable.
 * @param netcdfDataType The NetCDF type ID to validate (e.g., `NC_INT`, `NC_FLOAT`).
 * @param errorMessage The error message to include in the exception if the types mismatch.
 *
 * @throws netCDF::exceptions::NcBadType If the NetCDF type is not compatible with `T`.
 * @note Compilation will fail if `T` is not one of the supported types.
 */
    template<typename T>
    void validateNetcdfDataType(nc_type netcdfDataType,
                                const std::string &errorMessage) {
        auto throwBadTypeError = [&](const std::string &msg) {
            throw netCDF::exceptions::NcBadType(msg.c_str(), __FILE__,
                                                __LINE__);
        };
        using SupportedTypes = std::variant<int, float, double, long long, const char *>;

        if constexpr (!is_in_variant<T, SupportedTypes>::value) {
            throwBadTypeError(errorMessage);
        } else if constexpr (std::is_same_v<T, int>) {
            if (netcdfDataType != NC_INT)
                throwBadTypeError(errorMessage);

        } else if constexpr (std::is_same_v<T, float>) {
            if (netcdfDataType != NC_FLOAT)
                throwBadTypeError(errorMessage);

        } else if constexpr (std::is_same_v<T, double>) {
            if (netcdfDataType != NC_DOUBLE)
                throwBadTypeError(errorMessage);

        } else if constexpr (std::is_same_v<T, long long>) {
            if (netcdfDataType != NC_INT64)
                throwBadTypeError(errorMessage);

        } else if constexpr (std::is_same_v<T, const char *>) {
            if (netcdfDataType != NC_CHAR &&
                netcdfDataType != NC_STRING)
                throwBadTypeError(errorMessage);
        } else {
            static_assert(sizeof(T) == 0,
                          "Unsupported data type for NetCDF variable");
        }
    }


/**
 * @brief Logs detailed information about a NetCDF exception.
 *
 * This function captures and logs detailed error information from a
 * NetCDF exception, including the error code, file name, line number,
 * and error message. The error message is output to the standard error
 * stream, and the NetCDF error code is returned.
 *
 * @param e The NetCDF exception to log, of type `netCDF::exceptions::NcException`.
 * @param lineNumber The line number in the source file where the exception occurred.
 *                   Defaults to -1 if not provided.
 * @param fileName The name of the source file where the exception occurred.
 *                 Defaults to an empty string if not provided.
 * @return The error code from the NetCDF exception. If the error code is 0,
 * which is the case for NcNullGrp, NcNullDim, and NcNullType exceptions,
 * -1 is returned instead.
 *
 * @note If `fileName` is empty, the file name and line number are not included in the log.
 *       If `lineNumber` is less than or equal to 0, it is ignored in the log.
 *
 * @example
 * @code
 * try {
 *     std::vector<size_t> data = {1, 2, 3, 4, 5};
 *     std::vector<size_t> index = {0};
 *     auto var = file.getVar("foo");
 *     var.putVar(index, data.data());
 * }
 * catch (const netCDF::exceptions::NcException &e) {
 *     Obs2Ioda::netcdfErrorMessage(e, __LINE__, __FILE__);
 * }
 * @endcode
 */
    int netcdfErrorMessage(
            const netCDF::exceptions::NcException &e,
            int lineNumber = -1,
            const std::string &fileName = ""
    );

}

#endif //OBS2IODA_NETCDF_ERROR_H
