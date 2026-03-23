set(obs2ioda_cxx_SOURCES
    netcdf_error.cc
    netcdf_file.cc
    netcdf_group.cc
    netcdf_dimension.cc
    netcdf_variable.cc
    netcdf_attribute.cc
    netcdf_utils.cc
    ioda_obs_schema.cc
)

set(obs2ioda_cxx_LIBRARIES
    NetCDF::NetCDF_CXX
    NetCDF::NetCDF_C
    eckit
)

set(obs2ioda_cxx_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/generated
)

add_library(obs2ioda_cxx SHARED ${obs2ioda_cxx_SOURCES})
target_compile_definitions(obs2ioda_cxx PUBLIC OBS2IODA_ROOT_DIR="${CMAKE_SOURCE_DIR}")
obs2ioda_cxx_library(obs2ioda_cxx "${obs2ioda_cxx_INCLUDE_DIRS}" "${obs2ioda_cxx_LIBRARIES}")

ecbuild_add_test( TARGET  ${PROJECT_NAME}_obs2ioda_coding_norms
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_cpplint.py
                    ARGS    --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR}
                    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
