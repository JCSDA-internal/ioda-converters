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

set_target_properties(obs2ioda_cxx PROPERTIES
              ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
              LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
              INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")

target_link_libraries(obs2ioda_cxx PUBLIC "${obs2ioda_cxx_LIBRARIES}")
target_include_directories(obs2ioda_cxx PUBLIC "${obs2ioda_cxx_INCLUDE_DIRS}")

