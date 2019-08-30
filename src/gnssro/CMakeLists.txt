
find_package(NetCDF REQUIRED COMPONENTS Fortran)
find_package(bufrlib REQUIRED)

set(GNSSRO_SRC gnssro_bufr2ioda.f90)
add_executable(gnssro_bufr2ioda ${GNSSRO_SRC})
target_link_libraries(gnssro_bufr2ioda bufrlib::bufrlib_shared)

#NetCDF4
target_link_libraries(gnssro_bufr2ioda ${NETCDF_LIBRARIES})
target_compile_definitions(gnssro_bufr2ioda PUBLIC "${NETCDF_DEFINITIONS}")
target_include_directories(gnssro_bufr2ioda PUBLIC "${NETCDF_INCLUDE_DIRS}")

install(TARGETS gnssro_bufr2ioda)
set_target_properties(gnssro_bufr2ioda PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
