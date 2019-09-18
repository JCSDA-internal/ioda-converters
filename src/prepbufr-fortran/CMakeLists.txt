include(GNUInstallDirs)
find_package(NetCDF REQUIRED COMPONENTS Fortran)
find_package(bufrlib REQUIRED)

list(APPEND bufr2ioda_source
  get_obs.f90
  kinds.f90
  module_obs_base.f90
  module_obs_conv_pt.f90
  module_obsncio.f90
  module_prepbufr.f90
  module_time.f90
  )

add_executable(bufr2ioda ${bufr2ioda_source})
target_link_libraries(bufr2ioda bufrlib::bufrlib_shared ${NETCDF_LIBRARIES})
target_compile_definitions(bufr2ioda PUBLIC "${NETCDF_DEFINITIONS}")
target_include_directories(bufr2ioda PUBLIC "${NETCDF_INCLUDE_DIRS}")

install(TARGETS bufr2ioda
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
set_target_properties(bufr2ioda PROPERTIES
                      RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})

