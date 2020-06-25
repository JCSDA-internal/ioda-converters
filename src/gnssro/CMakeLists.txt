# src/gnssro/CMakeLists.txt
#
# Build gnssro ncepbufr to ioda NetCDF4 converter
#

if( HAVE_BUFRLIB_AND_NETCDF )
    set(GNSSRO_SRC gnssro_bufr2ioda.f90)
    add_executable(gnssro_bufr2ioda ${GNSSRO_SRC})
    target_link_libraries(gnssro_bufr2ioda ${bufrlib_LIBRARIES})
    if(CMAKE_Fortran_COMPILER_ID MATCHES GNU AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
        target_compile_options(gnssro_bufr2ioda PRIVATE -fallow-argument-mismatch)
    endif()

    #NetCDF4
    target_link_libraries(gnssro_bufr2ioda ${NETCDF_LIBRARIES})
    target_compile_definitions(gnssro_bufr2ioda PUBLIC "${NETCDF_DEFINITIONS}")
    target_include_directories(gnssro_bufr2ioda PUBLIC "${NETCDF_INCLUDE_DIRS}")

    install(TARGETS gnssro_bufr2ioda
            RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
    set_target_properties(gnssro_bufr2ioda PROPERTIES
                        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})
endif()
