# src/gnssro/CMakeLists.txt
#
# Build gnssro ncepbufr to ioda NetCDF4 converter
#

if( iodaconv_gnssro_ENABLED )
    set(GNSSRO_SRC gnssro_bufr2ioda.f90)
    add_executable(gnssro_bufr2ioda ${GNSSRO_SRC})
    target_link_libraries(gnssro_bufr2ioda ${bufrlib_LIBRARIES})
    target_link_libraries(gnssro_bufr2ioda NetCDF::NetCDF_Fortran)
    if(CMAKE_Fortran_COMPILER_ID MATCHES GNU AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
        target_compile_options(gnssro_bufr2ioda PRIVATE -fallow-argument-mismatch)
    endif()

    install(TARGETS gnssro_bufr2ioda RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
    set_target_properties(gnssro_bufr2ioda PROPERTIES
                        RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
endif()
