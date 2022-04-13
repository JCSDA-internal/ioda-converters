# src/gnssro/CMakeLists.txt
#
# Build 1) gnssro ncepbufr to ioda2 NetCDF4 converter
#       2) gnssro gsi ncdiag to ioda NetCDF4 converter
         
ecbuild_add_executable( TARGET  gnssro_bufr2ioda
                        SOURCES gnssro_bufr2ioda.f90
                        LIBS    bufr::bufr_d
                                NetCDF::NetCDF_Fortran)

ecbuild_add_executable( TARGET  gnssro_gsidiag2ioda
                        SOURCES gnssro_gsidiag2ioda.f90
                        LIBS    NetCDF::NetCDF_Fortran)

# config for scripts
list(APPEND scripts
  gnssro_bufr2ioda.py
)

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${scripts}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_gnssro_scripts_deps)
execute_process(COMMAND chmod +x ${bin_gnssro_scripts_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${scripts}"
            ${CMAKE_CURRENT_SOURCE_DIR}
            ${CMAKE_BINARY_DIR}/install-bin
            install_bin_gnssro_scripts_deps)

install( PROGRAMS ${install_bin_gnssro_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_gnssro_coding_norms
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS    ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
