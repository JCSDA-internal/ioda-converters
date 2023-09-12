# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

list( APPEND programs
  satbias_upgrader.py
  )

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   satbias_bin_deps)
execute_process(COMMAND chmod +x ${satbias_bin_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   satbias_bin_install_deps)

install( PROGRAMS ${satbias_bin_install_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_satbias_py_coding_norms
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS    ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )


ecbuild_add_executable( TARGET  satbias2ioda.x
                        SOURCES SatBiasConverter.cpp GsiSatBiasReader.cpp GsiSatBiasReader.h
                        LIBS    Eigen3::Eigen ${oops_LIBRARIES} ioda_engines )

ecbuild_add_executable( TARGET  acftbias2ioda.x
                        SOURCES GsiAircraftBiasConverter.cc GsiAircraftBiasReader.cc GsiAircraftBiasReader.h
                        LIBS    Eigen3::Eigen ${oops_LIBRARIES} ioda_engines )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_varbc_gsi_coding_norms
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_cpplint.py
                  ARGS    --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR}
                  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
