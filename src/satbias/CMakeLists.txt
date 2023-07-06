# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

ecbuild_add_executable( TARGET  satbias2ioda.x
                        SOURCES SatBiasConverter.cpp GsiSatBiasReader.cpp GsiSatBiasReader.h
                        LIBS    Eigen3::Eigen ${oops_LIBRARIES} ioda_engines )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_satbias_coding_norms
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_cpplint.py
                  ARGS    --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR}
                  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
