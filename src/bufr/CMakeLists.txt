# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#list(APPEND _ingester_srcs
#)

#list(APPEND INGESTER_LIBS_DEP
#  Eigen3::Eigen
#  eckit
#  oops
#  ioda-engines
#  bufr::bufr_4_DA)

#ecbuild_add_library( TARGET   ingester
#                     SOURCES  ${_ingester_srcs}
#                     LIBS     ${INGESTER_LIBS_DEP}
#                     INSTALL_HEADERS LISTED
#                     HEADER_DESTINATION ${INSTALL_INCLUDE_DIR}/iodaconv
#                     LINKER_LANGUAGE ${IODACONV_LINKER_LANGUAGE}
#                    )

#target_link_libraries(ingester PRIVATE oops::oops)
#target_link_libraries(ingester PRIVATE ioda_engines::ioda_engines)

#add_executable(test_ingester.x test_ingester.cpp)
#target_link_libraries(test_ingester.x PRIVATE ingester)

add_test(NAME iodaconv_bufr_coding_norms
         COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_cpplint.py
         --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR})
