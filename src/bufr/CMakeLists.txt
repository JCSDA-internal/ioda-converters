# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

list(APPEND _ingester_srcs
  IngesterTypes.h
  IngesterData.h
  IngesterData.cpp
  BufrParser/BufrParser.h
  BufrParser/BufrParser.cpp
  BufrParser/BufrDescription.h
  BufrParser/BufrDescription.cpp
  BufrParser/BufrMnemonicSet.h
  BufrParser/BufrMnemonicSet.cpp
  IodaEncoder/IodaEncoder.cpp
  IodaEncoder/IodaEncoder.h
  BufrParser/BufrTypes.h
  BufrParser/BufrCollectors/BufrAccumulator.cpp
  BufrParser/BufrCollectors/BufrAccumulator.h
  BufrParser/BufrCollectors/BufrCollector.cpp
  BufrParser/BufrCollectors/BufrCollector.h
  BufrParser/BufrCollectors/BufrIntCollector.cpp
  BufrParser/BufrCollectors/BufrIntCollector.h
  BufrParser/BufrCollectors/BufrRepCollector.cpp
  BufrParser/BufrCollectors/BufrRepCollector.h
  BufrParser/BufrCollectors/BufrCollectors.cpp
  BufrParser/BufrCollectors/BufrCollectors.h
)

list(APPEND INGESTER_LIBS_DEP
  Eigen3::Eigen
  eckit
  oops
  ioda-engines
  bufr::bufr_4_DA)

ecbuild_add_library( TARGET   ingester
                     SOURCES  ${_ingester_srcs}
                     LIBS     ${INGESTER_LIBS_DEP}
                     INSTALL_HEADERS LISTED
                     HEADER_DESTINATION ${INSTALL_INCLUDE_DIR}/iodaconv
                     LINKER_LANGUAGE ${IODACONV_LINKER_LANGUAGE}
                    )

add_executable(test_ingester.x test_ingester.cpp)
target_link_libraries(test_ingester.x PRIVATE ingester)

ecbuild_add_test( TARGET iodaconv_bufr_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_cpplint.py
                  ARGS --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR})
