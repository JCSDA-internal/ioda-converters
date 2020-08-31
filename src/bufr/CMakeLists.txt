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
  IodaEncoder/IodaEncoder.cpp
  IodaEncoder/IodaEncoder.h
  IodaEncoder/IodaDescription.cpp
  IodaEncoder/IodaDescription.h
)

list(APPEND INGESTER_LIBS_DEP
  Eigen3::Eigen
  eckit
  oops
  bufr::bufr_4_DA
  ioda-engines)

ecbuild_add_library( TARGET   ingester
                     SOURCES  ${_ingester_srcs}
                     LIBS     ${INGESTER_LIBS_DEP}
                     INSTALL_HEADERS LISTED
                     HEADER_DESTINATION ${INSTALL_INCLUDE_DIR}/iodaconv
                     LINKER_LANGUAGE ${IODACONV_LINKER_LANGUAGE}
                    )


add_executable(test_ingester.x test_ingester.cpp)
target_link_libraries(test_ingester.x PRIVATE ingester)


set (IODA_ENGINES_PATH ${CMAKE_PREFIX_PATH})
target_include_directories(test_ingester.x PRIVATE $<BUILD_INTERFACE:${IODA_ENGINES_PATH}/ioda-engines/include>)
#include_directories(${IODA_ENGINES_PATH}/ioda-engines/include)
#
#find_library(IODA_ENGINES_LIB libioda-engines ${IODA_ENGINES_PATH}/ioda-engines/lib)
#message('****** ' ${IODA_ENGINES_PATH}/ioda-engines/lib)
#target_link_libraries(test_ingester.x PRIVATE ${IODA_ENGINES_LIB})
