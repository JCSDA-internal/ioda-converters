# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

list(APPEND _ingester_srcs
  IngesterTypes.h
  DataContainer.h
  DataContainer.cpp
  DataObject/DataObject.h
  DataObject/StrVecDataObject.h
  DataObject/StrVecDataObject.cpp
  DataObject/ArrayDataObject.h
  DataObject/ArrayDataObject.cpp
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
  BufrParser/Exports/Export.h
  BufrParser/Exports/DatetimeExport.h
  BufrParser/Exports/DatetimeExport.cpp
  BufrParser/Exports/MnemonicExport.h
  BufrParser/Exports/MnemonicExport.cpp
  IodaEncoder/IodaEncoder.cpp
  IodaEncoder/IodaEncoder.h
  IodaEncoder/IodaDescription.cpp
  IodaEncoder/IodaDescription.h
)

add_library(ingester SHARED ${_ingester_srcs})

target_link_libraries( ingester PUBLIC Eigen3::Eigen )
target_link_libraries( ingester PUBLIC eckit )
target_link_libraries( ingester PUBLIC ${oops_LIBRARIES} ) #TODO: Change to "oops::oops" once oops adds namespace support
#target_link_libraries( ingester PUBLIC ioda_engines )
target_link_libraries( ingester PUBLIC bufr::bufr_4_DA )

set (IODA_ENGINES_PATH ${CMAKE_PREFIX_PATH})
include_directories(${IODA_ENGINES_PATH}/ioda-engines/include)
target_link_libraries(ingester PRIVATE ${IODA_ENGINES_PATH}/ioda-engines/lib/libioda-engines.dylib)
target_link_libraries(ingester PRIVATE ${IODA_ENGINES_PATH}/ioda-engines/lib/libHH.dylib)
target_link_libraries(ingester PRIVATE ${IODA_ENGINES_PATH}/ioda-engines/lib/libjedi.dylib)

target_include_directories(ingester PUBLIC
                           $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
                           $<INSTALL_INTERFACE:include/${PROJECT_NAME}>)

add_executable(test_ingester.x test_ingester.cpp)
target_link_libraries(test_ingester.x PRIVATE ingester)

#ecbuild_add_test( TARGET iodaconv_bufr_coding_norms
#                  TYPE SCRIPT
#                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_cpplint.py
#                  ARGS --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR})
