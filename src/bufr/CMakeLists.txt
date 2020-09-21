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

list(APPEND _ingester_deps
  Eigen3::Eigen
  eckit
  ${oops_LIBRARIES}
  ioda_engines
  bufr::bufr_4_DA
)

ecbuild_add_library( TARGET   ingester
                     SOURCES  ${_ingester_srcs}
                     LIBS     ${_ingester_deps}
                     INSTALL_HEADERS LISTED
                     HEADER_DESTINATION ${INSTALL_INCLUDEDIR}/bufr
                     LINKER_LANGUAGE CXX
                    )
add_library( ${PROJECT_NAME}::ingester ALIAS ingester )

add_executable(test_ingester.x test_ingester.cpp)
target_link_libraries(test_ingester.x PRIVATE ${PROJECT_NAME}::ingester)

add_executable(convertBufrToIoda.x main.cpp)
target_link_libraries(convertBufrToIoda.x PRIVATE ${PROJECT_NAME}::ingester)

add_test(NAME ${PROJECT_NAME}_bufr_coding_norms
COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_cpplint.sh --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR})
