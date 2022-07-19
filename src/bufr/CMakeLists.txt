# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

list(APPEND _ingester_srcs
  IngesterTypes.h
  DataContainer.h
  DataContainer.cpp
  Parser.h
  ParserFactory.h
  DataObject/DataObject.h
  DataObject/StrVecDataObject.h
  DataObject/StrVecDataObject.cpp
  DataObject/ArrayDataObject.h
  DataObject/ArrayDataObject.cpp
  DataObject/Int64VecDataObject.h
  DataObject/Int64VecDataObject.cpp
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
  BufrParser/Exports/Export.cpp
  BufrParser/Exports/RowSlice.h
  BufrParser/Exports/Filters/Filter.h
  BufrParser/Exports/Filters/BoundingFilter.h
  BufrParser/Exports/Filters/BoundingFilter.cpp
  BufrParser/Exports/Splits/Split.h
  BufrParser/Exports/Splits/CategorySplit.h
  BufrParser/Exports/Splits/CategorySplit.cpp
  BufrParser/Exports/Variables/Variable.h
  BufrParser/Exports/Variables/DatetimeVariable.h
  BufrParser/Exports/Variables/DatetimeVariable.cpp
  BufrParser/Exports/Variables/MnemonicVariable.h
  BufrParser/Exports/Variables/MnemonicVariable.cpp
  BufrParser/Exports/Variables/Transforms/Transform.h
  BufrParser/Exports/Variables/Transforms/OffsetTransform.h
  BufrParser/Exports/Variables/Transforms/OffsetTransform.cpp
  BufrParser/Exports/Variables/Transforms/ScalingTransform.h
  BufrParser/Exports/Variables/Transforms/ScalingTransform.cpp
  BufrParser/Exports/Variables/Transforms/TransformBuilder.h
  BufrParser/Exports/Variables/Transforms/TransformBuilder.cpp
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
  bufr::bufr_4
)

ecbuild_add_library( TARGET   ingester
                     SOURCES  ${_ingester_srcs}
                     INSTALL_HEADERS LISTED
                     HEADER_DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/bufr
                     LINKER_LANGUAGE CXX
                    )

target_link_libraries(ingester PUBLIC ${_ingester_deps})

target_include_directories(ingester PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:bufr>  # <prefix>/bufr
)

add_library( ${PROJECT_NAME}::ingester ALIAS ingester )

ecbuild_add_executable( TARGET  bufr2ioda.x
                        SOURCES bufr2ioda.cpp
                        LIBS    ingester )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_bufr_coding_norms
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_cpplint.py
                  ARGS    --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR}
                  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
