# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.


list(APPEND _ioda_encoder_srcs
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
        IodaEncoder/IodaEncoder.cpp
        IodaEncoder/IodaEncoder.h
        IodaEncoder/IodaDescription.cpp
        IodaEncoder/IodaDescription.h
        )

list (APPEND _ioda_encoder_deps
  Eigen3::Eigen
  eckit
  ${oops_LIBRARIES}
  ioda_engines
)

ecbuild_add_library( TARGET   ioda_encoder
                     SOURCES  ${_ioda_encoder_srcs}
                     LIBS     ${_ioda_encoder_deps}
                     INSTALL_HEADERS LISTED
                     HEADER_DESTINATION ${INSTALL_INCLUDEDIR}/ioda-encoder
                     LINKER_LANGUAGE CXX )

add_library( ${PROJECT_NAME}::ioda_encoder ALIAS ioda_encoder )


list(APPEND _bufr_parser_srcs
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
  BufrParser/Exports/Transforms/Transform.h
  BufrParser/Exports/Transforms/OffsetTransform.h
  BufrParser/Exports/Transforms/OffsetTransform.cpp
  BufrParser/Exports/Transforms/ScalingTransform.h
  BufrParser/Exports/Transforms/ScalingTransform.cpp
  BufrParser/Exports/Transforms/TransformBuilder.h
  BufrParser/Exports/Transforms/TransformBuilder.cpp  
)

list(APPEND _bufr_parser_deps
  Eigen3::Eigen
  eckit
  ${oops_LIBRARIES}
  bufr::bufr_4_DA
  ${PROJECT_NAME}::ioda_encoder
)

ecbuild_add_library( TARGET   bufr_parser
                     SOURCES  ${_bufr_parser_srcs}
                     LIBS     ${_bufr_parser_deps}
                     INSTALL_HEADERS LISTED
                     HEADER_DESTINATION ${INSTALL_INCLUDEDIR}/bufr
                     LINKER_LANGUAGE CXX )

add_library( ${PROJECT_NAME}::bufr_parser ALIAS bufr_parser )

ecbuild_add_executable( TARGET  bufr2ioda.x
                        SOURCES bufr2ioda.cpp
                        LIBS    ${PROJECT_NAME}::bufr_parser )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_bufr_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_cpplint.py
                  ARGS --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR}
                  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin )
