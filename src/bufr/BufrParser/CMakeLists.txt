# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.


list(APPEND _bufr_parser_srcs
  BufrParser.h
  BufrParser.cpp
  BufrDescription.h
  BufrDescription.cpp
  BufrMnemonicSet.h
  BufrMnemonicSet.cpp
  BufrTypes.h
  BufrCollectors/BufrAccumulator.cpp
  BufrCollectors/BufrAccumulator.h
  BufrCollectors/BufrCollector.cpp
  BufrCollectors/BufrCollector.h
  BufrCollectors/BufrIntCollector.cpp
  BufrCollectors/BufrIntCollector.h
  BufrCollectors/BufrRepCollector.cpp
  BufrCollectors/BufrRepCollector.h
  BufrCollectors/BufrCollectors.cpp
  BufrCollectors/BufrCollectors.h
  Exports/Export.h
  Exports/DatetimeExport.h
  Exports/DatetimeExport.cpp
  Exports/MnemonicExport.h
  Exports/MnemonicExport.cpp
  Exports/Transforms/Transform.h
  Exports/Transforms/OffsetTransform.h
  Exports/Transforms/OffsetTransform.cpp
  Exports/Transforms/ScalingTransform.h
  Exports/Transforms/ScalingTransform.cpp
  Exports/Transforms/TransformBuilder.h
  Exports/Transforms/TransformBuilder.cpp
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
