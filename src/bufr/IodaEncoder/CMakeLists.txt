# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.


list(APPEND _ioda_encoder_srcs
  EncoderTypes.h
  DataContainer.h
  DataContainer.cpp
  Parser.h
  ParserFactory.h
  DataObject/DataObject.h
  DataObject/StrVecDataObject.h
  DataObject/StrVecDataObject.cpp
  DataObject/ArrayDataObject.h
  DataObject/ArrayDataObject.cpp
  IodaEncoder.cpp
  IodaEncoder.h
  IodaDescription.cpp
  IodaDescription.h
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
