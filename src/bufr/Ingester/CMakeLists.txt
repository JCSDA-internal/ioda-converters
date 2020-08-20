# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

list(APPEND INGESTER_SRCS
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
        oops/IntSetParser.cpp
        oops/IntSetParser.h
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

add_library(ingester ${INGESTER_SRCS} )
target_link_libraries(ingester PUBLIC Eigen3::Eigen)
target_link_libraries(ingester PUBLIC eckit)
target_link_libraries(ingester PRIVATE bufr::bufr_4_DA)


target_include_directories(ingester PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)

target_include_directories(ingester INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:include>)
