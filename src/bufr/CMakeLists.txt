# (C) Copyright 2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

list(APPEND _ingester_srcs
  IngesterTypes.h
  DataContainer.h
  DataContainer.cpp
  Parser.h
  ObjectFactory.h
  DataObject.h
  DataObject.cpp
  BufrParser/BufrParser.h
  BufrParser/BufrParser.cpp
  BufrParser/BufrDescription.h
  BufrParser/BufrDescription.cpp
  BufrParser/Exports/Export.h
  BufrParser/Exports/Export.cpp
  BufrParser/Exports/Filters/Filter.h
  BufrParser/Exports/Filters/BoundingFilter.h
  BufrParser/Exports/Filters/BoundingFilter.cpp
  BufrParser/Exports/Splits/Split.h
  BufrParser/Exports/Splits/CategorySplit.h
  BufrParser/Exports/Splits/CategorySplit.cpp
  BufrParser/Exports/Variables/Variable.h
  BufrParser/Exports/Variables/DatetimeVariable.h
  BufrParser/Exports/Variables/DatetimeVariable.cpp
  BufrParser/Exports/Variables/SpectralRadianceVariable.h
  BufrParser/Exports/Variables/SpectralRadianceVariable.cpp
#>>emily
  BufrParser/Exports/Variables/RemappedBrightnessTemperatureVariable.h
  BufrParser/Exports/Variables/RemappedBrightnessTemperatureVariable.cpp
  BufrParser/Exports/Variables/SensorScanAngleVariable.h
  BufrParser/Exports/Variables/SensorScanAngleVariable.cpp
#<<emily
  BufrParser/Exports/Variables/AircraftAltitudeVariable.h
  BufrParser/Exports/Variables/AircraftAltitudeVariable.cpp
  BufrParser/Exports/Variables/TimeoffsetVariable.h
  BufrParser/Exports/Variables/TimeoffsetVariable.cpp
  BufrParser/Exports/Variables/QueryVariable.h
  BufrParser/Exports/Variables/QueryVariable.cpp
  BufrParser/Exports/Variables/Transforms/Transform.h
  BufrParser/Exports/Variables/Transforms/OffsetTransform.h
  BufrParser/Exports/Variables/Transforms/OffsetTransform.cpp
  BufrParser/Exports/Variables/Transforms/ScalingTransform.h
  BufrParser/Exports/Variables/Transforms/ScalingTransform.cpp
  BufrParser/Exports/Variables/Transforms/TransformBuilder.h
  BufrParser/Exports/Variables/Transforms/TransformBuilder.cpp
  BufrParser/Query/DataProvider/DataProvider.h
  BufrParser/Query/DataProvider/DataProvider.cpp
  BufrParser/Query/DataProvider/NcepDataProvider.h
  BufrParser/Query/DataProvider/NcepDataProvider.cpp
  BufrParser/Query/DataProvider/WmoDataProvider.h
  BufrParser/Query/DataProvider/WmoDataProvider.cpp
  BufrParser/Query/File.h
  BufrParser/Query/File.cpp
  BufrParser/Query/VectorMath.h
  BufrParser/Query/QuerySet.h
  BufrParser/Query/QuerySet.cpp
  BufrParser/Query/QueryRunner.h
  BufrParser/Query/QueryRunner.cpp
  BufrParser/Query/QueryParser.h
  BufrParser/Query/QueryParser.cpp
  BufrParser/Query/ResultSet.h
  BufrParser/Query/ResultSet.cpp
  BufrParser/Query/Target.h
  BufrParser/Query/Tokenizer.h
  BufrParser/Query/Tokenizer.cpp
  BufrParser/Query/SubsetTable.h
  BufrParser/Query/SubsetTable.cpp
  IodaEncoder/IodaEncoder.cpp
  IodaEncoder/IodaEncoder.h
  IodaEncoder/IodaDescription.cpp
  IodaEncoder/IodaDescription.h
)
#>>Ron
list (APPEND _atmslib_srcs
        BufrParser/Exports/Variables/Transforms/atms/kinds.F90
        BufrParser/Exports/Variables/Transforms/atms/atms_spatial_average_mod.f90
        BufrParser/Exports/Variables/Transforms/atms/atms_spatial_average_interface.h
        BufrParser/Exports/Variables/Transforms/atms/atms_spatial_average_interface.f90
)
ecbuild_add_library( TARGET   atms_lib
                     SOURCES  ${_atmslib_srcs}
                     TYPE STATIC
                     INSTALL_HEADERS LISTED
		    )
#<<Ron

list(APPEND _ingester_deps
  Eigen3::Eigen
  eckit
  ${oops_LIBRARIES}
  ioda_engines
  bufr::bufr_d
  atms_lib
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

target_compile_definitions(ingester PRIVATE BUILD_IODA_BINDING=1)

add_library( ${PROJECT_NAME}::ingester ALIAS ingester)

ecbuild_add_executable( TARGET  bufr2ioda.x
                        SOURCES bufr2ioda.cpp
                        LIBS    ingester )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_bufr_coding_norms
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_cpplint.py
                  ARGS    --quiet --recursive ${CMAKE_CURRENT_SOURCE_DIR}
                  WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/bin )


# Make the python bindings

list(APPEND _query_libs
			Eigen3::Eigen
			eckit
			bufr::bufr_d
			)

list (APPEND _query_srcs
	DataObject.h
	DataObject.cpp
	BufrParser/Query/DataProvider/DataProvider.h
	BufrParser/Query/DataProvider/DataProvider.cpp
	BufrParser/Query/DataProvider/NcepDataProvider.h
	BufrParser/Query/DataProvider/NcepDataProvider.cpp
	BufrParser/Query/DataProvider/WmoDataProvider.h
	BufrParser/Query/DataProvider/WmoDataProvider.cpp
	BufrParser/Query/File.h
	BufrParser/Query/File.cpp
	BufrParser/Query/VectorMath.h
	BufrParser/Query/QuerySet.h
	BufrParser/Query/QuerySet.cpp
	BufrParser/Query/QueryRunner.h
	BufrParser/Query/QueryRunner.cpp
	BufrParser/Query/QueryParser.h
	BufrParser/Query/QueryParser.cpp
	BufrParser/Query/ResultSet.h
	BufrParser/Query/ResultSet.cpp
	BufrParser/Query/Target.h
	BufrParser/Query/Tokenizer.h
	BufrParser/Query/Tokenizer.cpp
	BufrParser/Query/SubsetTable.h
	BufrParser/Query/SubsetTable.cpp
	BufrParser/Query/python_bindings.cpp
)

pybind11_add_module(bufr ${_query_srcs})
target_link_libraries(bufr PUBLIC ${_query_libs})
target_compile_definitions(bufr PRIVATE BUILD_PYTHON_BINDING=1)
target_include_directories(bufr PUBLIC
						   $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
						   $<INSTALL_INTERFACE:bufr>  # <prefix>/bufr
						   )

set_target_properties( bufr
					   PROPERTIES
					     ARCHIVE_OUTPUT_DIRECTORY "${PYIODACONV_BUILD_LIBDIR}"
					     LIBRARY_OUTPUT_DIRECTORY "${PYIODACONV_BUILD_LIBDIR}"
					   )

install (TARGETS bufr DESTINATION ${PYIODACONV_INSTALL_LIBDIR})
