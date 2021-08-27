# (C) Copyright 2018-2020 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# IODA-CONVERTERS
################################################################################

cmake_minimum_required( VERSION 3.12 )

find_package( ecbuild 3 REQUIRED HINTS ${CMAKE_CURENT_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/../ecbuild )

project( iodaconv VERSION 0.0.1 LANGUAGES CXX Fortran )

if( ecbuild_VERSION VERSION_LESS 3.4 )
  include(GNUInstallDirs)
endif()

list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )

include( iodaconv_extra_macros )
include( iodaconv_compiler_flags )

# Location of .pycodestyle for norm checking within IODA-converters
set( IODACONV_PYLINT_CFG_DIR ${CMAKE_CURRENT_SOURCE_DIR} )

# Location of installed python iodaconv libraries
set( PYIODACONV_BUILD_LIBDIR   ${CMAKE_BINARY_DIR}/lib/pyiodaconv )
set( PYIODACONV_INSTALL_LIBDIR ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/pyiodaconv )

## Dependencies

find_package(Python3 REQUIRED COMPONENTS Interpreter Development)

find_package( Eigen3 QUIET NO_MODULE HINTS
    $ENV{Eigen3_ROOT} $ENV{EIGEN3_ROOT} $ENV{Eigen_ROOT} $ENV{EIGEN_ROOT}
    $ENV{Eigen3_PATH} $ENV{EIGEN3_PATH} $ENV{Eigen_PATH} $ENV{EIGEN_PATH} )

if( ecbuild_VERSION VERSION_LESS 3.4 )
  find_package( eckit QUIET )
  find_package( oops QUIET )
else()
  ecbuild_find_package( eckit VERSION 1.13.0 QUIET )
  ecbuild_find_package( oops VERSION 1.0.0 QUIET )
endif()
# Optional: pygrib for python for GRIB1/GRIB2 input files
if ( USE_PYGRIB )
    message(STATUS "Adding python tests that import pygrib")
    set( iodaconv_pygrib_ENABLED True)
else()
    message(STATUS "Skipping python tests that import pygrib")
    set( iodaconv_pygrib_ENABLED False)
endif()

find_package( gsl-lite HINTS $ENV{gsl_lite_DIR} )
find_package( MPI )

find_package( ioda QUIET )

find_package( bufr QUIET )
find_package( NetCDF QUIET COMPONENTS Fortran )

if( bufr_FOUND AND NetCDF_Fortran_FOUND )
    set(iodaconv_gnssro_ENABLED True)
    message(STATUS "Found: bufr")
    message(STATUS "Found: NetCDF::NetCDF_Fortran")
    message(STATUS "Enabled Component: gnssro")
else()
    set(iodaconv_gnssro_ENABLED False)
    if(NOT bufr_FOUND)
        message(STATUS "NOT-Found: bufr - GNSSRO converter disabled")
    endif()
    if(NOT NetCDF_Fortran_FOUND)
        message(STATUS "NOT-Found: NetCDF - GNSSRO converter disabled")
    endif()
    message(STATUS "Disabled Component: gnssro")
endif()

if( eckit_FOUND AND oops_FOUND AND ioda_FOUND AND bufr_FOUND )
    set(iodaconv_bufr_ENABLED True)
    message(STATUS "Found: eckit")
    message(STATUS "Found: oops")
    message(STATUS "Found: ioda")
    message(STATUS "Found: bufr")
    message(STATUS "Enabled Component: bufr")
else()
    set(iodaconv_bufr_ENABLED False)
    if(NOT eckit_FOUND)
        message(STATUS "NOT-Found: eckit - BUFR converter disabled")
    endif()
    if(NOT oops_FOUND)
        message(STATUS "NOT-Found: oops - BUFR converter disabled")
    endif()
    if(NOT ioda_FOUND)
        message(STATUS "NOT-Found: ioda - BUFR converter disabled")
    endif()
    if(NOT bufr_FOUND)
        message(STATUS "NOT-Found: bufr - BUFR converter disabled")
    endif()
    message(STATUS "Disabled Component: bufr")
endif()

if( eckit_FOUND AND oops_FOUND AND ioda_FOUND )
    set(iodaconv_satbias_ENABLED True)
    message(STATUS "Enabled Component: satbias converter")
else()
    set(iodaconv_satbias_ENABLED False)
    message(STATUS "Disabled Component: satbias converter")
endif()

if (NetCDF_Fortran_FOUND )
    set(iodaconv_obserror_ENABLED True)
    message(STATUS "Enabled Component: obserror converter")
else()
    set(iodaconv_obserror_ENABLED False)
    message(STATUS "Disabled Component: obserror converter")
endif()

if( bufr_FOUND AND NetCDF_Fortran_FOUND )
    set(iodaconv_pbfortran_ENABLED True)
    message(STATUS "Found: bufr")
    message(STATUS "Found: NetCDF::NetCDF_Fortran")
    message(STATUS "Enabled Component: pbfortran")
else()
    set(iodaconv_pbfortran_ENABLED False)
    if(NOT bufr_FOUND)
        message(STATUS "NOT-Found: bufr - PrepBUFR-fortran converter disabled")
    endif()
    if(NOT NetCDF_Fortran_FOUND)
        message(STATUS "NOT-Found: NetCDF - PrepBUFR-fortran converter disabled")
    endif()
    message(STATUS "Disabled Component: pbfortran")
endif()

## Sources

add_subdirectory( src )
add_subdirectory( tools )
add_subdirectory( test )
add_subdirectory( doc )

# Package Config

ecbuild_install_project( NAME ${PROJECT_NAME} )

ecbuild_print_summary()
