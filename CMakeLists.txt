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

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_FORTRAN_STANDARD 08)
set(CMAKE_FORTRAN_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_C_EXTENSIONS OFF)
set(CMAKE_FORTRAN_EXTENSIONS OFF)

# Location of .pycodestyle for norm checking within IODA-converters
set( IODACONV_PYLINT_CFG_DIR ${CMAKE_CURRENT_SOURCE_DIR} )

# Python-finding settings
set(Python3_FIND_REGISTRY "LAST")
set(Python3_FIND_FRAMEWORK "LAST")
find_package( Python3 REQUIRED COMPONENTS Interpreter Development)

set(pyver python${Python3_VERSION_MAJOR}.${Python3_VERSION_MINOR})
set(PYDIR ${pyver}/pyiodaconv)

# Location of installed python iodaconv libraries
set( PYIODACONV_BUILD_LIBDIR   ${CMAKE_BINARY_DIR}/lib/${PYDIR} )
set( PYIODACONV_INSTALL_LIBDIR ${CMAKE_INSTALL_PREFIX}/lib/${PYDIR} )

## Dependencies

find_package( pybind11 QUIET)

find_package( Eigen3 QUIET NO_MODULE HINTS
    $ENV{Eigen3_ROOT} $ENV{EIGEN3_ROOT} $ENV{Eigen_ROOT} $ENV{EIGEN_ROOT}
    $ENV{Eigen3_PATH} $ENV{EIGEN3_PATH} $ENV{Eigen_PATH} $ENV{EIGEN_PATH} )

if( ecbuild_VERSION VERSION_LESS 3.4 )
  find_package( eckit QUIET )
  find_package( oops QUIET )
else()
  ecbuild_find_package( eckit VERSION 1.13.0 QUIET )
  ecbuild_find_package( oops QUIET )
endif()
# Optional: pygrib for python for GRIB1/GRIB2 input files
if ( USE_PYGRIB )
    message(STATUS "Adding python tests that import pygrib")
    set( iodaconv_pygrib_ENABLED True)
else()
    message(STATUS "Skipping python tests that import pygrib")
    set( iodaconv_pygrib_ENABLED False)
endif()

find_package( eccodes QUIET)

if ( eccodes_FOUND )
        message(STATUS "Found: eccodes")
        set(iodaconv_eccodes_ENABLED True)
endif()

find_package( cartopy QUIET )

if ( cartopy_FOUND )
        message(STATUS "Found: cartopy")
        set(iodaconv_cartopy_ENABLED True)
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

if( eckit_FOUND AND bufr_FOUND AND pybind11_FOUND  AND TARGET gsl::gsl-lite )
    set(iodaconv_bufr_python_ENABLED True)
    message(STATUS "Found: eckit")
    message(STATUS "Found: bufr")
    message(STATUS "Found: pybind11")
    message(STATUS "Found: gsl-lite")
    message(STATUS "Enabled Component: bufr python")
else()
    set(iodaconv_bufr_python_ENABLED False)
    if(NOT eckit_FOUND)
        message(STATUS "NOT-Found: eckit - BUFR python disabled")
    endif()
    if(NOT bufr_FOUND)
        message(STATUS "NOT-Found: bufr - BUFR python disabled")
    endif()
    if(NOT pybind11_FOUND)
        message(STATUS "NOT-Found: pybind11 - BUFR python disabled")
    endif()
    if(NOT TARGET gsl::gsl-lite)
        message(STATUS "NOT-Found: gsl-lite - BUFR python disabled")
    endif()
    message(STATUS "Disabled Component: bufr python")
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
add_subdirectory( share )
# Package Config

ecbuild_install_project( NAME ${PROJECT_NAME} )

ecbuild_print_summary()

