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

ecbuild_find_python( REQUIRED VERSION 3 )

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

find_package( ioda_engines QUIET )

find_package( bufr QUIET )
find_package( NetCDF QUIET COMPONENTS Fortran )

## Sources

add_subdirectory( src )
add_subdirectory( tools )
add_subdirectory( test )

# Package Config

ecbuild_install_project( NAME ${PROJECT_NAME} )

ecbuild_print_summary()
