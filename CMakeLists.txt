# (C) Copyright 2018 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# IODA-CONVERTERS
################################################################################

cmake_minimum_required( VERSION 3.3.2 FATAL_ERROR )

project( iodaconv C CXX Fortran )

set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH} )

set( ECBUILD_DEFAULT_BUILD_TYPE Release )
set( ENABLE_OS_TESTS           OFF CACHE BOOL "Disable OS tests" FORCE )
set( ENABLE_LARGE_FILE_SUPPORT OFF CACHE BOOL "Disable testing of large file support" FORCE )
set( ENABLE_MPI ON CACHE BOOL "Compile with MPI" )

include( ecbuild_system NO_POLICY_SCOPE )
include( GNUInstallDirs )

ecbuild_requires_macro_version( 2.7 )

################################################################################
# Project
################################################################################

ecbuild_declare_project()

ecbuild_enable_fortran( REQUIRED )
ecbuild_add_cxx11_flags()

set( IODACONV_LINKER_LANGUAGE CXX )

################################################################################
# Dependencies
################################################################################

# python
ecbuild_find_python( REQUIRED )

# odb api
ecbuild_add_option( FEATURE ODB_API
                    DEFAULT OFF
                    DESCRIPTION "Support for ODB API"
                    REQUIRED_PACKAGES "PROJECT odb_api  VERSION 0.17" )

if (HAVE_ODB_API)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -DHAVE_ODB_API")
endif (HAVE_ODB_API)

# NetCDF and bufrlib are optional. If found, the following variables are set
# bufrlib_FOUND
# NETCDF4_FOUND
find_package(bufrlib)
find_package(NetCDF COMPONENTS Fortran)
if( bufrlib_FOUND AND NETCDF4_FOUND )
  set( HAVE_BUFRLIB_AND_NETCDF "1" )
  message( STATUS "Fortran components enabled since both bufrlib and NETCDF are found")
else()
  set( HAVE_BUFRLIB_AND_NETCDF "0" )
  message( STATUS "Fortran components disabled since both bufrlib and NETCDF are not found")
endif()

################################################################################
# Export package info
################################################################################

list( APPEND IODACONV_TPLS ) # NetCDF oops )

set( IODACONV_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/src ${CMAKE_CURRENT_BINARY_DIR}/src
                       ${CMAKE_Fortran_MODULE_DIRECTORY} )
set( IODACONV_LIBRARIES ioda )

get_directory_property( IODACONV_DEFINITIONS COMPILE_DEFINITIONS )

foreach( _tpl ${IODACONV_TPLS} )
  string( TOUPPER ${_tpl} TPL )
  list( APPEND IODACONV_EXTRA_DEFINITIONS   ${${TPL}_DEFINITIONS}  ${${TPL}_TPL_DEFINITIONS}  )
  list( APPEND IODACONV_EXTRA_INCLUDE_DIRS  ${${TPL}_INCLUDE_DIRS} ${${TPL}_TPL_INCLUDE_DIRS} )
  list( APPEND IODACONV_EXTRA_LIBRARIES     ${${TPL}_LIBRARIES}    ${${TPL}_TPL_LIBRARIES}    )
endforeach()

if( HAVE_ODB_API )
  list( APPEND IODACONV_EXTRA_LIBRARIES     Odb    )
endif()

# Location of .pycodestyle for norm checking within IODA-converters
set( IODACONV_PYLINT_CFG_DIR ${CMAKE_CURRENT_SOURCE_DIR} )

# Location of installed python iodaconv libraries
set( PYIODACONV_BUILD_LIBDIR   ${CMAKE_BINARY_DIR}/lib/pyiodaconv )
set( PYIODACONV_INSTALL_LIBDIR ${CMAKE_INSTALL_PREFIX}/lib/pyiodaconv )

################################################################################
# Sources
################################################################################

include( iodaconv_compiler_flags )
include_directories( ${IODACONV_INCLUDE_DIRS} ${IODACONV_EXTRA_INCLUDE_DIRS} )
include( iodaconv_extra_macros )
add_subdirectory( src )
add_subdirectory( tools )
add_subdirectory( test )

if(ECBUILD_INSTALL_FORTRAN_MODULES)
  install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/${CMAKE_CFG_INTDIR} DESTINATION ${INSTALL_INCLUDE_DIR} )
endif()

################################################################################
# Finalise configuration
################################################################################

# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME ioda )

# print the summary of the configuration
ecbuild_print_summary()

