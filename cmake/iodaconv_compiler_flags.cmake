# (C) Copyright 2020-2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

if(NOT CMAKE_BUILD_TYPE MATCHES "Debug")
  add_definitions( -DNDEBUG )
endif()

#######################################################################################
# Fortran
#######################################################################################

if( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
  include( compiler_flags_GNU_Fortran )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
  include( compiler_flags_Intel_Fortran )
else()
  message( STATUS "Fortran compiler with ID ${CMAKE_Fortran_COMPILER_ID} will be used with CMake default options")
endif()


#######################################################################################
# C++
#######################################################################################

if( CMAKE_CXX_COMPILER_ID MATCHES "GNU" )
  include( compiler_flags_GNU_CXX )
elseif( CMAKE_CXX_COMPILER_ID MATCHES "Intel" )
  include( compiler_flags_Intel_CXX )
elseif( CMAKE_CXX_COMPILER_ID MATCHES "Clang" )
  include( compiler_flags_Clang_CXX )
else()
  message( STATUS "C++ compiler with ID ${CMAKE_CXX_COMPILER_ID} will be used with CMake default options")
endif()
