# (C) Copyright 2020-2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

####################################################################
# FLAGS COMMON TO ALL BUILD TYPES
####################################################################

if( OpenMP_Fortran_FOUND )
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} OpenMP::OpenMP_Fortran")
endif()

####################################################################
# RELEASE FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -funroll-all-loops -finline-functions")

####################################################################
# DEBUG FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -fcheck=bounds -ffpe-trap=invalid,zero,overflow,underflow -fbacktrace")

####################################################################
# BIT REPRODUCIBLE FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_BIT "-O2 -funroll-all-loops -finline-functions")

####################################################################
# LINK FLAGS
####################################################################

set(CMAKE_Fortran_LINK_FLAGS "")

####################################################################

# Meaning of flags
# ----------------
# -fstack-arrays     : Allocate automatic arrays on the stack (needs large stacksize!!!)
# -funroll-all-loops : Unroll all loops
# -fcheck=bounds     : Bounds checking

