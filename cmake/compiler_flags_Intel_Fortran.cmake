# (C) Copyright 2020-2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

####################################################################
# FLAGS COMMON TO ALL BUILD TYPES
####################################################################

if( OpenMP_Fortran_FOUND )
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -traceback")
endif()

####################################################################
# RELEASE FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -ip -unroll -inline -no-heap-arrays")

####################################################################
# DEBUG FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -check bounds -warn -heap-arrays -fpe-all=0 -fpe:0 -check all")

####################################################################
# BIT REPRODUCIBLE FLAGS
####################################################################

set(CMAKE_Fortran_FLAGS_BIT "-O2 -ip -ipo -unroll -inline -no-heap-arrays")

####################################################################
# LINK FLAGS
####################################################################

set(CMAKE_Fortran_LINK_FLAGS "")

####################################################################

# Meaning of flags
# ----------------
# todo
