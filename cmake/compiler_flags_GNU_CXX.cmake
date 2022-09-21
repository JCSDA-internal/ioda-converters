# (C) Copyright 2020-2020 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

####################################################################
# FLAGS COMMON TO ALL BUILD TYPES
####################################################################

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=gnu++14 -g -Wall -Wno-deprecated-declarations")

####################################################################
# RELEASE FLAGS
####################################################################

set(CMAKE_CXX_FLAGS_RELEASE "-O3")

####################################################################
# DEBUG FLAGS
####################################################################

set(CMAKE_CXX_FLAGS_DEBUG "-O0")

####################################################################
# BIT REPRODUCIBLE FLAGS
####################################################################

set(CMAKE_CXX_FLAGS_BIT "-O2")

####################################################################
# LINK FLAGS
####################################################################

set(CMAKE_CXX_LINK_FLAGS "")

####################################################################

# Meaning of flags
# ----------------
# todo

