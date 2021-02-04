# (C) Copyright 2017-2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

#
# IODA bundle
#

project( ioda-bundle C CXX Fortran )

cmake_minimum_required( VERSION 3.3.2 FATAL_ERROR )

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

include( ecbuild_bundle )

set( ENABLE_MPI ON CACHE BOOL "Compile with MPI" )

ecbuild_bundle_initialize()

ecbuild_requires_macro_version( 2.7 )

# jedi-cmake is not needed for CI
#ecbuild_bundle( PROJECT jedicmake GIT "https://github.com/JCSDA-internal/jedi-cmake.git" )

# ECMWF libs
# ----------
option("BUNDLE_SKIP_ECKIT" "Don't build eckit" "ON" ) # Skip eckit build unless user passes -DBUNDLE_SKIP_ECKIT=OFF
option("BUNDLE_SKIP_FCKIT" "Don't build fckit" "OFF") # Build fckit unless user passes -DBUNDLE_SKIP_FCKIT=ON
option("BUNDLE_SKIP_ATLAS" "Don't build atlas" "OFF") # Build atlas unless user passes -DBUNDLE_SKIP_ATLAS=ON

ecbuild_bundle( PROJECT eckit GIT "https://github.com/jcsda-internal/eckit.git" )
ecbuild_bundle( PROJECT fckit GIT "https://github.com/jcsda-internal/fckit.git" )
ecbuild_bundle( PROJECT atlas GIT "https://github.com/jcsda-internal/atlas.git" )

# Core JEDI repositories
ecbuild_bundle( PROJECT oops GIT "https://github.com/JCSDA-internal/oops.git" )
ecbuild_bundle( PROJECT ioda GIT "https://github.com/JCSDA-internal/ioda.git" )

# Build IODA converters
ecbuild_bundle( PROJECT iodaconv GIT "https://github.com/JCSDA-internal/ioda-converters.git" )


ecbuild_bundle_finalize()

include(cmake/cdash-integration.cmake)
include(CTest)
