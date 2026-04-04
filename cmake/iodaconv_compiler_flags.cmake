# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.



# Set compiler flags for basic build types,
# for compilers where this is not provided by ecbuild.
include(build_type_compiler_flags)

# Set JEDI's common compiler flags
include(jedi_common_compiler_flags)

# Set IODA-converters-specific compiler flags
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  ecbuild_add_fortran_flags("-ffree-line-length-none")
  ecbuild_add_fortran_flags("-ffpe-trap=invalid,zero,overflow,underflow" BUILD DEBUG)
  # Only for x86_64: allow larger datasets in memory
  if(CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
    ecbuild_add_fortran_flags("-mcmodel=medium")
  endif()
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)  # Intel or IntelLLVM
  # Only for x86_64: allow larger datasets in memory
  if(CMAKE_SYSTEM_PROCESSOR STREQUAL "x86_64")
    ecbuild_add_fortran_flags("-mcmodel=medium")
  endif()
endif()
