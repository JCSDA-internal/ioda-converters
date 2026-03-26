# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.


# Flags exposed via configuration options
ecbuild_add_option(FEATURE WARNINGS
                   DEFAULT ON
                   DESCRIPTION "Add warnings to compiler")

if(HAVE_WARNINGS)
  ecbuild_add_cxx_flags("-Wall" NO_FAIL)
  ecbuild_add_cxx_flags("-Wno-sign-compare" NO_FAIL)
  #ecbuild_add_cxx_flags("-Wextra" NO_FAIL)  # JEDI as a whole is not ready to enable these yet

  #ecbuild_add_fortran_flags("-Wall" NO_FAIL)  # gfortran
  if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)  # Intel or IntelLLVM
    ecbuild_add_fortran_flags("-warn")
  endif()
endif()


# Configure C++ flags

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Clang, Cray -> ecbuild defaults

# GCC
if(CMAKE_CXX_COMPILER_ID STREQUAL GNU)
  ecbuild_add_cxx_flags("-ftrapv" BUILD DEBUG)  # trap on signed integer overflow
endif()

# Intel
if(CMAKE_CXX_COMPILER_ID STREQUAL Intel)
  ecbuild_add_cxx_flags("-fp-trap=common -fp-model=precise" BUILD DEBUG)
endif()
if(CMAKE_CXX_COMPILER_ID STREQUAL IntelLLVM)
  ecbuild_add_cxx_flags("-fp-model=precise" BUILD DEBUG)
endif()

# NVHPC
if(CMAKE_CXX_COMPILER_ID STREQUAL NVHPC)
  ecbuild_add_cxx_flags("-Mbounds -Mchkstk -Ktrap=fp" BUILD DEBUG)
endif()

# Configure C flags

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)


# Configure Fortran flags

set(CMAKE_FORTRAN_STANDARD 08)
set(CMAKE_FORTRAN_STANDARD_REQUIRED ON)
set(CMAKE_FORTRAN_EXTENSIONS OFF)

# Cray -> ecbuild defaults

# GCC
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  ecbuild_add_fortran_flags("-ffpe-trap=invalid,zero,overflow" BUILD DEBUG)
endif()

# Intel
if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)  # Intel or IntelLLVM
  ecbuild_add_fortran_flags("-ftrapuv -fp-model=precise -fpe-all=0" BUILD DEBUG)
endif()

# NAG
if(CMAKE_Fortran_COMPILER_ID STREQUAL NAG)
  set(FORTRAN_LINKER_LANGUAGE "CXX")
endif()

# NVHPC
if(CMAKE_Fortran_COMPILER_ID STREQUAL NVHPC)
  ecbuild_add_fortran_flags("-Mpreprocess")
  ecbuild_add_fortran_flags("-Mbounds -Mchkstk -Ktrap=fp" BUILD DEBUG)
endif()

