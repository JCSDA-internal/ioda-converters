# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.


# IntelLLVM
if(CMAKE_CXX_COMPILER_ID STREQUAL IntelLLVM)
  set(CMAKE_CXX_FLAGS_RELEASE        "-O3 -DNDEBUG"      CACHE STRING "Release C++ compiler flags"                 FORCE)
  set(CMAKE_CXX_FLAGS_BIT            "-O2 -DNDEBUG"      CACHE STRING "Bit-reproducible C++ compiler flags"        FORCE)
  set(CMAKE_CXX_FLAGS_DEBUG          "-O0 -g -traceback" CACHE STRING "Debug C++ compiler flags"                   FORCE)
  set(CMAKE_CXX_FLAGS_PRODUCTION     "-O3 -g"            CACHE STRING "Production C++ compiler flags"              FORCE)
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG"   CACHE STRING "Release-with-debug-info C++ compiler flags" FORCE)
endif()
if(CMAKE_C_COMPILER_ID STREQUAL IntelLLVM)
  set(CMAKE_C_FLAGS_RELEASE        "-O3 -DNDEBUG"      CACHE STRING "Release C compiler flags"                  FORCE)
  set(CMAKE_C_FLAGS_BIT            "-O2 -DNDEBUG"      CACHE STRING "Bit-reproducible C compiler flags"         FORCE)
  set(CMAKE_C_FLAGS_DEBUG          "-O0 -g -traceback" CACHE STRING "Debug C compiler flags"                    FORCE)
  set(CMAKE_C_FLAGS_PRODUCTION     "-O3 -g"            CACHE STRING "Production C compiler flags"               FORCE)
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG"   CACHE STRING "Release-with-debug-info C compiler flags"  FORCE)
endif()
if(CMAKE_Fortran_COMPILER_ID STREQUAL IntelLLVM)
  set(Fortran_AUTOMATIC_ARRAYS_LIMIT 32768)  # (32 kb)
  math(EXPR Fortran_AUTOMATIC_ARRAYS_LIMIT_KB "${Fortran_AUTOMATIC_ARRAYS_LIMIT}/1024")

  set(Fortran_FLAG_STACK_ARRAYS     "-no-heap-arrays")
  set(Fortran_FLAG_AUTOMATIC_ARRAYS "-heap-arrays ${Fortran_AUTOMATIC_ARRAYS_LIMIT_KB}")

  set(CMAKE_Fortran_FLAGS_RELEASE        "-O3 -DNDEBUG -unroll -inline ${Fortran_FLAG_AUTOMATIC_ARRAYS}" CACHE STRING "Release Fortran flags"                 FORCE)
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG ${Fortran_FLAG_AUTOMATIC_ARRAYS}"              CACHE STRING "Release-with-debug-info Fortran flags" FORCE)
  set(CMAKE_Fortran_FLAGS_BIT            "-O2 -DNDEBUG -unroll -inline ${Fortran_FLAG_AUTOMATIC_ARRAYS}" CACHE STRING "Bit-reproducible Fortran flags"        FORCE)
  set(CMAKE_Fortran_FLAGS_DEBUG          "-O0 -g -traceback ${Fortran_FLAG_AUTOMATIC_ARRAYS} -check all" CACHE STRING "Debug Fortran flags"                   FORCE)
  set(CMAKE_Fortran_FLAGS_PRODUCTION     "-O3 -g ${Fortran_FLAG_AUTOMATIC_ARRAYS}"                       CACHE STRING "Production Fortran compiler flags"     FORCE)
endif()


# NVHPC
if(CMAKE_CXX_COMPILER_ID STREQUAL NVHPC)
  set(CMAKE_CXX_FLAGS_RELEASE        "-O3 -DNDEBUG"      CACHE STRING "Release C++ compiler flags"                 FORCE)
  set(CMAKE_CXX_FLAGS_BIT            "-O2 -DNDEBUG"      CACHE STRING "Bit-reproducible C++ compiler flags"        FORCE)
  set(CMAKE_CXX_FLAGS_DEBUG          "-O0 -g -traceback" CACHE STRING "Debug C++ compiler flags"                   FORCE)
  set(CMAKE_CXX_FLAGS_PRODUCTION     "-O3 -g"            CACHE STRING "Production C++ compiler flags"              FORCE)
  set(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG"   CACHE STRING "Release-with-debug-info C++ compiler flags" FORCE)
endif()
if(CMAKE_C_COMPILER_ID STREQUAL NVHPC)
  set(CMAKE_C_FLAGS_RELEASE        "-O3 -DNDEBUG"      CACHE STRING "Release C compiler flags"                 FORCE)
  set(CMAKE_C_FLAGS_BIT            "-O2 -DNDEBUG"      CACHE STRING "Bit-reproducible C compiler flags"        FORCE)
  set(CMAKE_C_FLAGS_DEBUG          "-O0 -g -traceback" CACHE STRING "Debug C compiler flags"                   FORCE)
  set(CMAKE_C_FLAGS_PRODUCTION     "-O3 -g"            CACHE STRING "Production C compiler flags"              FORCE)
  set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG"   CACHE STRING "Release-with-debug-info C compiler flags" FORCE)
endif()
if(CMAKE_Fortran_COMPILER_ID STREQUAL NVHPC)
  set(CMAKE_Fortran_FLAGS_RELEASE        "-O3 -DNDEBUG"      CACHE STRING "Release Fortran compiler flags"                 FORCE)
  set(CMAKE_Fortran_FLAGS_BIT            "-O2 -DNDEBUG"      CACHE STRING "Bit-reproducible Fortran compiler flags"        FORCE)
  set(CMAKE_Fortran_FLAGS_DEBUG          "-O0 -g -traceback" CACHE STRING "Debug Fortran compiler flags"                   FORCE)
  set(CMAKE_Fortran_FLAGS_PRODUCTION     "-O3 -g"            CACHE STRING "Production Fortran compiler flags"              FORCE)
  set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG"   CACHE STRING "Release-with-debug-info Fortran compiler flags" FORCE)
endif()
