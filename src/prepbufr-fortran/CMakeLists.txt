
set(ENV{CC} "icc")
set(ENV{CXX} "icpc")
set(ENV{FC} "ifort")

cmake_minimum_required (VERSION 2.8)

project (bufr2nc)

enable_language (Fortran)

MESSAGE( STATUS "Binary directory " ${PROJECT_BINARY_DIR})
MESSAGE( STATUS "SOURCE directory " ${PROJECT_SOURCE_DIR})

SET(EXECUTABLE_OUTPUT_PATH ${PROJECT_BINARY_DIR}/bin)
SET(LIBRARY_OUTPUT_PATH ${PROJECT_BINARY_DIR}/lib)
SET(BUFRPATH "/glade/p/ral/jntp/mhu/gsi/v37/build/lib")
SET(NETCDFPATH "/gpfs/u/home/xinzhang/modules/docker_intel")

SET(Fortran_FLAGS "-O0 -fp-model source -check bounds -g -traceback")

# include_directories ("${PROJECT_BINARY_DIR}/baselib")
include_directories ("${NETCDFPATH}/include")

SET(SRC_LIST kinds.f90 module_time.f90 module_obs_base.f90  module_obs_conv_pt.f90  module_prepbufr.f90 module_obsncio.f90)

set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})
set_source_files_properties(get_obs.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_LIBRARY (obsbase ${SRC_LIST})

ADD_EXECUTABLE(get_obs.exe get_obs.f90)
TARGET_LINK_LIBRARIES(get_obs.exe obsbase)
TARGET_LINK_LIBRARIES(get_obs.exe ${BUFRPATH}/libbufr_v.a -L${NETCDFPATH}/lib -lnetcdf -lnetcdff)
