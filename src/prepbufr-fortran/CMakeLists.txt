
# include_directories ("${PROJECT_BINARY_DIR}/baselib")
include_directories ("${NETCDFPATH}/include")
include_directories ("${BASEPATH}/src")

SET(SRC_LIST module_obs_base.f90  module_obs_conv_pt.f90  module_prepbufr.f90 module_obsncio.f90)

set_source_files_properties(${SRC_LIST} PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})
set_source_files_properties(get_obs.f90 PROPERTIES COMPILE_FLAGS ${Fortran_FLAGS})

ADD_LIBRARY (obsbase ${SRC_LIST})
TARGET_LINK_LIBRARIES(obsbase ${BASEPATH}/lib/libbase.a ${BASEPATH}/lib/libncio.a)

ADD_EXECUTABLE(get_obs.exe get_obs.f90)
TARGET_LINK_LIBRARIES(get_obs.exe obsbase)
TARGET_LINK_LIBRARIES(get_obs.exe ${BUFRPATH}/libbufr_v.a -L${NETCDFPATH}/lib -lnetcdf -lnetcdff)
