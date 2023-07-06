# src/ncar-bufr2nc-fortran/CMakeLists.txt
#
# Build NCEP PrepBUFR and BUFR to ioda NetCDF4 converter
#

list(APPEND bufr2nc_source
  define_mod.f90
  gnssro_mod.f90
  kinds.f90
  hsd.f90
  main.f90
  ncio_mod.f90
  netcdf_mod.f90
  prepbufr_mod.f90
  radiance_mod.f90
  satwnd_mod.f90
  ufo_variables_mod.F90
  utils_mod.f90
  )

if(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  add_compile_options(-Xcompiler -mcmodel=medium -Xcompiler "-Wl,--no-relax")
endif()

ecbuild_add_executable( TARGET  bufr2nc_fortran.x
                        SOURCES ${bufr2nc_source}
                        LIBS    bufr::bufr_d
                                NetCDF::NetCDF_Fortran)
