# src/mls/CMakeLists.txt
#
# Build 1) mlsT_dat2ioda binary (*.dat) to ioda2 NetCDF4 converter

list(APPEND mlsT_dat2ioda_source
dtg_module.f90
mls_module.f90
slen.f90
get_ev.f90
read_mlsT_files.f90
mlsT_dat2ioda.f90
  )

ecbuild_add_executable( TARGET  mlsT_dat2ioda
                        SOURCES ${mlsT_dat2ioda_source}
                        LIBS    NetCDF::NetCDF_Fortran)
