# src/gnssro/CMakeLists.txt
#
# Build gnssro ncepbufr to ioda NetCDF4 converter
#

ecbuild_add_executable( TARGET  gnssro_bufr2ioda
                        SOURCES gnssro_bufr2ioda.f90
                        LIBS    bufr::bufr_4_DA
                                NetCDF::NetCDF_Fortran)
