# src/gnssro/CMakeLists.txt
#
# Build 1) gnssro ncepbufr to ioda NetCDF4 converter
#       2) gnssro gsi ncdiag to ioda NetCDF4 converter

ecbuild_add_executable( TARGET  gnssro_bufr2ioda
                        SOURCES gnssro_bufr2ioda.f90
                        LIBS    bufr::bufr_4_DA
                                NetCDF::NetCDF_Fortran)

ecbuild_add_executable( TARGET  gnssro_gsidiag2ioda
                        SOURCES gnssro_gsidiag2ioda.f90
                        LIBS    NetCDF::NetCDF_Fortran)
