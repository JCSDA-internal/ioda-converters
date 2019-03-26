#!/bin/sh

module load netcdf

ifort -c  read_nc_combine_sst_IODA.f  -I/apps/w3lib/2.0.2-intel/lib `nc-config --fflags --flibs --libs`
ifort -o read_nc_sst_IODA read_nc_combine_sst_IODA.o -L/scratch3/NCEPDEV/nwprod/lib -lw3nco_4 -lw3emc_4  `nc-config --fflags --flibs --libs`
