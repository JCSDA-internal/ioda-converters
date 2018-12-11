#!/usr/bin/env python3

import netCDF4
from netCDF4 import Dataset

###################################################################################
# SUBROUTINES
###################################################################################

def WriteNcVar(Gfid, Ofid, OutDest, Vname, Vdtype, Vdims, Vvals):
    ###############################################################
    # This method will write out the variable into the appropriate
    # netcdf files.
    #
    if (OutDest == 'G' or OutDest == 'B'):
        # write to geo file
        Ovar = Gfid.createVariable(Vname, Vdtype, Vdims)
        Ovar[...] = Vvals[...]

    if (OutDest == 'O' or OutDest == 'B'):
        # write to obs file
        Ovar = Ofid.createVariable(Vname, Vdtype, Vdims)
        Ovar[...] = Vvals[...]

