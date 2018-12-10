#!/usr/bin/env python3

from __future__ import print_function
import sys
import os
import re
import argparse
import numpy as np
import netCDF4
from netCDF4 import Dataset

###################################################################################
# SUBROUTINES
###################################################################################

###################################################################################
# MAIN
###################################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()

# Main arguments
ap.add_argument("-c", "--clobber", action="store_true", help="allow overwrite of output file")
ap.add_argument("-o", "--outfile", help="path to output netcdf file")
ap.add_argument("input_file", nargs="+", help="list of input netcdf files")


MyArgs = ap.parse_args()

ClobberOfile = MyArgs.clobber
OutFname = MyArgs.outfile
InFileList = MyArgs.input_file

# Check files
BadArgs = False

# Verify if okay to write to the output files
if (os.path.isfile(OutFname)):
    if (ClobberOfile):
        print("WARNING: {0:s}: Overwriting nc file: {1:s}".format(ScriptName, OutFname))
        print("")
    else:
        print("ERROR: {0:s}: Specified nc file already exists: {1:s}".format(ScriptName, OutFname))
        print("ERROR: {0:s}:   Use -c option to overwrite.".format(ScriptName))
        print("")
        BadArgs = True

if (BadArgs):
    sys.exit(2)

# Everything looks okay, forge on and concatenate the files.
print("Concatenating input files:")
print("  Output file: {0:s}".format(OutFname))
print("")

# Collect up the contents of the files into a dictionary. Then write out the contents
# of the dictionary. This assumes that we are using this script for the contrived
# geovals/obs netcdf from GSI runs of which do not get to large sizes.

Attributes = { }
Dimensions = { }
Variables = { }
VarDims = { }

for InFname in InFileList:
    # Skip over empty files
    if (os.stat(InFname).st_size > 0):
        print("Reading: {0:s}".format(InFname))
        # Open the file and append the data to the ongoing lists
        nc = Dataset(InFname, 'r')
      
        # Attributes
        #
        # The attributes should be matching for every file.
        for Aname in nc.ncattrs():
            Avalue = nc.getncattr(Aname)
            if Aname in Attributes:
                if (Avalue != Attributes[Aname]):
                    print("WARNING: Attributes do not match:")
                    print("WARNING:     Input file: {0:s}".format(InFname))
                    print("WARNING:     Attribute: {0:s}".format(Aname))
                    print("WARNING:     Stored value: ", Attributes[Aname])
                    print("WARNING:     Input file value: ", Avalue)
            else:
                Attributes[Aname] = Avalue
      
        # Dimensions
        #
        # We want to append along the nobs dimension, but keep the other dimensions the
        # same. Only the nobs dimension should be changing size from file to file.
        for Dkey, Dim in nc.dimensions.items():
            if (Dim.name in Dimensions):
                if (Dim.name == 'nobs'):
                    Dimensions[Dim.name] += Dim.size
                else:
                    if (Dim.size != Dimensions[Dim.name]):
                        print("WARNING: Dimension sizes do not match:")
                        print("WARNING:     Input file: {0:s}".format(InFname))
                        print("WARNING:     Dimension: {0:s}".format(Dim.name))
                        print("WARNING:     Stored dimension size: ", Dimensions[Dim.name])
                        print("WARNING:     Input file dimension size: ", Dim.size)
            else:
                Dimensions[Dim.name] = Dim.size
           
        # Variables
        for Vkey, Var in nc.variables.items():
            if (Var.name in Variables):
                if (Var.dimensions[0] == 'nobs'):
                    Vvalues = Var[...].data
                    Variables[Var.name] = np.concatenate([ Variables[Var.name], Vvalues ], axis=0)
            else:
                Vvalues = Var[...].data
                Variables[Var.name] = Vvalues
                VarDims[Var.name] = Var.dimensions
      
        nc.close()

print("")


# Write out the dictionary into the output file.
print("Writing: {0:s}".format(OutFname))
nc = Dataset(OutFname, 'w', format='NETCDF4')

# Attributes
for Aname, Avalue in Attributes.items():
    nc.setncattr(Aname, Avalue)

# Dimensions
for Dname, Dsize in Dimensions.items():
    nc.createDimension(Dname, Dsize)

# Variables
for Vname, Vvalue in Variables.items():
    Var = nc.createVariable(Vname, Vvalue.dtype, VarDims[Vname])
    Var[...] = Vvalue[...]

nc.close()
