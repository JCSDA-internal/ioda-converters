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

# Everything looks okay, forge on and merge the files.

# First, read in all of the netcdf files and merge into a dictionary that keeps
# track of groupings by station id, flight number data, etc.
print("Concatenating input files:")
print("  Output file: {0:s}".format(OutFname))
print("")

InData = { 
  'Attributes': { },
  'Dimensions': { },
  'Variables': { },
  }

for InFname in InFileList:
  # Open the file and append the data to the ongoing lists
  nc = Dataset(InFname, 'r')

  # Attributes
  for Aname in nc.ncattrs():
      Avalue = nc.getncattr(Aname)
      print("DEBUG:", Aname, Avalue)

  # Dimensions
  for Dkey, Dim in nc.dimensions.items():
      print("DEBUG:", Dim.name, Dim.size)
     
  # Variables
  for Vkey, Var in nc.variables.items():
      print("DEBUG:", Var.name, Var.size, Var.shape)


