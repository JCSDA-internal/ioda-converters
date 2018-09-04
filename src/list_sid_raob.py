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
ap.add_argument("-o", "--out_file", required=True, help="output text file")
ap.add_argument("input_file", nargs="+", help="list of input netcdf files")


MyArgs = ap.parse_args()

InFileList = MyArgs.input_file
OutFname = MyArgs.out_file

# Everything looks okay, forge on and concatenate the files.
print("Looking for common station id's in the input files:")
print("")

# Read the variable "Station_ID" from the input files.
# Keep track of the station id strings and which input files contained them.
# 

StationIdList = { }

Nfiles = 0
for InFname in InFileList:
    print("Reading: {0:s}".format(InFname))
    Nfiles += 1
    nc = Dataset(InFname, 'r')
  
    # Get the station id variable
    StIds = nc.variables['Station_ID'][...].data
    for Sid in StIds:
        StrStId = Sid.tostring().decode("ascii").strip()
        if StrStId in StationIdList:
            StationIdList[StrStId].add(Nfiles)
        else:
            StationIdList[StrStId] = set([ Nfiles ])
  
    nc.close()

print("")

print("Writing: {0:s}".format(OutFname))
Ofid = open(OutFname, 'w')
for StrStId, FileSet in StationIdList.items():
    if (len(FileSet) == Nfiles):
        # Station ID appeared in all input files
        Ofid.write("{0:s}\n".format(StrStId))

Ofid.close()
