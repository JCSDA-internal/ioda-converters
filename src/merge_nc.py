#!/usr/bin/env python

from __future__ import print_function
import sys
import os
import re
import argparse
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
sp = ap.add_subparsers(dest="obs_type", help="Observation Types")

# Main arguments
ap.add_argument("output_file", help="path to output file")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output file")

# Arguments for Sondes
sondes_p = sp.add_parser("Sondes", help="Merge sondes files")
sondes_p.add_argument("-t", "--t_file", required=True, help="path to input temperature file")
sondes_p.add_argument("-q", "--q_file", required=True ,help="path to input moisture file")
sondes_p.add_argument("-u", "--u_file", required=True, help="path to input u-wind file")
sondes_p.add_argument("-v", "--v_file", required=True, help="path to input v-wind file")

# Arguments for Amsua
amsua_p = sp.add_parser("Amsua", help="Merge amsu-a files")
amsua_p.add_argument("-r", "--r_file", required=True, help="path to input radiance file")


MyArgs = ap.parse_args()
print(MyArgs)

ObsType = MyArgs.obs_type
OutFname = MyArgs.output_file
ClobberOfile = MyArgs.clobber

# Check files
BadArgs = False

# Verify that the input files exist
if (ObsType == "Sondes"):
    InFnames = { 
      't_file' : MyArgs.t_file,
      'q_file' : MyArgs.q_file,
      'u_file' : MyArgs.u_file,
      'v_file' : MyArgs.v_file
      }

elif (ObsType == "Amsua"):
    InFnames = {
      'r_file' : MyArgs.r_file
      }

for Fname in InFnames.values():
    if (not os.path.isfile(Fname)):
        print("ERROR: {0:s}: Input file does not exist: {1:s}".format(ScriptName, Fname))
        BadArgs = True

# Verify if okay to write to the output file
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

