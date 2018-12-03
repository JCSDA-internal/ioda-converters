#!/usr/bin/env python

import sys
import os
import argparse
import netCDF4
from netCDF4 import Dataset

###################################################################################
# MAIN
###################################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
#ap.add_argument("obs_type", help="observation type")
ap.add_argument("input_odb2", help="path to input Met Office ODB API file")
ap.add_argument("output_netcdf", help="path to output netCDF4 file")
# ap.add_argument("-m", "--maxmsgs", type=int, default=-1,
#                 help="maximum number of messages to keep", metavar="<max_num_msgs>")
# ap.add_argument("-t", "--thin", type=int, default=1,
#                 help="select every nth message (thinning)", metavar="<thin_interval>")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output netcdf file")

MyArgs = ap.parse_args()

#ObsType = MyArgs.obs_type
Odb2Fname = MyArgs.input_odb2
NetcdfFname = MyArgs.output_netcdf
#MaxNumMsg = MyArgs.maxmsgs
#ThinInterval = MyArgs.thin
ClobberOfile = MyArgs.clobber

# Check files
BadArgs = False
if (not os.path.isfile(Odb2Fname)): 
    print("ERROR: {0:s}: Specified input file does not exist: {1:s}".format(ScriptName, Odb2Fname))
    print("")
    BadArgs = True

if (os.path.isfile(NetcdfFname)):
    if (ClobberOfile):
        print("WARNING: {0:s}: Overwriting nc file: {1:s}".format(ScriptName, NetcdfFname))
        print("")
    else:
        print("ERROR: {0:s}: Specified nc file already exists: {1:s}".format(ScriptName, NetcdfFname))
        print("ERROR: {0:s}:   Use -c option to overwrite.".format(ScriptName))
        print("")
        BadArgs = True

if (BadArgs):
    sys.exit(2)
