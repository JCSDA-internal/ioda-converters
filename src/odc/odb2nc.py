#!/usr/bin/env python

# Find python runtime dependency paths.
import sys
import os
from pathlib import Path
import argparse
import codc as odc
import xarray as xr


###################################################################################
# SUBROUTINES
###################################################################################

###################################################################################
# MAIN
###################################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
ap.add_argument("input_odb", help="path to input ODB file")
ap.add_argument("output_netcdf", help="path to output netCDF4 file")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output netcdf file")

MyArgs = ap.parse_args()

OdbFname = MyArgs.input_odb
NetcdfFname = MyArgs.output_netcdf
ClobberOfile = MyArgs.clobber

# Check files
BadArgs = False
if (not os.path.isfile(OdbFname)):
    print("ERROR: {0:s}: Specified input ODB file does not exist: {1:s}".format(
        ScriptName, OdbFname))
    print("")
    BadArgs = True

if (os.path.isfile(NetcdfFname)):
    if (ClobberOfile):
        print("WARNING: {0:s}: Overwriting nc file: {1:s}".format(
            ScriptName, NetcdfFname))
        print("")
    else:
        print("ERROR: {0:s}: Specified nc file already exists: {1:s}".format(
            ScriptName, NetcdfFname))
        print("ERROR: {0:s}:   Use -c option to overwrite.".format(ScriptName))
        print("")
        BadArgs = True

if (BadArgs):
    sys.exit(2)

# Arguments are okay. Read contents of odb file into pandas dataframe using the codc
# interface. Then convert the dataframe to an xarray, and use xarray's to_netcdf method
# to create the output file.
#
# This process will convert the entire contents of the input ODB file to netcdf.

# Create the pandas dataframe. 'rb' mode on open method is "read, binary".
for idx, Dframe in enumerate(odc.read_odb(OdbFname)):
    print("Decoded data frame:", idx)

    (Nrows, Ncols) = Dframe.shape
    print("    Nrows: ", Nrows)
    print("    Ncols: ", Ncols)

    # Convert to an xarray dataset. Then remove the _FillValue attributes from all variables.
    XrDset = Dframe.to_xarray()
    VarEncoding = {}
    for Var in XrDset.items():
        VarEncoding[Var[0]] = {
            '_FillValue': None,
            'zlib': True,
            'complevel': 6
        }

    # Dump out to netcdf
    XrDset.to_netcdf(NetcdfFname, format='NETCDF4', encoding=VarEncoding)
