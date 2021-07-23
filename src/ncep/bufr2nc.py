#!/usr/bin/env python3

from __future__ import print_function
import ncepbufr
import sys
import os
import argparse
from netCDF4 import Dataset
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import bufr2ncCommon as cm
import bufr2ncObsTypes as ot

###############################################################################
# SUBROUTINES
###############################################################################


def BfilePreprocess(BufrFname, Obs):
    # This routine will read the BUFR file and figure out how many observations
    # will be read when recording data.

    bufr = ncepbufr.open(BufrFname)

    # The number of observations will be equal to the total number of subsets
    # contained in the selected messages.
    NumObs = 0
    Obs.start_msg_selector()
    while (Obs.select_next_msg(bufr)):
        NumObs += Obs.msg_obs_count(bufr)

    bufr.close()

    return [NumObs, Obs.num_msg_selected, Obs.num_msg_mtype]


###############################################################################
# MAIN
###############################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
ap.add_argument("obs_type", help="observation type")
ap.add_argument("input_bufr", help="path to input BUFR file")
ap.add_argument("output_netcdf", help="path to output netCDF4 file")
ap.add_argument("-m", "--maxmsgs", type=int, default=-1,
                help="maximum number of messages to keep", metavar="<max_num_msgs>")
ap.add_argument("-t", "--thin", type=int, default=1,
                help="select every nth message (thinning)", metavar="<thin_interval>")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output netcdf file")
ap.add_argument("-p", "--prepbufr", action="store_true",
                help="input BUFR file is in prepBUFR format")

MyArgs = ap.parse_args()

ObsType = MyArgs.obs_type
BufrFname = MyArgs.input_bufr
NetcdfFname = MyArgs.output_netcdf
MaxNumMsg = MyArgs.maxmsgs
ThinInterval = MyArgs.thin
ClobberOfile = MyArgs.clobber
if (MyArgs.prepbufr):
    BfileType = cm.BFILE_PREPBUFR
else:
    BfileType = cm.BFILE_BUFR

# Check files
BadArgs = False
if (not os.path.isfile(BufrFname)):
    print("ERROR: {0:s}: Specified input BUFR file does not exist: {1:s}".format(
        ScriptName, BufrFname))
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

# Check the observation type, and create an observation instance.
if (ObsType == 'Aircraft'):
    Obs = ot.AircraftObsType(BfileType)
elif (ObsType == 'Sondes'):
    Obs = ot.SondesObsType(BfileType)
elif (ObsType == 'Amsua'):
    Obs = ot.AmsuaObsType(BfileType)
elif (ObsType == 'Gpsro'):
    Obs = ot.GpsroObsType(BfileType)
else:
    print("ERROR: {0:s}: Unknown observation type: {1:s}".format(
        ScriptName, ObsType))
    print("")
    BadArgs = True

if (not BadArgs):
    if (Obs.mtype_re == 'UnDef'):
        if (BfileType == cm.BFILE_BUFR):
            print("ERROR: {0:s}: Observation type {1:s} for BUFR format is undefined".format(
                ScriptName, ObsType))
        elif (BfileType == cm.BFILE_PREPBUFR):
            print("ERROR: {0:s}: Observation type {1:s} for prepBUFR format is undefined".format(
                ScriptName, ObsType))
        print("")
        BadArgs = True

if (BadArgs):
    sys.exit(2)

# Arguments are okay, and we've got an observation object instantiated. Note
# that we need to have the obs object instantiated before calling
# BfilePreprocess() routine below. This is so BfilePreprocess() can select
# messages in the same manner as the subsequent conversion.
print("Converting BUFR to netCDF")
print("  Observation Type: {0:s}".format(ObsType))
if (BfileType == cm.BFILE_BUFR):
    print("  Input BUFR file (BUFR format): {0:s}".format(BufrFname))
elif (BfileType == cm.BFILE_PREPBUFR):
    print("  Input BUFR file (prepBUFR format): {0:s}".format(BufrFname))
print("  Output netCDF file: {0:s}".format(NetcdfFname))
if (MaxNumMsg > 0):
    print(
        "  Limiting nubmer of messages to record to {0:d} messages".format(MaxNumMsg))
if (ThinInterval > 1):
    print("  Thining: selecting every {0:d}-th message".format(ThinInterval))
print("")

# It turns out that using multiple unlimited dimensions in the netCDF file
# can be very detrimental to the file's size, and can also be detrimental
# to the runtime for creating the file.
#
# In order to mitigate this, we want to use fixed size dimensions instead.
# Each obs type object will have its associated dimension sizes defined as
# fixed sizes. The only missing part is how many observations (subsets) will
# be selected.
#
# This number of subsets needs to be determined from reading through all the
# selected messages. Fortunately, this is very fast.
#
# Make a pass through the BUFR file to determine the number of observations
# and the reference time.
#
# BfilePreprocess() will use the regular expression for selecting message
# types. NumObs will be set to the number of observations selected,
# NumMsgs will be set to the number of messages selected, and TotalMsgs will
# be set to the total number of messages that match Obs.mtype_re in the file.
Obs.max_num_msg = MaxNumMsg
Obs.thin_interval = ThinInterval
[NumObs, NumMsgs, TotalMsgs] = BfilePreprocess(BufrFname, Obs)

print("  Total number of messages that match obs type {0:s}: {1:d}".format(
    ObsType, TotalMsgs))
print("  Number of messages selected: {0:d}".format(NumMsgs))
print("  Number of observations selected: {0:d}".format(NumObs))
print("")

# Now that we have the number of observations we will be recording, set the
# dimension size in the obs object. Note the set_nlocs() method needs to be
# called before creating netcdf variables.
Obs.set_nlocs(NumObs)

# Create the dimensions and variables in the netCDF file in preparation for
# recording the selected observations.
nc = Dataset(NetcdfFname, 'w', format='NETCDF4')
Obs.create_nc_datasets(nc)

# Fill in the dimension variables with the coordinate values. Just using dummy
# values for now which are 1..n where n is the size of the coorespoding
# dimension.
Obs.fill_coords(nc)

# Open the BUFR file and initialize the file object.
bufr = ncepbufr.open(BufrFname)

# Run the conversion
Obs.convert(bufr, nc)

# Clean up
bufr.close()

nc.sync()
nc.close()
