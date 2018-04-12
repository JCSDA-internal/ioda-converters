#!/usr/bin/env python

from __future__ import print_function
import ncepbufr
import numpy as np
import sys
import os
import re
import argparse
import netCDF4
from netCDF4 import Dataset
import datetime as dt

import bufr2ncCommon as cm
import bufr2ncObsTypes as ot

###################################################################################
# SUBROUTINES
###################################################################################

def FindRefDate(StartDate):
    # This routine will return the next analysis time (0, 6, 12, 18Z) following
    # the date represented in StartDate. StartDate is an integer value of the form
    # yyyymmddhh. If StartDate matches an analysis time, then RefDate will be
    # set to StartDate. This is done so that if the message dates are all the same,
    # which happens a fair amount of the time, then the time offsets stored in the
    # output file will be close to the RefDate.
    #
    # Use datetime structures so that addition will take into account carry
    # over into the next day, month, year when hours are added.
    #
    # Compare the hours field in the start date with the analysis times
    # 0, 6, 12, 18, 24 where 24 is 0Z of the next day. 24 is there to make
    # sure that when subtracting the start date hour value, at least one
    # of the entries in the result will be greater than zero. Then the
    # distance to the next analysis time will be the first entry that is
    # greater than zero in the result of the subtraction (HourDiffs).
    StartDtime = cm.SplitDate(StartDate)
    HourDiffs = np.array([ 0, 6, 12, 18, 24 ]) - StartDtime.hour
    HourInc = int(HourDiffs[HourDiffs >= 0][0])

    # Form the datetime compatible version of HourInc, which can then
    # be added to the start date.
    DtDelta = dt.timedelta(hours=HourInc) 
    RefDtime = StartDtime + DtDelta
    RefDate = cm.MakeDate(RefDtime)

    return RefDate

def BfilePreprocess(BufrFname, Obs, MaxNumMsg):
    # This routine will read the BUFR file and figure out how many observations
    # will be read when recording data.
    #
    # It will also figure out the timestamp to use for the obs reference time.
    # The next analysis time (0, 6, 12, 18Z) past the earliest message date
    # found in the selected messages will be used for the reference time.
    #
    # The msg_date value is an integer in the form of YYYYMMDDHH. Because of
    # this format, the smallest integer is also the earliest date. So we
    # just need to find the minimum date value and then figure out the
    # next analysis time from that value.

    bufr = ncepbufr.open(BufrFname)

    # The number of observations will be equal to the total number of subsets
    # contained in the selected messages.
    TotalNumMsg = 0
    NumMsg = 0 
    NumObs = 0 
    EarliestDate = 9999999999 # A value about 8000 years from now
    LatestDate = 0
    while ( (bufr.advance() == 0) ): 
        # Select only the messages that belong to this observation type
        if (re.search(Obs.mtype_re, bufr.msg_type)):
            TotalNumMsg += 1

            # If MaxNumMsg is less than 1, then select all messages.
            # If MaxNumMsg is >= 1, then select no more than MaxNumMsg
            if ((MaxNumMsg < 1) or (NumMsg < MaxNumMsg)):
                # Attribute "subsets" contains the number of subsets
                # for the current message.
                NumMsg += 1
                NumObs += Obs.msg_obs_count(bufr)

                if (bufr.msg_date < EarliestDate):
                    EarliestDate = bufr.msg_date

    bufr.close()

    return [NumObs, NumMsg, TotalNumMsg, FindRefDate(EarliestDate)] 

###################################################################################
# MAIN
###################################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
ap.add_argument("obs_type", help="observation type")
ap.add_argument("input_bufr", help="path to input BUFR file")
ap.add_argument("output_netcdf", help="path to output netCDF4 file")
ap.add_argument("-m", "--maxmsgs", type=int, default=-1,
                help="maximum number of messages to keep", metavar="<max_num_msgs>")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output netcdf file")
ap.add_argument("-p", "--prepbufr", action="store_true",
                help="input BUFR file is in prepBUFR format")

MyArgs = ap.parse_args()

ObsType = MyArgs.obs_type
BufrFname = MyArgs.input_bufr
NetcdfFname = MyArgs.output_netcdf
MaxNumMsg = MyArgs.maxmsgs
ClobberOfile = MyArgs.clobber
if (MyArgs.prepbufr):
    BfileType = cm.BFILE_PREPBUFR
else:
    BfileType = cm.BFILE_BUFR

# Check files
BadArgs = False
if (not os.path.isfile(BufrFname)): 
    print("ERROR: {0:s}: Specified input BUFR file does not exist: {1:s}".format(ScriptName, BufrFname))
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
    print("ERROR: {0:s}: Unknown observation type: {1:s}".format(ScriptName, ObsType))
    print("")
    BadArgs = True

if (not BadArgs):
    if (Obs.mtype_re == 'UnDef'):
        if (BfileType == cm.BFILE_BUFR):
            print("ERROR: {0:s}: Observation type {1:s} for BUFR format is undefined".format(ScriptName, ObsType))
        elif (BfileType == cm.BFILE_PREPBUFR):
            print("ERROR: {0:s}: Observation type {1:s} for prepBUFR format is undefined".format(ScriptName, ObsType))
        print("")
        BadArgs = True

if (BadArgs):
    sys.exit(2)

# Arguments are okay, and we've got an observation object instantiated. Note that
# we need to have the obs object instantiated before calling BfilePreprocess()
# routine below. This is so BfilePreprocess() can select messages in the
# same manner as the subsequent conversion.
print("Converting BUFR to netCDF")
print("  Observation Type: {0:s}".format(ObsType))
if (BfileType == cm.BFILE_BUFR):
    print("  Input BUFR file (BUFR format): {0:s}".format(BufrFname))
elif (BfileType == cm.BFILE_PREPBUFR):
    print("  Input BUFR file (prepBUFR format): {0:s}".format(BufrFname))
print("  Output netCDF file: {0:s}".format(NetcdfFname))
if (MaxNumMsg > 0):
    print("  Limiting nubmer of messages to record to {0:d} messages".format(MaxNumMsg))
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
# NumMsgs will be set to the number of messages selected, and TotalMsgs
# will be set to the total number of messages that match Obs.mtype_re in the file.
[NumObs, NumMsgs, TotalMsgs, RefDate] = BfilePreprocess(BufrFname, Obs, MaxNumMsg)

print("  Total number of messages that match obs type {0:s}: {1:d}".format(ObsType, TotalMsgs))
print("  Number of messages selected: {0:d}".format(NumMsgs))
print("  Number of observations selected: {0:d}".format(NumObs))
print("  Reference date for observations: {0:d}".format(RefDate))
print("")

# We will use the netCDF4 method date2num to calculate the datetime offset from the
# reference date (RefDate). This method wants a datetime object holding the current
# date and time as the first argument, and a units string holding the reference time.
# The units string is in the form "seconds since YYYY-MM-DD HH:00 UTC" where YYYY, MM,
# DD and HH come from the reference date. Create the units string and add that to the
# Obs object.
RefDtime = cm.SplitDate(RefDate)
TimeUnits = "seconds since %0.4i-%0.2i-%0.2i %0.2i:00 UTC" %(RefDtime.year,
            RefDtime.month, RefDtime.day, RefDtime.hour)
Obs.set_time_units(TimeUnits)

# Now that we have the number of observations we will be recording, set the dimension
# size in the obs object. Note the set_nobs() method needs to be called before creating
# netcdf variables.
Obs.set_nobs(NumObs)

# Create the dimensions and variables in the netCDF file in preparation for
# recording the selected observations.
# Aslo, store the reference date in the netcdf file as an attribute
nc = Dataset(NetcdfFname, 'w', format='NETCDF4')
nc.ref_date = RefDate
Obs.create_nc_datasets(nc)

# Fill in the dimension variables with the coordinate values. Just using dummy values
# for now which are 1..n where n is the size of the coorespoding dimension.
Obs.fill_coords(nc)

# Open the BUFR file and initialize the file object.
bufr = ncepbufr.open(BufrFname)

# Run the conversion
Obs.convert(bufr, nc)

# Clean up
bufr.close()

nc.sync()
nc.close()
