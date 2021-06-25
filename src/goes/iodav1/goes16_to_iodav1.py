#!/usr/bin/env python

from __future__ import print_function
import sys
import os
import re
import argparse
import numpy as np
import netCDF4
from netCDF4 import Dataset
import datetime as dt
import pytz

N_STRING = 20

###################################################################################
# SUBROUTINES
###################################################################################


def CalcLatLon(Fid, ScanAngle, ElevAngle):
    #####################################################################
    # This routine will convert the GEOS-16 fixed grid coordinates
    # into the corresponding Earth latitude and longitude coordinates.
    # The latitude will be the geodetic latitude value.
    #
    # The formulas in this routine are from Paragraph 5.1.2.8, "Navigation of
    # Image Data", from the GEOS-16 document:
    #
    #   "GOES R SERIES PRODUCT DEFINITION AND USERS' GUIDE", vol. 3 (L1b products).
    #
    # For GOES-16 fixed grid:
    #    x is scan angle
    #    y is elevation angle
    #
    # Will need to do conversion from fixed grid coordinates to Earth Lat, Lon.
    # scan angle is related to longitude and elevation angle is related to latitude.

    # Conversion factors between radians and degrees
    Deg2Rad = np.pi / 180.0
    Rad2Deg = 180.0 / np.pi

    Vsize = len(ScanAngle)

    # First step is to obtain parameters from the file
    IprojVar = Fid.variables['goes_imager_projection']

    Req = IprojVar.semi_major_axis
    Rpol = IprojVar.semi_minor_axis
    H = IprojVar.perspective_point_height + IprojVar.semi_major_axis
    Lon0 = IprojVar.longitude_of_projection_origin * Deg2Rad

    Hsqr = H * H
    ReqSqr = Req * Req
    RpolSqr = Rpol * Rpol

    # Sin and Cos factors
    SinX = np.sin(ScanAngle)
    CosX = np.cos(ScanAngle)
    SinY = np.sin(ElevAngle)
    CosY = np.cos(ElevAngle)

    SinXSqr = np.power(SinX, 2.0)
    CosXSqr = np.power(CosX, 2.0)
    SinYSqr = np.power(SinY, 2.0)
    CosYSqr = np.power(CosY, 2.0)

    # Terms for the quadratic root equation below
    a = SinXSqr + (CosXSqr * (CosYSqr + (ReqSqr * SinYSqr / RpolSqr)))
    b = -2.0 * H * CosX * CosY
    c = Hsqr - ReqSqr

    Arg = np.power(b, 2.0) - (4.0 * a * c)
    Rs = ((-1.0 * b) - np.sqrt(Arg)) / (2.0 * a)

    # Satillite axis
    Sx = Rs * CosX * CosY
    Sy = (-1.0 * Rs) * SinX
    Sz = Rs * CosX * SinY

    # lat lon values in degrees
    HminusSx = H - Sx
    Lon = (Lon0 - np.arctan2(Sy, HminusSx)) * Rad2Deg
    Lat = (np.arctan2((ReqSqr * Sz), (RpolSqr * np.sqrt(HminusSx*HminusSx + Sy*Sy)))) * Rad2Deg

    return (Lat, Lon)


def CalcSolarAngles(Lat, Lon, Tref, Toff):
    ############################################################
    # This method will calculate the solar zenith and azimuth
    # angles for the locations on the Earth's surface.
    print("DEBUG: Tref, Toff : ", Tref, Toff)

    Nlocs = len(Lat)

    Tabs = np.array([Tref + dt.timedelta(hours=Toff[i]) for i in range(Nlocs)])
    print("DEBUG: Tabs: ", Tabs)

    SolZen = np.zeros((Nlocs,))
    SolAzim = np.zeros((Nlocs,))

    return (SolZen, SolAzim)


def WriteNcArray(NcGroup, Name, Dims, Values):
    ############################################################
    # This method will write out an array (built with the
    # CreateNcArray method) into the netcdf file.

    # Get the corresponding netcdf data type, and if it's a
    # character array, add the string length dimension to
    # the dims tuple.
    Dtype = NumpyToNcDtype(Values.dtype)
    if (Dtype == 'c'):
        AllDims = Dims + ('nstring',)
    else:
        AllDims = Dims

    NcGroup.createVariable(Name, Dtype, AllDims)
    NcGroup[Name][:] = Values


def NumpyToNcDtype(NumpyDtype):
    ############################################################
    # This method converts the numpy data type to the
    # corresponding netcdf datatype

    if (NumpyDtype == np.dtype('float64')):
        NcDtype = 'f4'    # convert double to float
    elif (NumpyDtype == np.dtype('float32')):
        NcDtype = 'f4'
    elif (NumpyDtype == np.dtype('int64')):
        NcDtype = 'i4'    # convert long to int
    elif (NumpyDtype == np.dtype('int32')):
        NcDtype = 'i4'
    elif (NumpyDtype == np.dtype('S1')):
        NcDtype = 'c'
    else:
        print("ERROR: Unrecognized numpy data type: ", NumpyDtype)
        exit(-2)

    return NcDtype


###################################################################################
# CLASSES
###################################################################################

###################################################################################
# MAIN
###################################################################################


ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()

# Main arguments
ap.add_argument("input_goes16_file", help="path to output ioda file")
ap.add_argument("channel_number", type=int, help="ABI band number")
ap.add_argument("output_ioda_file", help="path to output ioda file")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output file")


MyArgs = ap.parse_args()

InFname = MyArgs.input_goes16_file
ChanNum = MyArgs.channel_number
OutFname = MyArgs.output_ioda_file
ClobberOfile = MyArgs.clobber

# Check files
BadArgs = False

# Verify that the input files exist
if (not os.path.isfile(InFname)):
    print("ERROR: {0:s}: Input file does not exist: {1:s}".format(ScriptName, InFname))
    BadArgs = True

# Verify if okay to write to the output files
if (os.path.isfile(OutFname)):
    if (ClobberOfile):
        print("WARNING: {0:s}: Overwriting ioda file: {1:s}".format(ScriptName, OutFname))
        print("")
    else:
        print("ERROR: {0:s}: Specified ioda file already exists: {1:s}".format(ScriptName, OutFname))
        print("ERROR: {0:s}:   Use -c option to overwrite.".format(ScriptName))
        print("")
        BadArgs = True

if (BadArgs):
    sys.exit(2)

# Everything looks okay, forge on and convert the file.
print("Preparing netcdf files:")
print("  Input goes-16 file: {0:s}".format(InFname))
print("    ABI channel number: {0:d}".format(ChanNum))
print("  Output ioda file: {0:s}".format(OutFname))
print("")

# open the files
InFid = Dataset(InFname, 'r')
OutFid = Dataset(OutFname, 'w', format='NETCDF4')

# The auto scale feature of the netcdf API will do the conversion
# from packed shorts to floats applying the "scale_factor" and
# "add_offset" attribute values.
InFid.set_auto_scale(True)

# First, read in the quality flags in order to identify which locations
# are on the Earth (mask value = False) versus outer space (mask value = True).
# Note that we want to invert the value of the mask (True for locations on
# the Earth).
EarthLocs = ~np.ma.getmaskarray(InFid.variables['DQF'][:])

# Get the time values. The variables inside the file denote the start, middle and
# end times for the scan. These values are offsets from the "epoch" which is
# defined as Jan 1, 2000, 12:00:00 UTC according to the User's manual. Put the time
# values into datetime objects, figure out the year, month, day, hour values, then
# convert the times to offsets from the year, month, day, hour values.
Tepoch = dt.datetime(2000, 1, 1, 12, 0, 0, 0, pytz.UTC)
Tstart = Tepoch + dt.timedelta(seconds=int(round(float(InFid.variables['time_bounds'][0]))))
Tmid = Tepoch + dt.timedelta(seconds=int(round(float(InFid.variables['t'][0]))))
Tend = Tepoch + dt.timedelta(seconds=int(round(float(InFid.variables['time_bounds'][1]))))

# round to the nearest hour
Trefdate = Tmid.replace(minute=0, second=0, microsecond=0)
if (Tmid.minute >= 30):
    Trefdate = Trefdate + dt.timedelta(hours=1)

# Find the beginning and end time offsets in seconds
Ts = (Tstart - Trefdate).total_seconds()
Te = (Tend - Trefdate).total_seconds()

# Divide up the interval into even pieces while rounding to the nearest second.
# This is not exactly correct. The scans for mesoscale, CONUS, and full disk are
# interleaved and don't occur in even deltas. But, there doesn't seem to be
# enough information in the file to work out the exact pixel times.
TimeSteps = np.linspace(Ts, Te, num=EarthLocs.size, dtype=int).reshape(EarthLocs.shape)
TimeSteps = TimeSteps[EarthLocs]
OffsetTime = TimeSteps.astype(float) / 3600.0

# Calculate latitude and longitude values.
#    Read in x (scan angle) and y (elevation angle) from the file.
#    Create a meshed grid using x and y.
#        Order is cartesian (indexing='xy') since the 2D variables are
#        stored in that manner.
#    Use EarthLocs to convert the gridded x, y to vectors of valid locations.
#    Pass the vectors to the CalcLatLon routine in order to
#    pull out the Lat, Lon values for each location.
AbiX = (InFid.variables['x'][:]).data
AbiY = (InFid.variables['y'][:]).data
GridX, GridY = np.meshgrid(AbiX, AbiY, indexing='xy')

SelectedX = GridX[EarthLocs]
SelectedY = GridY[EarthLocs]

(Lat, Lon) = CalcLatLon(InFid, SelectedX, SelectedY)

# Calculate the solar zenith and azimuth angles
# WARNING: CalcSolarAngles is not ready since the calls to pysolar are
# extremely slow.
# (SolZen, SolAzim) = CalcSolarAngles(Lat, Lon, Trefdate, OffsetTime)

# Create dimensions
Nlocs = len(SelectedX)
Nvars = 1
Nrecs = 1
Nobs = Nvars * Nlocs

OutFid.createDimension('nlocs', Nlocs)
OutFid.createDimension('nvars', Nvars)
OutFid.createDimension('nrecs', Nrecs)
OutFid.createDimension('nobs', Nobs)

# Write out the metadata
ScanAngle = SelectedX * 180.0 / np.pi
ElevAngle = SelectedY * 180.0 / np.pi

WriteNcArray(OutFid, "Scan_Angle@MetaData", ('nlocs'), ScanAngle)
WriteNcArray(OutFid, "Elevation_Angle@MetaData", ('nlocs'), ElevAngle)
# WriteNcArray(OutFid, "Sol_Zenith_Angle@MetaData", ('nlocs'), SolZen)
# WriteNcArray(OutFid, "Sol_Azimuth_Angle@MetaData", ('nlocs'), SolAzim)

WriteNcArray(OutFid, "latitude@MetaData", ('nlocs'), Lat)
WriteNcArray(OutFid, "longitude@MetaData", ('nlocs'), Lon)

DateTime = (Trefdate.year*1000000 + Trefdate.month*10000 + Trefdate.day * 100 + Trefdate.hour)
OutFid.setncattr("date_time", DateTime)
WriteNcArray(OutFid, "time@MetaData", ('nlocs'), OffsetTime)

# Copy the radiance and qc mark data to the output
Vname = "radiance_{0:d}_".format(ChanNum)

Var = InFid.variables['Rad'][:][EarthLocs]
WriteNcArray(OutFid, Vname + "@ObsValue", ('nlocs'), Var)

Var = InFid.variables['DQF'][:].astype(int)[EarthLocs]
WriteNcArray(OutFid, Vname + "@PreQC", ('nlocs'), Var)

# Use std deviation of valid pixel for the error variance
ErrStdDev = InFid.variables['std_dev_radiance_value_of_valid_pixels'][0]
Var = np.full((Nlocs,), ErrStdDev**2, dtype='f4')
WriteNcArray(OutFid, Vname + "@ObsError", ('nlocs'), Var)

# clean up
InFid.close()
OutFid.close()
