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
# CLASSES
###################################################################################

#####################################################
############ BASE OBS TYPE ##########################
#####################################################
class ObsType(object):
    ### Constructor ###
    def __init__(self):
        self.obs_type = 'UnDef'

    ### Methods ###

    ###########################################################
    # Dummy method that returns an error so that developer
    # knows to fill in this method with a obs type specific one.
    def CalcNobsIndex(self, *Args):
        print("ERROR: CalcNobsIndex method is not defined for obs type: {0:s}".format(self.obs_type))
        sys.exit(1)

#######################################################
############ CONVENTIONAL OBS TYPE ####################
#######################################################
#
# This class serves as a base class for aircraft and sondes
#
class ConvObsType(ObsType):
    ### Constructor ###
    def __init__(self):
        super(ConvObsType, self).__init__()
        self.obs_type = 'Conv'

    ###########################################################
    # This method will cycle through the variables in the input
    # file and select a number of observations from the file
    # for writing into the output file.
    def CalcNobsIndex(self, Fname, IdFname, NumRecsSelect):
        # Read in the Station IDs that are common to all
        # three variable files (t,q,uv). Simply read in the
        # first NumRecsSelect ids and grab those out of the file.
        IdList = [ ]
        Ifid = open(IdFname, 'r')
        for i in range(NumRecsSelect):
            IdList.append(Ifid.readline().strip())
        Ifid.close

        # Read in the Station ID list from the netcdf file.
        nc = Dataset(Fname, 'r')
        StationIds = nc.variables['Station_ID'][...].data
        nc.close()

        IndexList = [ ]
        for i in range(StationIds.shape[0]):
            Id = np.squeeze(StationIds[i,...]).tostring().decode("ascii").strip()
            if (Id in IdList):
                IndexList.append(i)

        NobsIndex = np.array(IndexList)
        return NobsIndex


#######################################################
############ SONDES OBS TYPE ##########################
#######################################################
class SondesObsType(ConvObsType):
    ### Constructor ###
    def __init__(self):
        super(SondesObsType, self).__init__()
        self.obs_type = 'Sondes'

#######################################################
############ AIRCRAFT OBS TYPE ########################
#######################################################
class AircraftObsType(ConvObsType):
    ### Constructor ###
    def __init__(self):
        super(AircraftObsType, self).__init__()
        self.obs_type = 'Aircraft'

#######################################################
############ AMSU-A OBS TYPE ##########################
#######################################################
class AmsuaObsType(ObsType):
    ### Constructor ###
    def __init__(self):
        super(AmsuaObsType, self).__init__()
        self.obs_type = 'Amsua'

    ###########################################################
    # This method will cycle through the variables in the input
    # file and select a number of observations from the file
    # for writing into the output file.
    def CalcNobsIndex(self, Fname, NumRecsSelect):
         # Need to keep channel groupings together. Vars are 1D arrays
         # where values are ch1, ch2, ch3, ..., chN, where N is nchans.
         nc = Dataset(Fname, 'r')
         Nobs = nc.dimensions['nobs'].size
         Nchans = nc.dimensions['nchans'].size
         nc.close()

         Nrecs = Nobs // Nchans    # number of channel groupings

         NrecsSkip = Nrecs // NumRecsSelect # how many records to skip between selections
         IndexList = [ ]
         for i in range(NumRecsSelect):
             IndexStart = i * Nchans * NrecsSkip
             for j in range(Nchans):
                  IndexList.append(IndexStart + j)

         NobsIndex = np.array(IndexList)
         return NobsIndex
    

###################################################################################
# MAIN
###################################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
sp = ap.add_subparsers(dest="obs_type", help="Observation Type")

# Main arguments
ap.add_argument("-c", "--clobber", action="store_true", help="allow overwrite of output file")
ap.add_argument("num_recs", help="number of observation records to select")
ap.add_argument("in_file", help="path to input netcdf file")
ap.add_argument("out_file", help="path to output netcdf file")

sondes_p = sp.add_parser("Sondes", help="Select from sondes file")
sondes_p.add_argument("-s", "--s_file", required=True, help="path to sondes file")

aircraft_p = sp.add_parser("Aircraft", help="Select from aircraft file")
aircraft_p.add_argument("-a", "--a_file", required=True, help="path to aircraft file")

amsua_p = sp.add_parser("Amsua", help="Select from amsu-a file")


MyArgs = ap.parse_args()

ClobberOfile = MyArgs.clobber
NumRecsSelect = int(MyArgs.num_recs)
ObsType = MyArgs.obs_type
InFname = MyArgs.in_file
OutFname = MyArgs.out_file

# Check files
BadArgs = False

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

# Everything looks okay, forge on and concatenate the files.
print("Selecting observations:")
print("  Output file: {0:s}".format(OutFname))
print("  Observation type: {0:s}".format(ObsType))
print("  Number of observation records to select: {0:d}".format(NumRecsSelect))
print("")

# Instantiate an obs type object and calculate the indecies for selection along
# the nobs dimension.
if (ObsType == "Sondes"):
    Obs = SondesObsType()
    SidFname = MyArgs.s_file
    NobsIndex = Obs.CalcNobsIndex(InFname, SidFname, NumRecsSelect)

elif (ObsType == "Aircraft"):
    Obs = AircraftObsType()
    AidFname = MyArgs.a_file
    NobsIndex = Obs.CalcNobsIndex(InFname, AidFname, NumRecsSelect)

elif (ObsType == "Amsua"):
    Obs = AmsuaObsType()
    NobsIndex = Obs.CalcNobsIndex(InFname, NumRecsSelect)


# Copy all attributes and dimensions
# Figure out a step size through the input file and copy obs that are spaced
# out throughout the file.
NcIn = Dataset(InFname, 'r')
NcOut = Dataset(OutFname, 'w', format='NETCDF4')

# Attributes
for Aname in NcIn.ncattrs():
    Avalue = NcIn.getncattr(Aname)
    NcOut.setncattr(Aname, Avalue)

# Dimensions
for Dkey, Dim in NcIn.dimensions.items():
    if (Dim.name == 'nobs'):
        NcOut.createDimension('nobs', NobsIndex.size)
    else:
        NcOut.createDimension(Dim.name, Dim.size)

# Variables
for Vkey, VarIn in NcIn.variables.items():
    Vname = VarIn.name
    Vvalue = VarIn[...].data
    VarOut = NcOut.createVariable(Vname, Vvalue.dtype, VarIn.dimensions)
    if (VarOut.dimensions[0] == 'nobs'):
        VarOut[...] = Vvalue[NobsIndex,...]
    else:
        VarOut[...] = Vvalue[...]


NcIn.close()
NcOut.close()
