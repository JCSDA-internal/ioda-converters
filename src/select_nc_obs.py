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
############ SONDES OBS TYPE ##########################
#######################################################
class SondesObsType(ObsType):
    ### Constructor ###
    def __init__(self):
        super(SondesObsType, self).__init__()
        self.obs_type = 'Sondes'

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
    def CalcNobsIndex(self, NcIn, NumRecsSelect):
         # Need to keep channel groupings together. Vars are 1D arrays
         # where values are ch1, ch2, ch3, ..., chN, where N is nchans.

         Nobs = NcIn.dimensions['nobs'].size
         Nchans = NcIn.dimensions['nchans'].size
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
ap.add_argument("in_file", help="list of input netcdf files")
ap.add_argument("out_file", help="path to output netcdf file")

sondes_p = sp.add_parser("Sondes", help="Select from sondes file")

amsua_p = sp.add_parser("Amsua", help="Select from amsu-a file")


MyArgs = ap.parse_args()

ClobberOfile = MyArgs.clobber
NumRecsSelect = int(MyArgs.num_recs)
ObsType = MyArgs.obs_type
InFname = MyArgs.in_file
OutFname = MyArgs.out_file

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

# ObsType arg gets checked for valid value via the argparse methods.
if (ObsType == 'Sondes'):
    Obs = SondesObsType()
elif (ObsType == 'Amsua'):
    Obs = AmsuaObsType()

# Everything looks okay, forge on and concatenate the files.
print("Selecting observations:")
print("  Input file: {0:s}".format(InFname))
print("  Output file: {0:s}".format(OutFname))
print("  Observation type: {0:s}".format(ObsType))
print("  Number of observation records to select: {0:d}".format(NumRecsSelect))
print("")


# Copy all attributes and dimensions
# Figure out a step size through the input file and copy obs that are spaced
# out throughout the file.
NcIn = Dataset(InFname, 'r')
NcOut = Dataset(OutFname, 'w', format='NETCDF4')

# Figure out how to select obs from the nobs dimension
NobsIndex = Obs.CalcNobsIndex(NcIn, NumRecsSelect)

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

### # Collect up the contents of the files into a dictionary. Then write out the contents
### # of the dictionary. This assumes that we are using this script for the contrived
### # geovals/obs netcdf from GSI runs of which do not get to large sizes.
### 
### Attributes = { }
### Dimensions = { }
### Variables = { }
### VarDims = { }
### 
### for InFname in InFileList:
###     print("Reading: {0:s}".format(InFname))
###     # Open the file and append the data to the ongoing lists
###     nc = Dataset(InFname, 'r')
###   
###     # Attributes
###     #
###     # The attributes should be matching for every file.
###     for Aname in nc.ncattrs():
###         Avalue = nc.getncattr(Aname)
###         if Aname in Attributes:
###             if (Avalue != Attributes[Aname]):
###                 print("WARNING: Attributes do not match:")
###                 print("WARNING:     Input file: {0:s}".format(InFname))
###                 print("WARNING:     Attribute: {0:s}".format(Aname))
###                 print("WARNING:     Stored value: ", Attributes[Aname])
###                 print("WARNING:     Input file value: ", Avalue)
###         else:
###             Attributes[Aname] = Avalue
###   
###     # Dimensions
###     #
###     # We want to append along the nobs dimension, but keep the other dimensions the
###     # same. Only the nobs dimension should be changing size from file to file.
###     for Dkey, Dim in nc.dimensions.items():
###         if (Dim.name in Dimensions):
###             if (Dim.name == 'nobs'):
###                 Dimensions[Dim.name] += Dim.size
###             else:
###                 if (Dim.size != Dimensions[Dim.name]):
###                     print("WARNING: Dimension sizes do not match:")
###                     print("WARNING:     Input file: {0:s}".format(InFname))
###                     print("WARNING:     Dimension: {0:s}".format(Dim.name))
###                     print("WARNING:     Stored dimension size: ", Dimensions[Dim.name])
###                     print("WARNING:     Input file dimension size: ", Dim.size)
###         else:
###             Dimensions[Dim.name] = Dim.size
###        
###     # Variables
###     for Vkey, Var in nc.variables.items():
###         if (Var.name in Variables):
###             if (Var.dimensions[0] == 'nobs'):
###                 Vvalues = Var[...].data
###                 Variables[Var.name] = np.concatenate([ Variables[Var.name], Vvalues ], axis=0)
###         else:
###             Vvalues = Var[...].data
###             Variables[Var.name] = Vvalues
###             VarDims[Var.name] = Var.dimensions
###   
###     nc.close()
### 
### print("")
### 
### 
### # Write out the dictionary into the output file.
### print("Writing: {0:s}".format(OutFname))
### nc = Dataset(OutFname, 'w', format='NETCDF4')
### 
### # Attributes
### for Aname, Avalue in Attributes.items():
###     nc.setncattr(Aname, Avalue)
### 
### # Dimensions
### for Dname, Dsize in Dimensions.items():
###     nc.createDimension(Dname, Dsize)
### 
### # Variables
### for Vname, Vvalue in Variables.items():
###     Var = nc.createVariable(Vname, Vvalue.dtype, VarDims[Vname])
###     Var[...] = Vvalue[...]
### 
### nc.close()
