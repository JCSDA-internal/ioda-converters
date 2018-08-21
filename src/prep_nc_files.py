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
        self.geo_vars = []
        self.obs_vars = []

    ### Methods ###

    ###########################################################
    # Dummy method that returns an error so that developer
    # knows to fill in this method with a obs type specific one.
    def PrepNetcdf(self, *Args):
        print("ERROR: PrepNetcdf method is not defined for obs type: {0:s}".format(self.obs_type))
        sys.exit(1)

#######################################################
############ SONDES OBS TYPE ##########################
#######################################################
class SondesObsType(ObsType):
    ### Constructor ###
    def __init__(self):
        super(SondesObsType, self).__init__()
        self.obs_type = 'Sondes'
        self.geo_vars = {
          'Latitude' : 'latitude',
          'Longitude' : 'longitude',
          'Time' : 'time',
          'Height' : 'height',
          'virtual_temperature' : 'virtual_temperature',
          'atmosphere_ln_pressure_coordinates' : 'atmosphere_ln_pressure_coordinates',
          }
        self.obs_vars = {
          'Latitude' : 'latitude',
          'Longitude' : 'longitude',
          'Time' : 'time',
          'Height' : 'height',
          'Pressure' : 'air_pressure',
          'Observation' : 'air_temperature',
          'Errinv_Final' : 'air_temperature_err_inv',
          'Setup_QC_Mark' : 'air_temperature_qc',
          }

#######################################################
############ AMSU-A OBS TYPE ##########################
#######################################################
class AmsuaObsType(ObsType):
    ### Constructor ###
    def __init__(self):
        super(AmsuaObsType, self).__init__()
        self.obs_type = 'Amsua'
        self.geo_vars = {
          'Latitude' : 'latitude',
          'Longitude' : 'longitude',
          'Obs_Time' : 'time',
          'Elevation' : 'height',
          'virtual_temperature' : 'virtual_temperature',
          'humidity_mixing_ratio' : 'humidity_mixing_ratio',
          'air_pressure' : 'air_pressure',
          'air_pressure_levels' : 'air_pressure_levels',
          'mass_concentration_of_ozone_in_air' : 'mass_concentration_of_ozone_in_air',
          'mass_concentration_of_carbon_dioxide_in_air' : 'mass_concentration_of_carbon_dioxide_in_air',
          'atmosphere_mass_content_of_cloud_liquid_water' : 'atmosphere_mass_content_of_cloud_liquid_water',
          'atmosphere_mass_content_of_cloud_ice' : 'atmosphere_mass_content_of_cloud_ice',
          'effective_radius_of_cloud_liquid_water_particle' : 'effective_radius_of_cloud_liquid_water_particle',
          'effective_radius_of_cloud_ice_particle' : 'effective_radius_of_cloud_ice_particle',
          'Water_Fraction' : 'Water_Fraction',
          'Land_Fraction' : 'Land_Fraction',
          'Ice_Fraction' : 'Ice_Fraction',
          'Snow_Fraction' : 'Snow_Fraction',
          'Water_Temperature' : 'Water_Temperature',
          'Land_Temperature' : 'Land_Temperature',
          'Ice_Temperature' : 'Ice_Temperature',
          'Snow_Temperature' : 'Snow_Temperature',
          'Vegetation_Fraction' : 'Vegetation_Fraction',
          'Sfc_Wind_Speed' : 'Sfc_Wind_Speed',
          'Sfc_Wind_Direction' : 'Sfc_Wind_Direction',
          'Lai' : 'Lai',
          'Soil_Moisture' : 'Soil_Moisture',
          'Soil_Temperature' : 'Soil_Temperature',
          'Land_Type_Index' : 'Land_Type_Index',
          'Vegetation_Type' : 'Vegetation_Type',
          'Soil_Type' : 'Soil_Type',
          'Snow_Depth' : 'Snow_Depth'
          }
        self.obs_vars = {
          'Latitude' : 'latitude',
          'Longitude' : 'longitude',
          'Obs_Time' : 'time',
          'Elevation' : 'height',
          'Observation' : 'brightness_temperature',
          'Inverse_Observation_Error' : 'brightness_temperature_err_inv',
          'QC_Flag' : 'brightness_temperature_qc',
          'Sat_Zenith_Angle' : 'Sat_Zenith_Angle',
          'Sol_Zenith_Angle' : 'Sol_Zenith_Angle',
          'Sat_Azimuth_Angle' : 'Sat_Azimuth_Angle',
          'Sol_Azimuth_Angle' : 'Sol_Azimuth_Angle',
          'Scan_Position' : 'Scan_Position',
          'Scan_Angle' : 'Scan_Angle'
          }

    ### Methods ###
    def PrepNetcdf(self, InFnames, OutGeoFname, OutObsFname):
        # This method is used to reformat an AMSU-A netcdf file containing
        # both geovals and observations into two netcdf files, one containing
        # geovals and the other containing observations.
        #
        # The data comes in vectors where each vector contains obs values
        # for the first location for channels 1..n, then the obs values for
        # the second location for channels 1..n, etc.
        #
        # This script will rearrange the data as if each channel is a separate
        # variable. Ie, the data will be reshape into 2D data where the rows
        # correspond to locations and the columns correspond to channel numbers.
        # Some data is repeated (eg, Latitude) where you get the same value for
        # every channel on a given location. These cases will be collapsed into
        # a vector that holds the unique values.

        Rfname = InFnames['r_file']
        print("Reading netcdf file: {0:s}".format(Rfname))

        # Open up the netcdf files
        Rfid = Dataset(Rfname, 'r')
        Gfid = Dataset(OutGeoFname, 'w', format='NETCDF4')
        Ofid = Dataset(OutObsFname, 'w', format='NETCDF4')

        # Copy over the file attributes
        for Attr in Rfid.ncattrs():
            Gfid.setncattr(Attr, Rfid.getncattr(Attr))
            Ofid.setncattr(Attr, Rfid.getncattr(Attr))

        # Copy over the dimensions. Create a new dimension 'nrecs' which will be used
        # as for the rows in the 2D tables.
        for Dim in Rfid.dimensions.values():
            Gfid.createDimension(Dim.name, Dim.size)
            Ofid.createDimension(Dim.name, Dim.size)

        # Create a new dimension, called 'nrecs', that will represent the number
        # of rows in the 2D tables.
        Nchans = len(Rfid.dimensions['nchans'])
        Nobs = len(Rfid.dimensions['nobs'])
        Nrecs = Nobs // Nchans

        Gfid.createDimension('nrecs', Nrecs)
        Ofid.createDimension('nrecs', Nrecs)

        # Walk through all of the variables and:
        #   1. Determine if the true shape is 1D (values repeat across channels)
        #      or 2D (values are unique across channels).
        #   2. Decide to which output file the variable belongs, and write accordingly.
        for Var in Rfid.variables.values():
            if (Var.dimensions[0] == 'nchans'):
                # channel information, copy to both files
                Gvar = Gfid.createVariable(Var.name, Var.dtype, Var.dimensions)
                Ovar = Ofid.createVariable(Var.name, Var.dtype, Var.dimensions)
                Gvar[:] = Var[:]
                Ovar[:] = Var[:]
            else:
                # obs or geovals data, need to first collapse data then copy
                # to appropriate file
                [ VarVals, VarWasCollapsed ] = self.CollapseVar(Var, Nrecs, Nchans)

                # determine where variable belongs
                VarToGeo = (Var.name in self.geo_vars.keys())
                VarToObs = (Var.name in self.obs_vars.keys())

                # determine var dimensions
                #
                # From the input netcdf, the variables that are selected here have
                # 'nobs' as their first dimension. If the var was collapsed, want to
                # replace 'nobs' with 'nrecs'. If the var was not collapsed, want to
                # replace 'nobs' with 'nrecs', 'nchans'. Any additional dimensions after
                # 'nobs' on the input variable need to be copied to the output dimensions.
                VarDims = [ 'nrecs' ]
                if (not VarWasCollapsed):
                    VarDims.append('nchans')
                for i in range(1,Var.ndim):
                    VarDims.append(Var.dimensions[i])

                # Write variable into selected file(s) 
                if (VarToGeo):
                    Gvar = Gfid.createVariable(self.geo_vars[Var.name], Var.dtype, VarDims)
                    Gvar[...] = VarVals

                if (VarToObs):
                    Ovar = Ofid.createVariable(self.obs_vars[Var.name], Var.dtype, VarDims)
                    Ovar[...] = VarVals


        Rfid.close()
        Gfid.close()
        Ofid.close()

    def CollapseVar(self, Var, Nrecs, Nchans):
        ###############################################################
        # This method will check to see if a variable can be
        # "collapsed" and return either the 2D (not collapsible) or
        # 1D (collapsible) version of the variable.
        #
        # Reshape the variable into Nrecs by Nchans. Then collapse
        # the variable if appropriate. Use the trick of converting the
        # first row to a set and if the length of the set is 1, then 
        # all the values were matching.
        if (Var.ndim == 1):
            OutVals = Var[:].reshape(Nrecs, Nchans)
            CollapseVar = self.IsCollapsible(OutVals)
            if (CollapseVar):
                OutVals = np.squeeze(OutVals[:,0])
        elif (Var.ndim == 2):
            N1 = Var.shape[1]
            OutVals = Var[:].reshape(Nrecs, Nchans, N1)
            CollapseVar = self.IsCollapsible(OutVals)
            if (CollapseVar):
                OutVals = np.squeeze(OutVals[:,0,:])

        return [ OutVals, CollapseVar ]

    def IsCollapsible(self, Vals):
        ###############################################################
        # This method will check to see if a variable can be collapsed.
        # Collapsibility is true when all channels in a given row
        # have matching values. This method assumes if the first row
        # contains all matching values, then this will be true for all
        # rows.
        ItemsMatch = True

        # Test "row" by row
        for i in range(Vals.shape[0]):
            TestVals = np.squeeze(Vals[i,...])
            if (TestVals.ndim == 1):
                # reshape so that TestVals is always 2D
                TestVals = TestVals.reshape(( len(TestVals), 1 ))
            [ N1, N2 ] = TestVals.shape

            # Make sure items all match for every column
            for j in range(N2):
                TestVector = np.squeeze(TestVals[:,j])
                if (len(set(TestVector)) > 1):
                    ItemsMatch = False
                    break

            if (not ItemsMatch):
                break

        return ItemsMatch

###################################################################################
# MAIN
###################################################################################
ScriptName = os.path.basename(sys.argv[0])

# Parse command line
ap = argparse.ArgumentParser()
sp = ap.add_subparsers(dest="obs_type", help="Observation Types")

# Main arguments
ap.add_argument("output_geo_file", help="path to output geovals file")
ap.add_argument("output_obs_file", help="path to output observations file")
ap.add_argument("-c", "--clobber", action="store_true",
                help="allow overwrite of output file")

# Arguments for Sondes
sondes_p = sp.add_parser("Sondes", help="Merge sondes files")
sondes_p.add_argument("-t", "--t_file", required=True, help="path to input temperature file")
sondes_p.add_argument("-q", "--q_file", required=True, help="path to input moisture file")
sondes_p.add_argument("-u", "--u_file", required=True, help="path to input u-wind file")
sondes_p.add_argument("-v", "--v_file", required=True, help="path to input v-wind file")

# Arguments for Amsua
amsua_p = sp.add_parser("Amsua", help="Merge amsu-a files")
amsua_p.add_argument("-r", "--r_file", required=True, help="path to input radiance file")


MyArgs = ap.parse_args()

ObsType = MyArgs.obs_type
OutGeoFname = MyArgs.output_geo_file
OutObsFname = MyArgs.output_obs_file
ClobberOfile = MyArgs.clobber

# Check files
BadArgs = False

# Instantiate an obs type object, and set up the expected input
# file names.
if (ObsType == "Sondes"):
    Obs = SondesObsType()
    InFnames = { 
      't_file' : MyArgs.t_file,
      'q_file' : MyArgs.q_file,
      'u_file' : MyArgs.u_file,
      'v_file' : MyArgs.v_file
      }

elif (ObsType == "Amsua"):
    Obs = AmsuaObsType()
    InFnames = {
      'r_file' : MyArgs.r_file
      }

# Verify that the input files exist
for Fname in InFnames.values():
    if (not os.path.isfile(Fname)):
        print("ERROR: {0:s}: Input file does not exist: {1:s}".format(ScriptName, Fname))
        BadArgs = True

# Verify if okay to write to the output files
if (os.path.isfile(OutGeoFname)):
    if (ClobberOfile):
        print("WARNING: {0:s}: Overwriting nc file: {1:s}".format(ScriptName, OutGeoFname))
        print("")
    else:
        print("ERROR: {0:s}: Specified nc file already exists: {1:s}".format(ScriptName, OutGeoFname))
        print("ERROR: {0:s}:   Use -c option to overwrite.".format(ScriptName))
        print("")
        BadArgs = True

if (os.path.isfile(OutGeoFname)):
    if (ClobberOfile):
        print("WARNING: {0:s}: Overwriting nc file: {1:s}".format(ScriptName, OutObsFname))
        print("")
    else:
        print("ERROR: {0:s}: Specified nc file already exists: {1:s}".format(ScriptName, OutObsFname))
        print("ERROR: {0:s}:   Use -c option to overwrite.".format(ScriptName))
        print("")
        BadArgs = True

if (BadArgs):
    sys.exit(2)

# Everything looks okay, forge on and merge the files.

# First, read in all of the netcdf files and merge into a dictionary that keeps
# track of groupings by station id, flight number data, etc.
print("Merging input files:")
print("  Output geovals file: {0:s}".format(OutGeoFname))
print("  Output observations file: {0:s}".format(OutObsFname))
print("")

Obs.PrepNetcdf(InFnames, OutGeoFname, OutObsFname)


