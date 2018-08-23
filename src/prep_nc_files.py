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
        self.copy_vars = {
          #  input var name : [ output destination, is collapsible, output destination ]
          #
          #     output destination: B - both files, G - geovar file, O - observation file

          # both files
          'Latitude'  : 'latitude',
          'Longitude' : 'longitude',
          'Time'      : 'time',
          'Height'    : 'height',

          # geovals file only
          'virtual_temperature' : 'virtual_temperature',
          'atmosphere_ln_pressure_coordinates' : 'atmosphere_ln_pressure_coordinates',

          # obs file only
          'Pressure'      : 'air_pressure',
          'Observation_T' : 'air_temperature',
          'Error_Final'   : 'air_temperature_err',
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
        self.copy_vars = {
          #  input var name : [ output destination, is collapsible, output destination ]
          #
          #     output destination: B - both files, G - geovar file, O - observation file

          # both files
          'chaninfoidx'     : [ 'B', 'chaninfoidx', False ],
          'frequency'       : [ 'B', 'frequency', False ],
          'polarization'    : [ 'B', 'polarization', False ],
          'wavenumber'      : [ 'B', 'wavenumber', False ],
          'error_variance'  : [ 'B', 'error_variance', False ],
	  'mean_lapse_rate' : [ 'B', 'mean_lapse_rate', False ],
          'use_flag'        : [ 'B', 'use_flag', False ],
          'sensor_chan'     : [ 'B', 'sensor_chan', False ],
          'satinfo_chan'    : [ 'B', 'satinfo_chan', False ], 
          'Latitude'        : [ 'B', 'latitude', True ],
          'Longitude'       : [ 'B', 'longitude', True ],
          'Elevation'       : [ 'B', 'height', True ],
          'Obs_Time'        : [ 'B', 'time', True ],

          # geovar file only
          'Water_Fraction'        : [ 'G', 'Water_Fraction', True ],
          'Land_Fraction'         : [ 'G', 'Land_Fraction', True ],
          'Ice_Fraction'          : [ 'G', 'Ice_Fraction', True ],
          'Snow_Fraction'         : [ 'G', 'Snow_Fraction', True ],
          'Water_Temperature'     : [ 'G', 'Water_Temperature', True ],
          'Land_Temperature'      : [ 'G', 'Land_Temperature', True ],
          'Ice_Temperature'       : [ 'G', 'Ice_Temperature', True ],
          'Snow_Temperature'      : [ 'G', 'Snow_Temperature', True ],
          'Soil_Temperature'      : [ 'G', 'Soil_Temperature', True ],
          'Soil_Moisture'         : [ 'G', 'Soil_Moisture', True ],
          'Land_Type_Index'       : [ 'G', 'Land_Type_Index', True ],
          'Vegetation_Fraction'   : [ 'G', 'Vegetation_Fraction', True ],
          'Snow_Depth'            : [ 'G', 'Snow_Depth', True ],
          'Sfc_Wind_Speed'        : [ 'G', 'Sfc_Wind_Speed', True ],
          'Vegetation_Type'       : [ 'G', 'Vegetation_Type', True ],
          'Lai'                   : [ 'G', 'Lai', True ],
          'Soil_Type'             : [ 'G', 'Soil_Type', True ],
          'Sfc_Wind_Direction'    : [ 'G', 'Sfc_Wind_Direction', True ],
          'virtual_temperature'   : [ 'G', 'virtual_temperature', True ],
          'humidity_mixing_ratio' : [ 'G', 'humidity_mixing_ratio', True ],
          'air_pressure'          : [ 'G', 'air_pressure', True ],
          'air_pressure_levels'   : [ 'G', 'air_pressure_levels', True ],
          'mass_concentration_of_ozone_in_air'              : [ 'G', 'mass_concentration_of_ozone_in_air', True ],
          'mass_concentration_of_carbon_dioxide_in_air'     : [ 'G', 'mass_concentration_of_carbon_dioxide_in_air', True ],
          'atmosphere_mass_content_of_cloud_liquid_water'   : [ 'G', 'atmosphere_mass_content_of_cloud_liquid_water', True ],
          'atmosphere_mass_content_of_cloud_ice'            : [ 'G', 'atmosphere_mass_content_of_cloud_ice', True ],
          'effective_radius_of_cloud_liquid_water_particle' : [ 'G', 'effective_radius_of_cloud_liquid_water_particle', True ],
          'effective_radius_of_cloud_ice_particle'          : [ 'G', 'effective_radius_of_cloud_ice_particle', True ],

          # obs file only
          'Scan_Position'     : [ 'O', 'Scan_Position', True ],
          'Sat_Zenith_Angle'  : [ 'O', 'Sat_Zenith_Angle', True ],
          'Sat_Azimuth_Angle' : [ 'O', 'Sat_Azimuth_Angle', True ],
          'Sol_Zenith_Angle'  : [ 'O', 'Sol_Zenith_Angle', True ],
          'Sol_Azimuth_Angle' : [ 'O', 'Sol_Azimuth_Angle', True ],
          'Scan_Angle'        : [ 'O', 'Scan_Angle', True ],
          'Observation'       : [ 'O', 'brightness_temperature', False ],
          'Observation_Error' : [ 'O', 'brightness_temperature_err', False ],
          'QC_Flag'           : [ 'O', 'brightness_temperature_qc', False ]
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
        print("")

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

        # Walk through geo_vars and obs_vars and copy vars to their corresponding
        # output files.
        for InVname in self.copy_vars.keys():
            OutDest       = self.copy_vars[InVname][0]
            OutVname      = self.copy_vars[InVname][1]
            IsCollapsible = self.copy_vars[InVname][2]

            # determine where variable belongs
            VarToGeo = (OutDest == 'G' or OutDest == 'B')
            VarToObs = (OutDest == 'O' or OutDest == 'B')

            if (InVname in Rfid.variables):
                # variable exists in the input file
                Var = Rfid.variables[InVname]

                # Only consider collapsing if the first dimension is 'nobs'
                if (Var.dimensions[0] == 'nobs'):
                    # collapse variable (if collapsible) before copying to the output file
                    if (IsCollapsible):
                        VarVals = self.CollapseVar(Var, Nrecs, Nchans)
                    else:
                        VarVals = Var[...]

                    # Determine var dimensions
                    VarDims = [ 'nrecs' ]
                    if (not IsCollapsible):
                        VarDims.append('nchans')
                    for i in range(1,Var.ndim):
                        VarDims.append(Var.dimensions[i])

                    # Write variable into selected file(s) 
                    if (VarToGeo):
                        Ovar = Gfid.createVariable(OutVname, Var.dtype, VarDims)
                        Ovar[...] = VarVals
    
                    if (VarToObs):
                        Ovar = Ofid.createVariable(OutVname, Var.dtype, VarDims)
                        Ovar[...] = VarVals
                else:
                    # copy variable as is to the output file
                    if (VarToGeo):
                        Ovar = Gfid.createVariable(OutVname, Var.dtype, Var.dimensions)
                        Ovar[...] = Var[...]

                    if (VarToObs):
                        Ovar = Ofid.createVariable(OutVname, Var.dtype, Var.dimensions)
                        Ovar[...] = Var[...]
            else:
                # variable does not exist in the input file
                print("WARNING: Variable '{0:s}' does not exist in the input file".format(InVname))
                print("")

        Rfid.close()
        Gfid.close()
        Ofid.close()

    def CollapseVar(self, Var, Nrecs, Nchans):
        ###############################################################
        # This method will "collapse" a variable.
        #
        # Reshape the variable into Nrecs by Nchans. Then collapse
        # the variable (eliminate the nchans dimension).
        if (Var.ndim == 1):
            OutVals = Var[:].reshape(Nrecs, Nchans)
            OutVals = np.squeeze(OutVals[:,0])
        elif (Var.ndim == 2):
            N1 = Var.shape[1]
            OutVals = Var[:].reshape(Nrecs, Nchans, N1)
            OutVals = np.squeeze(OutVals[:,0,:])

        return OutVals

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
print("Preparing netcdf files:")
print("  Output geovals file: {0:s}".format(OutGeoFname))
print("  Output observations file: {0:s}".format(OutObsFname))
print("")

Obs.PrepNetcdf(InFnames, OutGeoFname, OutObsFname)


