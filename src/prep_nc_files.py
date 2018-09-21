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
        self.missing_value = 1.0e10

    ### Methods ###

    ###########################################################
    # Dummy method that returns an error so that developer
    # knows to fill in this method with a obs type specific one.
    def PrepNetcdf(self, *Args):
        print("ERROR: PrepNetcdf method is not defined for obs type: {0:s}".format(self.obs_type))
        sys.exit(1)

#######################################################
############ CONVENTIONAL OBS TYPE ####################
#######################################################
#
# This class serves as a base class for aircraft and
# radiosonde
class ConvObsType(ObsType):
    ### Constructor ###
    def __init__(self):
        super(ConvObsType, self).__init__()
        self.obs_type = 'Conv'

        # set up names for dictionary keys
        self.sid   = 'Sid'
        self.selev = 'Selev'
        self.lon   = 'Lon'
        self.lat   = 'Lat'
        self.press = 'Press'
        self.time  = 'Time'
        self.rnum  = 'RecNum'

        self.geot = 'GeoT'
        self.geoq = 'GeoQ'
        self.geou = 'GeoU'
        self.geov = 'GeoV'
        self.geop = 'GeoP'
        self.geop2 = 'GeoP2'

        self.obst = 'ObsT'
        self.obsq = 'ObsQ'
        self.obsu = 'ObsU'
        self.obsv = 'ObsV'

        self.obste = 'ObsTerr'
        self.obsqe = 'ObsQerr'
        self.obsue = 'ObsUerr'
        self.obsve = 'ObsVerr'

        self.obstq = 'ObsTqc'
        self.obsqq = 'ObsQqc'
        self.obsuq = 'ObsUqc'
        self.obsvq = 'ObsVqc'

        self.var_names = {
          self.sid   : 'station_id',
          self.selev : 'station_elevation',

          self.lon   : 'longitude',
          self.lat   : 'latitude',
          self.press : 'air_pressure',
          self.time  : 'time',
          self.rnum  : 'record_number',

          self.geot : 'virtual_temperature',
          self.geoq : 'specific_humidity',
          self.geou : 'eastward_wind',
          self.geov : 'northward_wind',
          self.geop : 'atmosphere_ln_pressure_coordinate',
          self.geop2 : 'atmosphere_pressure_coordinate',

          self.obst : 'air_temperature',
          self.obsq : 'specific_humidity',
          self.obsu : 'eastward_wind',
          self.obsv : 'northward_wind'
          }

        self.var_names[self.obste] = self.var_names[self.obst] + '_err'
        self.var_names[self.obsqe] = self.var_names[self.obsq] + '_err'
        self.var_names[self.obsue] = self.var_names[self.obsu] + '_err'
        self.var_names[self.obsve] = self.var_names[self.obsv] + '_err'

        self.var_names[self.obstq] = self.var_names[self.obst] + '_qc'
        self.var_names[self.obsqq] = self.var_names[self.obsq] + '_qc'
        self.var_names[self.obsuq] = self.var_names[self.obsu] + '_qc'
        self.var_names[self.obsvq] = self.var_names[self.obsv] + '_qc'

    ### Methods ###
    def PrepNetcdf(self, InFnames, OutGeoFname, OutObsFname):
        # This method is used to reformat an AMSU-A netcdf file containing
        # both geovals and observations into two netcdf files, one containing
        # geovals and the other containing observations.
        #
        # The data comes in vectors (indexed by observation number), where the
        # corresponding T,Q,U,V values for a given station id can be different
        # lengths with their points at different locations.
        #
        # Read the input data into a dictionary that can be used to keep the
        # soundings grouped together (identified by station id) and the locations
        # grouped together.

        Tfname = InFnames['t_file']
        Qfname = InFnames['q_file']
        UVfname = InFnames['uv_file']

        ObsData = { }
        ExtraDims = { }

        ## Temperature ##
        print("Reading: {0:s}".format(Tfname))
        Fid = Dataset(Tfname, 'r')

        # grab variables
        Nobs = Fid.dimensions['nobs'].size

        Sid   = Fid.variables['Station_ID'][:]
        Selev = Fid.variables['Station_Elevation'][:]

        Lon  = Fid.variables['Longitude'][:]
        Lat  = Fid.variables['Latitude'][:]
        P    = Fid.variables['Pressure'][:]
        Time = Fid.variables['Time'][:]

        T    = Fid.variables['Observation_T'][:]
        Terr = Fid.variables['Err_Final'][:]
        Tqc  = Fid.variables['Setup_QC_Mark'][:]

        Tvirt  = Fid.variables['virtual_temperature'][:].data
        Pcoord = Fid.variables['atmosphere_ln_pressure_coordinate'][:].data
        Pcoord2 = Fid.variables['atmosphere_pressure_coordinate'][:].data * 1000.0 # kPa to Pa

        # Record extra dimension information
        Dim = Fid.dimensions['Station_ID_maxstrlen']
        ExtraDims[self.sid] = [ Dim.name, Dim.size ]

        Dim = Fid.dimensions['num_profile_levels']
        ExtraDims[self.geot] = [ Dim.name, Dim.size ]
        ExtraDims[self.geop] = [ Dim.name, Dim.size ]
        ExtraDims[self.geop2] = [ Dim.name, Dim.size ]

        # Organize data by record and then by location.
        for i in range(Nobs):
            # Form a "record" key from the station id, and a "location" key from
            # lon,lat,pressure,time.
            RecKey = "{0:s}".format(Sid[i].tostring().decode('ascii').strip())
            LocKey = "{0:f}:{1:f}:{2:f}:{3:f}".format(Lon[i],Lat[i],P[i],Time[i])

            # Observation data
            if (RecKey not in ObsData):
                ObsData[RecKey] = { }

            if (LocKey not in ObsData[RecKey]):
                ObsData[RecKey][LocKey] = { }

            ObsData[RecKey][LocKey][self.selev]  = Selev[i]

            ObsData[RecKey][LocKey][self.obst]  = T[i]
            ObsData[RecKey][LocKey][self.obste] = Terr[i]
            ObsData[RecKey][LocKey][self.obstq] = Tqc[i]

            ObsData[RecKey][LocKey][self.geot] = Tvirt[i]
            ObsData[RecKey][LocKey][self.geop] = Pcoord[i]
            ObsData[RecKey][LocKey][self.geop2] = Pcoord2[i]
      
        Fid.close()


        ## Moisture ##
        print("Reading: {0:s}".format(Qfname))
        Fid = Dataset(Qfname, 'r')
        
        # grab variables
        Nobs = Fid.dimensions['nobs'].size

        Sid   = Fid.variables['Station_ID'][:]
        Selev = Fid.variables['Station_Elevation'][:]

        Lon  = Fid.variables['Longitude'][:]
        Lat  = Fid.variables['Latitude'][:]
        P    = Fid.variables['Pressure'][:]
        Time = Fid.variables['Time'][:]

        Q    = Fid.variables['q_Observation'][:]
        Qerr = Fid.variables['Err_Final'][:]
        Qqc  = Fid.variables['Prep_QC_Mark'][:]

        Qgeo   = Fid.variables['specific_humidity'][:].data
        Pcoord = Fid.variables['atmosphere_ln_pressure_coordinate'][:].data
        Pcoord2 = Fid.variables['atmosphere_pressure_coordinate'][:].data * 1000.0 # kPa to Pa

        # Record extra dimension information
        Dim = Fid.dimensions['specific_humidity_arr_dim']
        ExtraDims[self.geoq] = [ Dim.name, Dim.size ]

        # Organize data by record and then by location.
        for i in range(Nobs):
            # Form a "record" key from the station id, and a "location" key from
            # lon,lat,pressure,time.
            RecKey = "{0:s}".format(Sid[i].tostring().decode('ascii').strip())
            LocKey = "{0:f}:{1:f}:{2:f}:{3:f}".format(Lon[i],Lat[i],P[i],Time[i])

            # Observation data
            if (RecKey not in ObsData):
                ObsData[RecKey] = { }

            if (LocKey not in ObsData[RecKey]):
                ObsData[RecKey][LocKey] = { }

            ObsData[RecKey][LocKey][self.selev]  = Selev[i]

            ObsData[RecKey][LocKey][self.obsq]  = Q[i]
            ObsData[RecKey][LocKey][self.obsqe] = Qerr[i]
            ObsData[RecKey][LocKey][self.obsqq] = Qqc[i]

            ObsData[RecKey][LocKey][self.geoq] = Qgeo[i]
            ObsData[RecKey][LocKey][self.geop] = Pcoord[i]
            ObsData[RecKey][LocKey][self.geop2] = Pcoord2[i]
      
        Fid.close()

        ## Winds ##
        print("Reading: {0:s}".format(UVfname))
        Fid = Dataset(UVfname, 'r')
        
        # grab variables
        Nobs = Fid.dimensions['nobs'].size

        Sid   = Fid.variables['Station_ID'][:]
        Selev = Fid.variables['Station_Elevation'][:]

        Lon  = Fid.variables['Longitude'][:]
        Lat  = Fid.variables['Latitude'][:]
        P    = Fid.variables['Pressure'][:]
        Time = Fid.variables['Time'][:]

        U    = Fid.variables['u_Observation'][:]
        Uerr = Fid.variables['Err_Final'][:]
        Uqc  = Fid.variables['Setup_QC_Mark'][:]
        V    = Fid.variables['v_Observation'][:]
        Verr = Fid.variables['Err_Final'][:]
        Vqc  = Fid.variables['Setup_QC_Mark'][:]

        Ugeo   = Fid.variables['eastward_wind'][:].data
        Vgeo   = Fid.variables['northward_wind'][:].data
        Pcoord = Fid.variables['atmosphere_ln_pressure_coordinate'][:].data
        Pcoord2 = Fid.variables['atmosphere_pressure_coordinate'][:].data * 1000.0 # kPa to Pa

        # Record extra dimension information
        Dim = Fid.dimensions['eastward_wind_arr_dim']
        ExtraDims[self.geou] = [ Dim.name, Dim.size ]

        Dim = Fid.dimensions['northward_wind_arr_dim']
        ExtraDims[self.geov] = [ Dim.name, Dim.size ]

        # Organize data by record and then by location.
        for i in range(Nobs):
            # Form a "record" key from the station id, and a "location" key from
            # lon,lat,pressure,time.
            RecKey = "{0:s}".format(Sid[i].tostring().decode('ascii').strip())
            LocKey = "{0:f}:{1:f}:{2:f}:{3:f}".format(Lon[i],Lat[i],P[i],Time[i])

            # Observation data
            if (RecKey not in ObsData):
                ObsData[RecKey] = { }

            if (LocKey not in ObsData[RecKey]):
                ObsData[RecKey][LocKey] = { }

            ObsData[RecKey][LocKey][self.selev]  = Selev[i]

            ObsData[RecKey][LocKey][self.obsu]  = U[i]
            ObsData[RecKey][LocKey][self.obsue] = Uerr[i]
            ObsData[RecKey][LocKey][self.obsuq] = Uqc[i]
            ObsData[RecKey][LocKey][self.obsv]  = V[i]
            ObsData[RecKey][LocKey][self.obsve] = Verr[i]
            ObsData[RecKey][LocKey][self.obsvq] = Vqc[i]

            ObsData[RecKey][LocKey][self.geou] = Ugeo[i]
            ObsData[RecKey][LocKey][self.geov] = Vgeo[i]
            ObsData[RecKey][LocKey][self.geop] = Pcoord[i]
            ObsData[RecKey][LocKey][self.geop2] = Pcoord2[i]
     
        Fid.close()

        print("")

        # Count up the number of records, locations, and observations
        Nrecs = 0
        Nlocs = 0
        for Sid in ObsData:
            Nrecs += 1
            for Loc in ObsData[Sid]:
               Nlocs += 1

        Nvars = 4
        Nobs  = Nlocs * Nvars

        # Create output netcdf files
        Tfid = Dataset(Tfname, 'r')
        Gfid = Dataset(OutGeoFname, 'w', format='NETCDF4')
        Ofid = Dataset(OutObsFname, 'w', format='NETCDF4')

        # Copy over the file attributes. Add new attributes for the sizes
        for Attr in Tfid.ncattrs():
            Gfid.setncattr(Attr, Tfid.getncattr(Attr))
            Ofid.setncattr(Attr, Tfid.getncattr(Attr))

        # Create dimensions.
        Gfid.createDimension('nlocs', Nlocs)
        Gfid.createDimension(ExtraDims[self.sid][0], ExtraDims[self.sid][1])
        Gfid.createDimension(ExtraDims[self.geot][0], ExtraDims[self.geot][1])
        Gfid.createDimension(ExtraDims[self.geoq][0], ExtraDims[self.geoq][1])
        Gfid.createDimension(ExtraDims[self.geou][0], ExtraDims[self.geou][1])
        Gfid.createDimension(ExtraDims[self.geov][0], ExtraDims[self.geov][1])

        Ofid.createDimension('nlocs', Nlocs)
        Ofid.createDimension(ExtraDims[self.sid][0],  ExtraDims[self.sid][1])

        # Create dummy dimensions for nrecs and nvars and nobs.
        Gfid.createDimension('nrecs', Nrecs)
        Gfid.createDimension('nvars', Nvars)
        Gfid.createDimension('nobs',  Nobs)

        Ofid.createDimension('nrecs', Nrecs)
        Ofid.createDimension('nvars', Nvars)
        Ofid.createDimension('nobs',  Nobs)

        # Create variables.
        #    header, location vars
        Gfid.createVariable(self.var_names[self.sid], 'S8', ('nlocs', ExtraDims[self.sid][0]))
        Ofid.createVariable(self.var_names[self.sid], 'S8', ('nlocs', ExtraDims[self.sid][0]))

        Gfid.createVariable(self.var_names[self.selev], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.selev], 'f4', ('nlocs'))

        Gfid.createVariable(self.var_names[self.lon], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.lon], 'f4', ('nlocs'))
       
        Gfid.createVariable(self.var_names[self.lat], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.lat], 'f4', ('nlocs'))
       
        Gfid.createVariable(self.var_names[self.press], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.press], 'f4', ('nlocs'))

        Gfid.createVariable(self.var_names[self.time], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.time], 'f4', ('nlocs'))

        Gfid.createVariable(self.var_names[self.rnum], 'i4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.rnum], 'i4', ('nlocs'))

        #    geo file vars
        Gfid.createVariable(self.var_names[self.geot], 'f4', ('nlocs', ExtraDims[self.geot][0]))
        Gfid.createVariable(self.var_names[self.geoq], 'f4', ('nlocs', ExtraDims[self.geoq][0]))
        Gfid.createVariable(self.var_names[self.geou], 'f4', ('nlocs', ExtraDims[self.geou][0]))
        Gfid.createVariable(self.var_names[self.geov], 'f4', ('nlocs', ExtraDims[self.geov][0]))
        Gfid.createVariable(self.var_names[self.geop], 'f4', ('nlocs', ExtraDims[self.geop][0]))
        Gfid.createVariable(self.var_names[self.geop2], 'f4', ('nlocs', ExtraDims[self.geop2][0]))

        #    obs file vars (all have one dimension: nlocs)
        Ofid.createVariable(self.var_names[self.obst], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.obsq], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.obsu], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.obsv], 'f4', ('nlocs'))

        Ofid.createVariable(self.var_names[self.obste], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.obsqe], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.obsue], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.obsve], 'f4', ('nlocs'))

        Ofid.createVariable(self.var_names[self.obstq], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.obsqq], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.obsuq], 'f4', ('nlocs'))
        Ofid.createVariable(self.var_names[self.obsvq], 'f4', ('nlocs'))

        # Copy data into variables indexed by nlocs
        iloc = 0
        irec = 0
        for RecKey in ObsData:
            # Extract the station id
            Sid = netCDF4.stringtochar(np.array(RecKey, 'S8'))
            irec += 1
            for LocKey in ObsData[RecKey]:
                # Extract the location data
                LocItems = LocKey.split(':')
                Lon   = float(LocItems[0])
                Lat   = float(LocItems[1])
                Press = float(LocItems[2])
                Time  = float(LocItems[3])

                # geovals
                Gfid[self.var_names[self.sid]][iloc,:] = Sid
                Gfid[self.var_names[self.lon]][iloc] = Lon
                Gfid[self.var_names[self.lat]][iloc] = Lat
                Gfid[self.var_names[self.press]][iloc] = Press
                Gfid[self.var_names[self.time]][iloc] = Time
                Gfid[self.var_names[self.rnum]][iloc] = irec

                if (self.selev in ObsData[RecKey][LocKey]):
                    Gfid[self.var_names[self.selev]][iloc] = ObsData[RecKey][LocKey][self.selev]
                if (self.geop in ObsData[RecKey][LocKey]):
                    Gfid[self.var_names[self.geop]][iloc,:] = ObsData[RecKey][LocKey][self.geop]
                if (self.geop2 in ObsData[RecKey][LocKey]):
                    Gfid[self.var_names[self.geop2]][iloc,:] = ObsData[RecKey][LocKey][self.geop2]
                if (self.geot in ObsData[RecKey][LocKey]):
                    Gfid[self.var_names[self.geot]][iloc,:] = ObsData[RecKey][LocKey][self.geot]
                if (self.geoq in ObsData[RecKey][LocKey]):
                    Gfid[self.var_names[self.geoq]][iloc,:] = ObsData[RecKey][LocKey][self.geoq]
                if (self.geou in ObsData[RecKey][LocKey]):
                    Gfid[self.var_names[self.geou]][iloc,:] = ObsData[RecKey][LocKey][self.geou]
                if (self.geov in ObsData[RecKey][LocKey]):
                    Gfid[self.var_names[self.geov]][iloc,:] = ObsData[RecKey][LocKey][self.geov]

                # obs
                Ofid[self.var_names[self.sid]][iloc,:] = Sid
                Ofid[self.var_names[self.lon]][iloc] = Lon
                Ofid[self.var_names[self.lat]][iloc] = Lat
                Ofid[self.var_names[self.press]][iloc] = Press
                Ofid[self.var_names[self.time]][iloc] = Time
                Ofid[self.var_names[self.rnum]][iloc] = irec

                if (self.selev in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.selev]][iloc] = ObsData[RecKey][LocKey][self.selev]
                if (self.obst in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obst]][iloc] = ObsData[RecKey][LocKey][self.obst]
                if (self.obsq in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obsq]][iloc] = ObsData[RecKey][LocKey][self.obsq]
                if (self.obsu in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obsu]][iloc] = ObsData[RecKey][LocKey][self.obsu]
                if (self.obsv in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obsv]][iloc] = ObsData[RecKey][LocKey][self.obsv]

                if (self.obste in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obste]][iloc] = ObsData[RecKey][LocKey][self.obste]
                if (self.obsqe in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obsqe]][iloc] = ObsData[RecKey][LocKey][self.obsqe]
                if (self.obsue in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obsue]][iloc] = ObsData[RecKey][LocKey][self.obsue]
                if (self.obsve in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obsve]][iloc] = ObsData[RecKey][LocKey][self.obsve]

                if (self.obstq in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obstq]][iloc] = ObsData[RecKey][LocKey][self.obstq]
                if (self.obsqq in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obsqq]][iloc] = ObsData[RecKey][LocKey][self.obsqq]
                if (self.obsuq in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obsuq]][iloc] = ObsData[RecKey][LocKey][self.obsuq]
                if (self.obsvq in ObsData[RecKey][LocKey]):
                    Ofid[self.var_names[self.obsvq]][iloc] = ObsData[RecKey][LocKey][self.obsvq]

                iloc += 1

#################################################
############ SONDES OBS TYPE ####################
#################################################
class SondesObsType(ConvObsType):
    ### Constructor ###
    def __init__(self):
        super(SondesObsType, self).__init__()
        self.obs_type = 'Sondes'

###################################################
############ AIRCRAFT OBS TYPE ####################
###################################################
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
        self.copy_vars = {
          #  input var name : [ output destination, output var name, is collapsible ]
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

        # Need a copy of channel numbers for the "expand variables" section below.
        ChanNums = Rfid.variables['chaninfoidx'][:]

        # Copy over the file attributes
        for Attr in Rfid.ncattrs():
            Gfid.setncattr(Attr, Rfid.getncattr(Attr))
            Ofid.setncattr(Attr, Rfid.getncattr(Attr))

        # Copy over the dimensions. Create a new dimension 'nlocs' which will be used
        # for indexing the rows of the tables.
        for Dim in Rfid.dimensions.values():
            Gfid.createDimension(Dim.name, Dim.size)
            Ofid.createDimension(Dim.name, Dim.size)

        Nchans = len(Rfid.dimensions['nchans'])
        Nobs = len(Rfid.dimensions['nobs'])
        Nlocs = Nobs // Nchans
        Nrecs = Nlocs
        Nvars = Nchans # each separate channel will become a separate variable

        Gfid.createDimension('nlocs', Nlocs)
        Ofid.createDimension('nlocs', Nlocs)

        # Create dummy dimensions containing the counts of records and variables.
        # Already copied over the 'nobs' dimension which will turn into a dummy
        # dimensions whose size is the number of observations.
        Gfid.createDimension('nrecs', Nrecs)
        Gfid.createDimension('nvars', Nvars)

        Ofid.createDimension('nrecs', Nrecs)
        Ofid.createDimension('nvars', Nvars)

        # Create the record_num variable. In this case this variable simply contains
        # the numbers 1 .. nrecs since each location is also an individual record.
        Gfid.createVariable('record_number', 'i4', ('nlocs'))
        Ofid.createVariable('record_number', 'i4', ('nlocs'))

        Gfid['record_number'][:] = np.array(np.arange(Nrecs)) + 1
        Ofid['record_number'][:] = np.array(np.arange(Nrecs)) + 1

        # Walk through geo_vars and obs_vars and copy vars to their corresponding
        # output files.
        for InVname in self.copy_vars.keys():
            OutDest       = self.copy_vars[InVname][0]
            OutVname      = self.copy_vars[InVname][1]
            IsCollapsible = self.copy_vars[InVname][2]

            if (InVname in Rfid.variables):
                # variable exists in the input file
                Var = Rfid.variables[InVname]

                # Only consider collapsing if the first dimension is 'nobs'
                if (Var.dimensions[0] == 'nobs'):
                    # Reshape the variable into nlocs X nchans
                    [ VarVals, VarDims ] = self.ReshapeVar(Var, Nlocs, Nchans)

                    # If specified, collapse the variable. Otherwise expand the variable
                    # into a series of variables, one for each channel.
                    if (IsCollapsible):
                        # Collapse the variable and write out.
                        if (Var.ndim == 1):
                            VarVals = VarVals[:,0].squeeze()
                            VarDims = (VarDims[0])
                        else:
                            VarVals = VarVals[:,0,:].squeeze()
                            VarDims = (VarDims[0], VarDims[2])

                        self.WriteNcVar(Gfid, Ofid, OutDest, OutVname, Var.dtype,
                                        VarDims, VarVals)
                    else:
                        # Expand into a series of variables, one for each channel.
                        for ichan in range(Nchans):
                            Vname = "{0:s}_{1:d}_".format(OutVname, ChanNums[ichan])
                            Vvals = VarVals[:,ichan].squeeze()
                            if (len(VarDims) == 2):
                                Vdims = (VarDims[0])
                            else:
                                Vdims = (VarDims[0], VarDims[2])

                            self.WriteNcVar(Gfid, Ofid, OutDest, Vname, Var.dtype, Vdims, Vvals)

                else:
                    # copy variable as is
                    self.WriteNcVar(Gfid, Ofid, OutDest, OutVname, Var.dtype,
                                    Var.dimensions, Var[...])

            else:
                # variable does not exist in the input file
                print("WARNING: Variable '{0:s}' does not exist in the input file".format(InVname))
                print("")

        Rfid.close()
        Gfid.close()
        Ofid.close()

    def ReshapeVar(self, Var, Nlocs, Nchans):
        ###############################################################
        # This method will reshape a variable along the first axis.
        # It is assumed that the input variable, along the first axis,
        # has the data in 'C' order (column-major). Also it is assumed
        # that the variable is either 1D or 2D.
        #
        if (Var.ndim == 1):
            OutVals = Var[:].reshape(Nlocs, Nchans, order='C')
            OutDims = ('nlocs', 'nchans')
        elif (Var.ndim == 2):
            N1 = Var.shape[1]
            OutVals = Var[:].reshape(Nlocs, Nchans, N1, order='C')
            OutDims = ('nlocs', 'nchans', Var.dimensions[1])

        return [ OutVals, OutDims ]

    def WriteNcVar(self, Gfid, Ofid, OutDest, Vname, Vdtype, Vdims, Vvals):
        ###############################################################
        # This method will write out the variable into the appropriate
        # netcdf files.
        #
        if (OutDest == 'G' or OutDest == 'B'):
            # write to geo file
            Ovar = Gfid.createVariable(Vname, Vdtype, Vdims)
            Ovar[...] = Vvals[...]

        if (OutDest == 'O' or OutDest == 'B'):
            # write to obs file
            Ovar = Ofid.createVariable(Vname, Vdtype, Vdims)
            Ovar[...] = Vvals[...]

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
sondes_p.add_argument("-uv", "--uv_file", required=True, help="path to input wind (u and v) file")

# Arguments for Aircraft
aircraft_p = sp.add_parser("Aircraft", help="Merge aircraft files")
aircraft_p.add_argument("-t", "--t_file", required=True, help="path to input temperature file")
aircraft_p.add_argument("-q", "--q_file", required=True, help="path to input moisture file")
aircraft_p.add_argument("-uv", "--uv_file", required=True, help="path to input wind (u and v) file")

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
      'uv_file' : MyArgs.uv_file,
      }

elif (ObsType == "Aircraft"):
    Obs = SondesObsType()
    InFnames = { 
      't_file' : MyArgs.t_file,
      'q_file' : MyArgs.q_file,
      'uv_file' : MyArgs.uv_file,
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


