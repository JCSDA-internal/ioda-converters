#!/usr/bin/env python

import scipy.io.netcdf as netcdf
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import dateutil.parser
import datetime
import glob
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import h5py

class preobs:
    def __init__(self, filename, window_length=24.0, midwindow_date='2018041500'):

        # Set DA reftime
        self.yyyy = midwindow_date[0:4]
        self.mm = midwindow_date[4:6]
        self.dd = midwindow_date[6:8]
        self.hh = midwindow_date[8:10]

        # Save observation source 
        self.source = filename

        # Set the reference time for the DA window
        ref_da_time = datetime.datetime(int(self.yyyy),int(self.mm),int(self.dd),int(self.hh))
        self.ref_da_time =  ref_da_time

        # Observation file name (redundant? needed?)
        self.filename=filename

        # Open obs file and read/load relevant info
        smap          = h5py.File(filename,'r')
        self.lat      =  np.squeeze(smap['/lat'][:])
        self.lon      =  np.squeeze(smap['/lon'][:])
        self.times    =  np.squeeze(smap['/row_time'][:]) #1.03684024e+08
        self.sss      =  np.squeeze(smap['/smap_sss'][:])
        self.sss_u    =  np.squeeze(smap['/smap_sss_uncertainty'][:])
        self.tb_h_aft =  np.squeeze(smap['/tb_h_aft'][:])
        self.tb_h_fore=  np.squeeze(smap['/tb_h_fore'][:])
        self.tb_v_aft =  np.squeeze(smap['/tb_v_aft'][:])
        self.tb_v_fore= np.squeeze(smap['/tb_v_fore'][:])

        # Get ref time for julian date
        #units = ncfile.variables['time_mjd'].units[-23:-4]
        #reftime = dateutil.parser.parse(units)

        # Set time in hrs referenced to center DA window
        for i in range(len(self.times)):
            #print(self.times[0])
            aa=datetime.datetime(2015, 1, 1, 0, 0) + datetime.timedelta(seconds=np.double(self.times[i]))
            ref_time = datetime.datetime(2018, 4, 15, 0, 0)
            dt=aa-ref_time
            if (dt.days<0):
                sgn = -1
            else:
                sgn = 1
            self.times[i]=sgn*(np.abs(dt.days*24.0) + dt.seconds/3600.0)
        
        # Trivial QC, might not be needed in the future
        self.validobs = np.where( (self.sss<50) & (self.sss>0))
        self.num_validobs = np.shape(self.sss[self.validobs])[0]

        # Store obs info in memory

        self.lon   = self.lon[self.validobs]
        self.times=np.tile(self.times,(self.sss_u.shape[0],1))
        self.lat   = self.lat[self.validobs]
        self.sss   = self.sss[self.validobs]
        self.sss_u = self.sss_u[self.validobs]
        self.times = self.times[self.validobs]
        self.tb_h_aft  =self.tb_h_aft[self.validobs]
        self.tb_h_fore =self.tb_h_fore[self.validobs]
        self.tb_v_aft  =self.tb_v_aft[self.validobs]
        self.tb_v_fore =self.tb_v_fore[self.validobs]

    def append(self, other):
        # Append other to self
        self.lon   = np.append(self.lon, other.lon)        
        self.lat   = np.append(self.lat, other.lat)        
        self.times = np.append(self.times, other.times)
        self.sss   = np.append(self.sss, other.sss)
        self.sss_u = np.append(self.sss_u, other.sss_u)
        self.tb_h_aft  = np.append(self.tb_h_aft, other.tb_h_aft)
        self.tb_h_fore = np.append(self.tb_h_fore, other.tb_h_fore)
        self.tb_v_aft  = np.append(self.tb_v_aft, other.tb_v_aft)
        self.tb_v_fore = np.append(self.tb_v_fore, other.tb_v_fore)
    
    def plot(self):
        # Simple plot method for sanity checks
        plt.scatter(self.lon[self.validobs],self.lat[self.validobs],c=self.sss[self.validobs],vmin=25,vmax=40)

    def toioda(self, fname='sss_test.nc'):
        # Write in ioda netcdf format
        nvars = 1
        nlocs = np.shape(self.times)[0]
        nrecs = 1
        nobs = nvars*nlocs

        ncfile = netcdf.netcdf_file(fname, mode='w')

        # Dimensions
        ncfile.createDimension('nlocs', None) 
        ncfile.createDimension('nrecs', nrecs)
        ncfile.createDimension('nvars', nvars)
        ncfile.createDimension('nobs', nobs)
        
        # Variables
        lon = ncfile.createVariable('longitude@MetaData', np.dtype('float32').char, ('nlocs',))
        lat = ncfile.createVariable('latitude@MetaData',np.dtype('float32').char,('nlocs',))
        time = ncfile.createVariable('time@MetaData',np.dtype('float32').char,('nlocs',))
        sss_obs = ncfile.createVariable('sss@ObsValue',np.dtype('float32').char,('nlocs',))
        sss_obs.units = 'psu'
        #sss_obs.description = 'adt = ssha + mdt'        
        sss_obs_u = ncfile.createVariable('sss@ObsError',np.dtype('float32').char,('nlocs',))
        sss_obs_u.units = 'psu' 
        sss_obs_u.description = 'sss uncertainty'
        tb_h_aft_obs = ncfile.createVariable('tb_h_aft@ObsValue',np.dtype('float32').char,('nlocs',))
        tb_h_fore_obs = ncfile.createVariable('tb_h_fore@ObsValue',np.dtype('float32').char,('nlocs',))
        tb_v_aft_obs  = ncfile.createVariable('tb_v_aft@ObsValue',np.dtype('float32').char,('nlocs',))
        tb_v_fore_obs = ncfile.createVariable('tb_v_fore@ObsValue',np.dtype('float32').char,('nlocs',))
        # Dump to file
        lat[:] = self.lat
        lon[:] = self.lon
        time[:] = self.times
        sss_obs[:] = self.sss
        sss_obs_u[:] = self.sss_u
        tb_h_aft_obs[:]  = self.tb_h_aft
        tb_h_fore_obs[:] = self.tb_h_fore
        tb_v_aft_obs[:]  = self.tb_v_aft
        tb_v_fore_obs[:] = self.tb_v_fore
        # Global attributes
        ncfile.date_time = int(self.yyyy+self.mm+self.dd+self.hh)
        ncfile.source = self.source
        ncfile.close()

