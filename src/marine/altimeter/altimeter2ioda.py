#!/usr/bin/env python

import scipy.io.netcdf as netcdf
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import dateutil.parser
import datetime
import glob
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter

class preobs:
    def __init__(self, filename, window_length=24.0, midwindow_date='2018041500'):

        # Set DA reftime
        self.yyyy = midwindow_date[0:4]
        self.mm = midwindow_date[4:6]
        self.dd = midwindow_date[6:8]
        self.hh = midwindow_date[8:10]

        self.source = filename
        
        ref_da_time = datetime.datetime(int(self.yyyy),int(self.mm),int(self.dd),int(self.hh))

        self.ref_da_time =  ref_da_time
        
        self.filename=filename
        ncfile = netcdf.netcdf_file(filename, mode='r', mmap=False)
        self.lat      = ncfile.variables['lat'].scale_factor*\
                        np.squeeze(ncfile.variables['lat'][:])
        self.lon      = ncfile.variables['lon'].scale_factor*\
                        np.squeeze(ncfile.variables['lon'][:])
        self.adt     = ncfile.variables['adt_xgm2016'].scale_factor*\
                        np.squeeze(ncfile.variables['adt_xgm2016'][:])
        #self.validobs = np.where( (self.adt<2.0) & (self.adt>-2.0) )

        self.time     = np.squeeze(ncfile.variables['time_mjd'][:])

        # Get ref time for julian date
        units = ncfile.variables['time_mjd'].units[-23:-4]
        reftime = dateutil.parser.parse(units)

        for j in range(len(self.time)):
            if (np.isnan(self.time[j])):
                self.time[j]=99999.9
            else:
                days = int(self.time[j])
                hrsr  = (self.time[j]-days)*24.0
                hrs = int(hrsr)
                sec = int((hrsr - hrs)*3600)

                obs_date = reftime + datetime.timedelta(days=days,hours=hrs,seconds=sec)
                dt = obs_date-self.ref_da_time
                self.time[j]=dt.days*24.0 + dt.seconds/3600.0
            
        self.validobs = np.where( (self.adt<2.0) & (self.adt>-2.0) & (abs(self.time)<=window_length))
        self.num_validobs = np.shape(self.time[self.validobs])[0]

        ncfile.close()

        self.lon = self.lon[self.validobs]
        self.lat = self.lat[self.validobs]
        self.adt = self.adt[self.validobs]
        self.time = self.time[self.validobs]
        
        
    def append(self, other):
        self.lon = np.append(self.lon, other.lon)        
        self.lat = np.append(self.lat, other.lat)        
        self.time = np.append(self.time, other.time)
        self.adt = np.append(self.adt, other.adt)
        
    def plot(self):
        plt.scatter(self.lon[self.validobs],self.lat[self.validobs],c=self.adt[self.validobs],vmin=-1.6,vmax=1.4)

    def toioda(self, fname='test.nc'):

        nvars = 1
        nlocs = np.shape(self.time)[0]
        #nlocs = np.shape(self.time[self.validobs])[0]        
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
        ssh_obs = ncfile.createVariable('obs_absolute_dynamic_topography@ObsValue',np.dtype('float32').char,('nlocs',))
        ssh_obs.units = 'm'
        ssh_obs.description = 'adt = ssha + mdt'        
        ssh_obs_error = ncfile.createVariable('obs_absolute_dynamic_topography@ObsError',np.dtype('float32').char,('nlocs',))
        ssh_obs_error.units = 'm' 
        ssh_obs_error.description = 'Standard deviation of observation error'

        # Dump to file
        lat[:] = self.lat
        lon[:] = self.lon
        time[:] = self.time
        ssh_obs[:] = self.adt
        ssh_obs_error[:] = 0.1*np.ones((nlocs))

        # Global attributes
        ncfile.date_time = int(self.yyyy+self.mm+self.dd+self.hh)
        ncfile.source = self.source
        ncfile.close()

if __name__ == '__main__':

    description='Read NESDIS altimetry files and convert to IODA netCDF format ex: altimeter2ioda.py --path /home/gvernier/Data/altimeters/nesdis/2018/ --date 2018071900'
    parser = ArgumentParser(description=description,formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--path', help='path to altimetry files',type=str, required=True)
    parser.add_argument('--date', help='middle of the window, yyyymmddhh', type=str, required=True)
    parser.add_argument('--ic', help='initial condition, yyyymmddhh', type=str, required=True)    
    parser.add_argument('--window', help='half window length in hours', type=str, required=True)
    args = parser.parse_args()

    midwindow_date = args.date
    ic_date = args.ic

    altimeters=['j3','c2','sa']

    for sat in altimeters:
        flist = glob.glob(args.path+'/*'+sat+'*.nc')
        flist.sort()
        cnt=0
        for filename in flist:
            dofy_file = int(filename[-6:-3])
            dofy_da = datetime.datetime.strptime(args.date[0:8],"%Y%m%d")
            dofy_da = dofy_da.timetuple().tm_yday
            if (abs(1.0*(dofy_file-dofy_da))<=2.0*int(args.window)/24.0):
                print(filename)            
                if (cnt==0):
                    adt = preobs(filename=filename, window_length=int(args.window), midwindow_date=midwindow_date)
                else:
                    tmp_adt = preobs(filename=filename, window_length=int(args.window), midwindow_date=midwindow_date)                
                    adt.append(tmp_adt)
                cnt+=1

        fnameout='adt-ioda-'+sat+'-'+ic_date+'.nc'
        adt.toioda(fname=fnameout)

