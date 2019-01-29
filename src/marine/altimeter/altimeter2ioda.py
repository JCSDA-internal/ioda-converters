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
        self.validobs = np.where( (self.adt<2.0) & (self.adt>-2.0) )

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
            
        self.validobs = np.where( (self.adt<2.0) & (self.adt>-2.0) & (abs(self.time)<=0.5*window_length))
        self.num_validobs = np.shape(self.time[self.validobs])[0]
        
        ncfile.close()
        
    def plot(self):
        plt.scatter(self.lon[self.validobs],self.lat[self.validobs],c=self.adt[self.validobs],vmin=-1.6,vmax=1.4)

    def toioda(self, fname='test.nc'):

        nvars = 1
        nlocs = np.shape(self.time[self.validobs])[0]
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
        lat[:] = self.lat[self.validobs]
        lon[:] = self.lon[self.validobs]
        time[:] = self.time[self.validobs]
        ssh_obs[:] = self.adt[self.validobs]
        ssh_obs_error[:] = 0.1*np.ones((nlocs))

        # Global attributes
        ncfile.date_time = int(self.yyyy+self.mm+self.dd+self.hh)
        ncfile.source = self.source
        ncfile.close()

if __name__ == '__main__':

    description='Read NESDIS altimetry files and convert to IODA netCDF format ex: altimeter2ioda.py --path /home/gvernier/Data/altimeters/nesdis/2018/ --date 2018071900'
    parser = ArgumentParser(description=description,formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('--path', help='path to altimetry files',type=str, required=True)
    parser.add_argument('--date', help='file date, yyyymmddhh', type=str, required=True)
    parser.add_argument('--window', help='window length in hours', type=str, required=True)
    args = parser.parse_args()

    midwindow_date = args.date

    flist = glob.glob(args.path+'/*.nc')
    flist.sort()
    cnt=0
    for filename in flist:
        dofy_file = int(filename[-6:-3])
        dofy_da = datetime.datetime.strptime(args.date[0:8],"%Y%m%d")
        dofy_da = dofy_da.timetuple().tm_yday
        if (abs(1.0*(dofy_file-dofy_da))<=2.0*int(args.window)/24.0):
            adt = preobs(filename=filename, window_length=int(args.window), midwindow_date=midwindow_date)
            if (adt.num_validobs>0):
                fnameout='adt-ioda-'+midwindow_date+'-'+str(cnt).zfill(3)+'.nc'
                print(filename,fnameout, adt.num_validobs)
                adt.toioda(fname=fnameout)
                cnt+=1


#ncrcat -h file_1979 file_1980 file_1981 file_197919801981
#
