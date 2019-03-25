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
import ioda_conv_ncio as iconv
from collections import defaultdict


class preobs:
    def __init__(self, filename, window_length=24.0, midwindow_date='2018041500',writer):

        # Set DA reftime
        self.yyyy = midwindow_date[0:4]
        self.mm = midwindow_date[4:6]
        self.dd = midwindow_date[6:8]
        self.hh = midwindow_date[8:10]

        # Save observation source 
        self.source = filename
        self.writer = writer

        # Set the reference time for the DA window
        ref_da_time = datetime.datetime(int(self.yyyy),int(self.mm),int(self.dd),int(self.hh))
        self.ref_da_time =  ref_da_time

        # Observation file name (redundant? needed?)
        self.filename=filename

        # Open obs file and read/load relevant info
        smap          =  h5py.File(self.filename,'r')
        self.lat      =  np.squeeze(smap['/lat'][:])
        self.lon      =  np.squeeze(smap['/lon'][:])
        self.times    =  np.squeeze(smap['/row_time'][:]) #1.03684024e+08
        self.sss      =  np.squeeze(smap['/smap_sss'][:])
        self.sss_u    =  np.squeeze(smap['/smap_sss_uncertainty'][:])
        self.sss_qc    =  np.squeeze(smap['/quality_flag'][:])
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
            base_date=datetime.datetime(2015, 1, 1, 0, 0) + datetime.timedelta(seconds=np.double(self.times[i]))
            ref_time = datetime.datetime(2018, 4, 15, 0, 0)
            dt=base_date-ref_time
            if (dt.days<0):
                sgn = -1
            else:
                sgn = 1
            self.times[i]=sgn*(np.abs(dt.days*24.0) + dt.seconds/3600.0)
        
        # Trivial QC, might not be needed in the future
        self.validobs = np.where( (self.sss<50) & (self.sss>0))
        self.num_validobs = np.shape(self.sss[self.validobs])[0]

        # Store obs info in memory

        self.lon    = self.lon[self.validobs]
        self.times  = np.tile(self.times,(self.sss_u.shape[0],1))
        self.lat    = self.lat[self.validobs]
        self.sss    = self.sss[self.validobs]
        self.sss_u  = self.sss_u[self.validobs]
        self.sss_qc = self.sss_qc[self.validobs]
        self.times  = self.times[self.validobs]
        self.tb_h_aft  = self.tb_h_aft[self.validobs]
        self.tb_h_fore = self.tb_h_fore[self.validobs]
        self.tb_v_aft  = self.tb_v_aft[self.validobs]
        self.tb_v_fore = self.tb_v_fore[self.validobs]

    def append(self, other):
        # Append other to self
        self.lon       = np.append(self.lon, other.lon)        
        self.lat       = np.append(self.lat, other.lat)        
        self.times     = np.append(self.times, other.times)
        self.sss       = np.append(self.sss, other.sss)
        self.sss_u     = np.append(self.sss_u, other.sss_u)
        self.sss_qc    = np.append(self.sss_qc, other.sss_qc)
        self.tb_h_aft  = np.append(self.tb_h_aft, other.tb_h_aft)
        self.tb_h_fore = np.append(self.tb_h_fore, other.tb_h_fore)
        self.tb_v_aft  = np.append(self.tb_v_aft, other.tb_v_aft)
        self.tb_v_fore = np.append(self.tb_v_fore, other.tb_v_fore)
    
    def plot(self):
        # Simple plot method for sanity checks
        plt.scatter(self.lon[self.validobs],self.lat[self.validobs],c=self.sss[self.validobs],vmin=25,vmax=40)

    def toioda(self, fname='sss_test.nc'):
        # Write in ioda netcdf format
        valKey = vName['SSS'], self.writer.OvalName()
        errKey = vName['SSS'], self.writer.OerrName()
        qcKey = vName['SSS'], self.writer.OqcName()

        count = 0
        for i in range(len(hh)):
            if qcs[i] != 0:
                continue

            count += 1
            dt = base_date + timedelta(hours=float(hh[i]))
            locKey = lats[i], lons[i], dt.strftime("%Y-%m-%dT%H:%M:%SZ")
            self.data[0][locKey][valKey] = sss[i]
            self.data[0][locKey][errKey] = sss_u[i]
            self.data[0][locKey][qcKey] = sss_qc[i]


vName = {
  'SSS': "sea_surface_salinity",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("date_time", "string")
    ]

AttrData = {
  'odb_version': 1,
   }

if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description=('Read JPL SMAP sss files and convert '
                     + 'to IODA format')
    )
    parser.add_argument('-i',
                        help="name of sss input file",
                        type=str, required=True)
    parser.add_argument('-o',
                        help="name of ioda output file",
                        type=str, required=True)
    parser.add_argument('-d', '--date',
                        help="base date", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H%M')

    writer = iconv.NcWriter(args.o, [], locationKeyList)

    # Read in the profiles
    prof = Profile(args.i, fdate, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    writer.BuildNetcdf(prof.data, AttrData)





