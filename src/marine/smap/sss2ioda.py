#!/usr/bin/env python
from __future__ import print_function
import argparse
import ioda_conv_ncio as iconv
import scipy.io.netcdf as netcdf
import numpy as np
import dateutil.parser
from datetime import datetime, timedelta
import glob
import h5py
from collections import defaultdict



class Salinity(object):
    def __init__(self, filename, date ,writer):
        self.filename = filename
        self.date     = date
        self.data     = defaultdict(lambda: defaultdict(dict))
        self.writer   = writer
        # Save observation source 
        self.source   = filename
        self._read()

        # Open obs file and read/load relevant info
    def _read(self):
        smap          =  h5py.File(self.filename,'r')
        lat      =  np.squeeze(smap['/lat'][:])
        print(lat.shape)
        print('lat',lat)
        lon      =  np.squeeze(smap['/lon'][:])
        print('lon',lon)
        times    =  np.squeeze(smap['/row_time'][:]) #1.03684024e+08
        print(times.shape)
        times  = np.tile(times,(1,lon.shape[0]))
        print(times.shape)
        sss      =  np.squeeze(smap['/smap_sss'][:])
        print(sss.shape)
        print('sss',sss)
        sss_u    =  np.squeeze(smap['/smap_sss_uncertainty'][:])
        sss_qc   =  np.squeeze(smap['/quality_flag'][:])
        print(sss_qc.shape)
        print('qc',sss_qc)
        tb_h_aft =  np.squeeze(smap['/tb_h_aft'][:])
        tb_h_fore=  np.squeeze(smap['/tb_h_fore'][:])
        tb_v_aft =  np.squeeze(smap['/tb_v_aft'][:])
        tb_v_fore= np.squeeze(smap['/tb_v_fore'][:])
        #lat.shape =(lat.shape[0]*lat.shape[1],1)
        #lon.shape =(lon.shape[0]*lon.shape[1],1)
        #times  = np.tile(times,(sss.shape[0],1))
        #times.shape =(times.shape[0]*times.shape[1],1)
        sss.shape =(sss.shape[0]*sss.shape[1],1)
        print(max(sss))
        print(min(sss))
        print('sss',sss)
        #sss_u.shape =(sss_u.shape[0]*sss_u.shape[1],1)
        #sss_qc.shape =(sss_qc.shape[0]*sss_qc.shape[1],1)

        # Write in ioda netcdf format
        valKey = vName['SSS'], self.writer.OvalName()
        errKey = vName['SSS'], self.writer.OerrName()
        qcKey = vName['SSS'], self.writer.OqcName()

        count = 0
        for i in range(len(lat)):
            #if sss_qc[i] != 0:
            #    continue
            count += 1
            #print(times[0,i].shape)
            #print(times[0,i])
            base_date=datetime(2015, 1, 1, 0, 0) + timedelta(seconds=np.double(times[0,i]))
            dt = base_date
            locKey = lat[i], lon[i], dt.strftime("%Y-%m-%dT%H:%M:%SZ")
            #print(i, lat[i], lon[i], sss[i], valKey,locKey, self.data)
            self.data[0][locKey][valKey] = sss[i]
            self.data[0][locKey][errKey] = sss_u[i]
            self.data[0][locKey][qcKey]  = sss_qc[i]


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
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    writer = iconv.NcWriter(args.o, [], locationKeyList)

    # Read in the salinity
    sal = Salinity(args.i, fdate, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    writer.BuildNetcdf(sal.data, AttrData)





