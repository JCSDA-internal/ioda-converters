#!/usr/bin/env python
from __future__ import print_function
import argparse
import ioda_conv_ncio as iconv
import scipy.io.netcdf as netcdf
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import dateutil.parser
from datetime import datetime, timedelta
import glob
import h5py
from collections import defaultdict



class Profile(object):
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
        lon      =  np.squeeze(smap['/lon'][:])
        times    =  np.squeeze(smap['/row_time'][:]) #1.03684024e+08
        sss      =  np.squeeze(smap['/smap_sss'][:])
        sss_u    =  np.squeeze(smap['/smap_sss_uncertainty'][:])
        sss_qc   =  np.squeeze(smap['/quality_flag'][:])
        tb_h_aft =  np.squeeze(smap['/tb_h_aft'][:])
        tb_h_fore=  np.squeeze(smap['/tb_h_fore'][:])
        tb_v_aft =  np.squeeze(smap['/tb_v_aft'][:])
        tb_v_fore= np.squeeze(smap['/tb_v_fore'][:])


        # Write in ioda netcdf format
        valKey = vName['SSS'], self.writer.OvalName()
        errKey = vName['SSS'], self.writer.OerrName()
        qcKey = vName['SSS'], self.writer.OqcName()

        count = 0
        for i in range(len(lat)):
            if sss_qc[i] != 0:
                continue

            count += 1
            base_date=datetime(2015, 1, 1, 0, 0) + timedelta(seconds=np.double(times[i]))

            dt = base_date
            locKey = lat[i], lon[i], dt.strftime("%Y-%m-%dT%H:%M:%SZ")
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

    # Read in the profiles
    prof = Profile(args.i, fdate, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    writer.BuildNetcdf(prof.data, AttrData)





