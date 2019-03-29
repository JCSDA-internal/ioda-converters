#!/usr/bin/env python
from __future__ import print_function
import argparse
import ioda_conv_ncio as iconv
import numpy as np
from datetime import datetime, timedelta
import h5py
from collections import defaultdict


class Smap:
    def __init__(self, fname):
        self.filename = fname
        smap = h5py.File(self.filename, 'r')
        self.lat = smap['/lat'][:]
        self.lon = smap['/lon'][:]
        self.times = smap['/row_time'][:]
        self.sss = smap['/smap_sss'][:]
        self.sss_u = smap['/smap_sss_uncertainty'][:]
        self.sss_qc = smap['/quality_flag'][:]

        nt = self.sss.shape[0]
        ns = self.lat.shape[0]*self.lat.shape[1]
        self.ns = ns

        self.lat.shape = (ns,)
        self.lon.shape = (ns,)
        self.times = np.tile(self.times, (nt, 1))
        self.times.shape = (ns,)
        self.sss.shape = (ns,)
        self.sss_u.shape = (ns,)
        self.sss_qc.shape = (ns,)

    def append(self, other):
        # Append other to self
        self.lon = np.append(self.lon, other.lon)
        self.lat = np.append(self.lat, other.lat)
        self.times = np.append(self.times, other.times)
        self.sss = np.append(self.sss, other.sss)
        self.sss_u = np.append(self.sss_u, other.sss_u)
        self.sss_qc = np.append(self.sss_qc, other.sss_qc)


class Salinity(object):
    def __init__(self, filenames, date, writer):
        self.filenames = filenames
        self.date = date
        self.data = defaultdict(lambda: defaultdict(dict))
        self.writer = writer
        # Save observation sources
        self.source = filenames
        self._read()

        # Open obs file and read/load relevant info
    def _read(self):
        count = 0
        for fname in self.filenames:
            print(fname)
            if (count == 0):
                salt = Smap(fname)
            else:
                tmp_salt = Smap(fname)
                salt.append(tmp_salt)

            count += 1

        # Write in ioda netcdf format
        valKey = vName['SSS'], self.writer.OvalName()
        errKey = vName['SSS'], self.writer.OerrName()
        qcKey = vName['SSS'], self.writer.OqcName()

        for i in range(len(salt.lat)):
            base_date = datetime(2015, 1, 1, 0, 0) + \
                timedelta(seconds=np.double(salt.times[i]))
            dt = base_date
            locKey = salt.lat[i], salt.lon[i], dt.strftime(
                "%Y-%m-%dT%H:%M:%SZ")
            self.data[0][locKey][valKey] = salt.sss[i]
            self.data[0][locKey][errKey] = salt.sss_u[i]
            self.data[0][locKey][qcKey] = salt.sss_qc[i]


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
                        type=str, nargs='+', required=True)
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
