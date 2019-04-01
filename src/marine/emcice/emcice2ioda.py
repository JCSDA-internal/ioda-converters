#!/usr/bin/env python

from __future__ import print_function
import argparse
import ioda_conv_ncio as iconv
import netCDF4 as nc
from datetime import datetime, timedelta
import dateutil.parser
from collections import defaultdict


class Observation(object):

    def __init__(self, filename, date, writer):
        print(date)
        self.filename = filename
        self.date = date
        self.data = defaultdict(lambda: defaultdict(dict))
        self.writer = writer
        self._read()

    def _read(self):
        ncd = nc.MFDataset(self.filename)
        # zeroing out due to bug in original data
        time = 0.0*ncd.variables['date_time_group'][:]
        lons = ncd.variables['longitude'][:]
        lats = ncd.variables['latitude'][:]
        vals = ncd.variables['ice_concentration'][:]
        qc = ncd.variables['quality'][:]
        ncd.close()

        valKey = vName, self.writer.OvalName()
        errKey = vName, self.writer.OerrName()
        qcKey = vName, self.writer.OqcName()

        # reftime =
        count = 0
        for i in range(len(lons)):

            count += 1

            obs_date = self.date

            locKey = lats[i], lons[i], obs_date.strftime("%Y-%m-%dT%H:%M:%SZ")
            self.data[0][locKey][valKey] = vals[i]
            self.data[0][locKey][errKey] = 0.1
            self.data[0][locKey][qcKey] = qc[i]


vName = "sea_ice_area_fraction",

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
        description=('')
    )
    parser.add_argument('-i',
                        help="EMC ice fraction obs input file",
                        type=str, nargs='+', required=True)
    parser.add_argument('-o',
                        help="name of ioda output file",
                        type=str, required=True)
    parser.add_argument('-d', '--date',
                        help="base date", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    writer = iconv.NcWriter(args.o, [], locationKeyList)

    # Read in
    ice = Observation(args.i, fdate, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    writer.BuildNetcdf(ice.data, AttrData)
