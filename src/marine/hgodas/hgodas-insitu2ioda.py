#!/usr/bin/env python

from __future__ import print_function
import argparse
import ioda_conv_ncio as iconv
import netCDF4 as nc
from datetime import datetime, timedelta
from collections import defaultdict


class Profile(object):

    def __init__(self, filename, date, writer):
        self.filename = filename
        self.date = date
        self.data = defaultdict(lambda: defaultdict(dict))
        self.writer = writer
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        time = ncd.variables['time'][:]
        obid = ncd.variables['obid'][:]
        dpth = ncd.variables['depth'][:]
        lons = ncd.variables['lon'][:]
        lats = ncd.variables['lat'][:]
        hrs = ncd.variables['hr'][:]
        vals = ncd.variables['val'][:]
        errs = ncd.variables['err'][:]
        qcs = ncd.variables['qc'][:]
        ncd.close()

        base_date = datetime(1970, 1, 1) + timedelta(seconds=int(time[0]))

        for i in range(len(hrs)):
            # there shouldn't be any bad obs, but just in case remove them all
            if qcs[i] != 0:
                continue

            valKey = vName[obid[i]], self.writer.OvalName()
            errKey = vName[obid[i]], self.writer.OerrName()
            qcKey = vName[obid[i]], self.writer.OqcName()

            dt = base_date + timedelta(hours=float(hrs[i]))
            locKey = lats[i], lons[i], dpth[i], dt.strftime(
                "%Y-%m-%dT%H:%M:%SZ")
            self.data[0][locKey][valKey] = vals[i]
            self.data[0][locKey][errKey] = errs[i]
            self.data[0][locKey][qcKey] = qcs[i]


vName = {
    2210: "sea_water_temperature",
    2220: "sea_water_salinity",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("depth", "float"),
    ("date_time", "string")
]

AttrData = {
    'odb_version': 1,
}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description=('Read CPC Hybrid-GODAS ob files and convert '
                     + 'to IODA format')
    )
    parser.add_argument('-i',
                        help="name of HGODAS obs input file",
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
