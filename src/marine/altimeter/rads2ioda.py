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
        self.filename = filename
        self.date = date
        self.data = defaultdict(lambda: defaultdict(dict))
        self.writer = writer
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        time = ncd.variables['time_mjd'][:]
        lons = ncd.variables['lon'][:]
        lats = ncd.variables['lat'][:]
        vals = ncd.variables['adt_xgm2016'][:]
        units = ncd.variables['time_mjd'].units[-23:-4]
        reftime = dateutil.parser.parse(units)
        
        ncd.close()

        valKey = vName, self.writer.OvalName()
        errKey = vName, self.writer.OerrName()

        
        count = 0
        for i in range(len(lons)):

            count += 1

            days = int(time[i])
            hrsr = (time[i]-days)*24.0
            hrs = int(hrsr)
            sec = int((hrsr - hrs)*3600)
            obs_date = reftime + timedelta(days=days,hours=hrs,seconds=sec)

            locKey = lats[i], lons[i], obs_date.strftime("%Y-%m-%dT%H:%M:%SZ")
            self.data[0][locKey][valKey] = vals[i]
            self.data[0][locKey][errKey] = 0.1



vName = "obs_absolute_dynamic_topography",

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
                        help="name of RADS obs input file",
                        type=str, required=True)
    parser.add_argument('-o',
                        help="name of ioda output file",
                        type=str, required=True)
    parser.add_argument('-d', '--date',
                        help="base date", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H%M')

    writer = iconv.NcWriter(args.o, [], locationKeyList)

    # Read in the altimeter
    altim = Observation(args.i, fdate, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    writer.BuildNetcdf(altim.data, AttrData)
