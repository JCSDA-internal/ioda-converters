#!/usr/bin/env python3

#
# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import argparse
import netCDF4 as nc
from datetime import datetime, timedelta

import lib_python.ioda_conv_engines as iconv
from lib_python.orddicts import DefaultOrderedDict


vName = {
    'T': "seaSurfaceTemperature",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "string")
]

GlobalAttrs = {}


class Profile(object):

    def __init__(self, filename, date):
        self.filename = filename
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        time = ncd.variables['time'][:]
        lons = ncd.variables['lon'][:]
        lats = ncd.variables['lat'][:]
        hrs = ncd.variables['hr'][:]
        vals = ncd.variables['val'][:]
        errs = ncd.variables['err'][:]
        qcs = ncd.variables['qc'][:]
        ncd.close()

        base_date = datetime(1970, 1, 1) + timedelta(seconds=int(time[0]))

        valKey = vName['T'], iconv.OvalName()
        errKey = vName['T'], iconv.OerrName()
        qcKey = vName['T'], iconv.OqcName()

        count = 0
        for i in range(len(hrs)):
            # there shouldn't be any bad obs, but just in case remove them all
            if qcs[i] != 0:
                continue

            count += 1
            dt = base_date + timedelta(hours=float(hrs[i]))
            locKey = lats[i], lons[i], dt.strftime("%Y-%m-%dT%H:%M:%SZ")
            self.data[locKey][valKey] = vals[i] + 273.15
            self.data[locKey][errKey] = errs[i]
            self.data[locKey][qcKey] = qcs[i]


def main():

    parser = argparse.ArgumentParser(
        description=('Read CPC Hybrid-GODAS sst files and convert'
                     ' to IODA format')
    )
    parser.add_argument('-i', '--input',
                        help="name of HGODAS profile input file",
                        type=str, required=True)
    parser.add_argument('-o', '--output',
                        help="name of ioda output file",
                        type=str, required=True)
    parser.add_argument('-d', '--date',
                        help="base date", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    VarDims = {
        'seaSurfaceTemperature': ['Location'],
    }

    # Read in the profiles
    prof = Profile(args.input, fdate)

    # write them out
    ObsVars, Location = iconv.ExtractObsData(prof.data, locationKeyList)

    DimDict = {'Location': Location}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('seaSurfaceTemperature', 'ObsValue')]['units'] = 'K'
    VarAttrs[('seaSurfaceTemperature', 'ObsError')]['units'] = 'K'
    VarAttrs[('seaSurfaceTemperature', 'ObsValue')]['_FillValue'] = 999
    VarAttrs[('seaSurfaceTemperature', 'ObsError')]['_FillValue'] = 999
    VarAttrs[('seaSurfaceTemperature', 'PreQC')]['_FillValue'] = 999
    writer.BuildIoda(ObsVars, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
