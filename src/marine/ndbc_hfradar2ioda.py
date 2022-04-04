#!/usr/bin/env python3

# ioda-converter for National Data Buoy Center High Frequency Radar radial velocity
# Script modified by Ling Liu (IMSG@NOAA/NCEP/EMC)

from __future__ import print_function
import numpy as np
import numpy.matlib
import sys
import argparse
import netCDF4 as nc
from datetime import datetime, timedelta
import dateutil.parser
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

vName = ["vComponentOfCurrent",
         "uComponentOfCurrent",
         ]

locKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]

GlobalAttrs = {
}


class Observation(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):

        ncd = nc.MFDataset(self.filename)
        time = ncd.variables['time'][:]
        lons = ncd.variables['lon'][:]
        lats = ncd.variables['lat'][:]
        vals_u = ncd.variables['u'][:]
        vals_v = ncd.variables['v'][:]
        dopx = ncd.variables['dopx'][:]
        dopy = ncd.variables['dopy'][:]
        ncd.close()

        lons, lats = np.meshgrid(lons, lats)
        lons = lons.flatten()
        lats = lats.flatten()
        vals_u = vals_u.flatten()
        vals_v = vals_v.flatten()
        dopx = dopx.flatten()
        dopy = dopy.flatten()
        time = np.matlib.repmat(time, len(lons), 1)
        count = 0
        for i in range(len(lons)):
            for j in [0, 1]:
                valKey = vName[j], iconv.OvalName()
                errKey = vName[j], iconv.OerrName()
                qcKey = vName[j], iconv.OqcName()
                if vals_u[i] != '--':
                    count += 1
                    obs_date = int(time[i])
                    locKey = lats[i], lons[i], obs_date
                    if j == 0:
                        self.data[locKey][valKey] = vals_u[i]
                        self.data[locKey][errKey] = 0.1
                        if dopx[i] <= 0.5:
                            self.data[locKey][qcKey] = 0
                        else:
                            self.data[locKey][qcKey] = 11
                    else:
                        self.data[locKey][valKey] = vals_v[i]
                        self.data[locKey][errKey] = 0.1
                        if dopy[i] <= 0.5:
                            self.data[locKey][qcKey] = 0
                        else:
                            self.data[locKey][qcKey] = 11


def main():

    parser = argparse.ArgumentParser(
        description=(
            'HF radar converter to v2'
        )
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of radar input file",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of ioda v2 output",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')
    VarDims = {
        'vComponentOfCurrent': ['Location'],
        'uComponentOfCurrent': ['Location']}
    radar = Observation(args.input, fdate)

    ObsVars, nlocs = iconv.ExtractObsData(radar.data, locKeyList)
    DimDict = {'Location': nlocs}
    writer = iconv.IodaWriter(args.output, locKeyList, DimDict)
    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('vComponentOfCurrent', 'ObsValue')]['units'] = 'm s-1'
    VarAttrs[('vComponentOfCurrent', 'ObsError')]['units'] = 'm s-1'
    VarAttrs[('uComponentOfCurrent', 'ObsValue')]['units'] = 'm s-1'
    VarAttrs[('uComponentOfCurrent', 'ObsError')]['units'] = 'm s-1'
    VarAttrs[('vComponentOfCurrent', 'ObsValue')]['_FillValue'] = -32767
    VarAttrs[('uComponentOfCurrent', 'ObsValue')]['_FillValue'] = -32767
    VarAttrs[('longitude', 'MetaData')]['units'] = 'degree_east'
    VarAttrs[('latitude', 'MetaData')]['units'] = 'degree_north'
    VarAttrs[('dateTime', 'MetaData')]['units'] = 'seconds since 1970-01-01T00:00:00Z'
    writer.BuildIoda(ObsVars, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
