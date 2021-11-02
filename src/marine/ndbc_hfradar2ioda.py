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

vName = ["sea_water_meridional_current",
         "sea_water_zonal_current"]

locKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

GlobalAttrs = {
    'odb_version': 1,
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
        units = '1970-01-01 00:00:00'
        reftime = dateutil.parser.parse(units)
        ncd.close()
        lons, lats = np.meshgrid(lons, lats)
        lons = lons.flatten()
        lats = lats.flatten()
        vals_u = vals_u.flatten()
        vals_v = vals_v.flatten()
        time = np.matlib.repmat(time, len(lons), 1)
        count = 0
        for i in range(len(lons)):
            for j in [0, 1]:
                valKey = vName[j], iconv.OvalName()
                errKey = vName[j], iconv.OerrName()
                qcKey = vName[j], iconv.OqcName()
                if vals_u[i] != '--':
                    count += 1
                    obs_date = reftime + timedelta(seconds=int(time[i]))
                    locKey = lats[i], lons[i], obs_date.strftime("%Y-%m-%dT%H:%M:%SZ")
                    if j == 0:
                        self.data[locKey][valKey] = vals_u[i]
                        self.data[locKey][errKey] = 0.1
                        self.data[locKey][qcKey] = 0
                    else:
                        self.data[locKey][valKey] = vals_v[i]
                        self.data[locKey][errKey] = 0.1
                        self.data[locKey][qcKey] = 0


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
        'sea_water_meridional_current': ['nlocs'],
        'sea_water_zonal_current': ['nlocs']}
    radar = Observation(args.input, fdate)
    GlobalAttrs['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    ObsVars, nlocs = iconv.ExtractObsData(radar.data, locKeyList)
    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(args.output, locKeyList, DimDict)
    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('sea_water_meridional_current', 'ObsValue')]['units'] = 'm/s'
    VarAttrs[('sea_water_zonal_current', 'ObsValue')]['units'] = 'm/s'
    VarAttrs[('longitude', 'MetaData')]['units'] = 'degree'
    VarAttrs[('latitude', 'MetaData')]['units'] = 'degree'
    writer.BuildIoda(ObsVars, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
