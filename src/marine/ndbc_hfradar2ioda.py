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


locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

class Observation(object):

    def __init__(self, filename, date, writer):

        self.filename = filename
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.writer = writer
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
                        self.datalocKey][qcKey] = 0
                    else:
                        self.data[locKey][valKey] = vals_v[i]
                        self.data[locKey][errKey] = 0.1
                        self.data[locKey][qcKey] = 0


def main():
    desc = 'Convert HF radar data to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)

    parser.add_argument(
        '-i', '--input',
        help="Radar observation input file(s)",
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output',
        help="path of ioda output file",
        type=str, required=True)
    parser.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)

    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')
    #writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the altimeter
    altim = Observation(args.input, fdate, writer)

    # write them out
    #AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    ObsVars, nlocs = iconv.ExtractObsData(self.data, self.locKeyList,DimDict)
    #writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, AttrData)
    DimDict = {'nlocs': nlocs}
    self.writer = iconv.IodaWriter(self.filename, self.locKeyList,DimDict)
    self.writer.BuildIoda(ObsVars, varDims, self.varAttrs,self.GlobalAttrs)

if __name__ == '__main__':
    main()
