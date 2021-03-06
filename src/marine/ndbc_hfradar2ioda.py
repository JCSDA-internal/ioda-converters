#!/usr/bin/env python

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

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict

vName = ["sea_water_meridional_current",
         "sea_water_zonal_current"]


locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

AttrData = {
    'odb_version': 1,
}


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
                valKey = vName[j], self.writer.OvalName()
                errKey = vName[j], self.writer.OerrName()
                qcKey = vName[j], self.writer.OqcName()
                if vals_u[i] != '--':
                    count += 1
                    obs_date = reftime + timedelta(seconds=int(time[i]))
                    locKey = lats[i], lons[i], obs_date.strftime("%Y-%m-%dT%H:%M:%SZ")
                    if j == 0:
                        self.data[0][locKey][valKey] = vals_u[i]
                        self.data[0][locKey][errKey] = 0.1
                        self.data[0][locKey][qcKey] = 0
                    else:
                        self.data[0][locKey][valKey] = vals_v[i]
                        self.data[0][locKey][errKey] = 0.1
                        self.data[0][locKey][qcKey] = 0


def main():

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads marine HF radar radial velocity from archiving'
            'and converts into IODA formatted'
            'output files')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="Radar observation input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of ioda output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)

    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')
    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the altimeter
    altim = Observation(args.input, fdate, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    (ObsVars, LocMdata, VarMdata) = writer.ExtractObsData(altim.data)
    writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, AttrData)


if __name__ == '__main__':
    main()
