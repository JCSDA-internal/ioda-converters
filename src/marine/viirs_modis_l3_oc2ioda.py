#!/usr/bin/env python3

#
# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import argparse
import netCDF4 as nc
from datetime import datetime, timedelta
import os
import numpy as np
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict


vName = {
    'chlor_a': "mass_concentration_of_chlorophyll_in_sea_water",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

AttrData = {}


class OCL3(object):

    def __init__(self, filename, date, thin, writer):
        self.filename = filename
        self.date = date
        self.thin = thin
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.writer = writer
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        lons = ncd.variables['lon'][:].ravel()
        lats = ncd.variables['lat'][:].ravel()
        vals = ncd.variables['chlor_a'][:].ravel()
        mask = vals > 0.0
        vals = vals[mask]
        len_grid = len(lons)*len(lats)
        lons, lats = np.meshgrid(lons, lats, copy=False)
        lons = lons.ravel()[mask]
        lats = lats.ravel()[mask]

        # get global attributes
        for v in ('platform', 'instrument', 'processing_version',
                  'time_coverage_start'):
            AttrData[v] = ncd.getncattr(v)
        ncd.close()

        valKey = vName['chlor_a'], self.writer.OvalName()
        errKey = vName['chlor_a'], self.writer.OerrName()
        qcKey = vName['chlor_a'], self.writer.OqcName()

        # apply thinning mask
        if self.thin > 0.0:
            mask_thin = np.random.uniform(size=len(lons)) > self.thin
            lons = lons[mask_thin]
            lats = lats[mask_thin]
            vals = vals[mask_thin]

        count = 0
        for i in range(len(vals)):
            count += 1
            locKey = lats[i], lons[i], AttrData['time_coverage_start']
            self.data[0][locKey][valKey] = vals[i]
            self.data[0][locKey][errKey] = vals[i] * 0.25
            self.data[0][locKey][qcKey] = 0


def main():

    parser = argparse.ArgumentParser(
        description=('Read VIIRS/MODIS L3 standard mapped chlor_a files'
                     ' and convert to IODA format')
    )
    parser.add_argument('-i', '--input',
                        help="name of L3 standard mapped chlor_a input file",
                        type=str, required=True)
    parser.add_argument('-o', '--output',
                        help="name of ioda output file",
                        type=str, required=True)
    parser.add_argument('-d', '--date', metavar="YYYYMMDDHH",
                        help="base date for the center of the window",
                        type=str, required=True)
    parser.add_argument('-t', '--thin',
                        help="percentage of random thinning, from 0.0 to 1.0. Zero indicates"
                        " no thinning is performed. (default: %(default)s)",
                        type=float, required=False, default=0.0)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')
    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the data
    chl = OCL3(args.input, fdate, args.thin, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")
    AttrData['thinning'] = args.thin
    AttrData['converter'] = os.path.basename(__file__)

    (ObsVars, LocMdata, VarMdata) = writer.ExtractObsData(chl.data)
    writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, AttrData)


if __name__ == '__main__':
    main()
