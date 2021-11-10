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

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict


vName = {
    'chlor_a': "mass_concentration_of_chlorophyll_in_sea_water",
}

VarDims = {
    vName['chlor_a']: ['nlocs']
}

DimDict = {}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

GlobalAttrs = {}


class OCL3(object):

    def __init__(self, filename, date, thin):
        self.filename = filename
        self.date = date
        self.thin = thin
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
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
            GlobalAttrs[v] = ncd.getncattr(v)
        ncd.close()

        valKey = vName['chlor_a'], iconv.OvalName()
        errKey = vName['chlor_a'], iconv.OerrName()
        qcKey = vName['chlor_a'], iconv.OqcName()

        self.VarAttrs[vName['chlor_a'], iconv.OvalName()]['_FillValue'] = -32767.
        self.VarAttrs[vName['chlor_a'], iconv.OerrName()]['_FillValue'] = -32767.
        self.VarAttrs[vName['chlor_a'], iconv.OqcName()]['_FillValue'] = -32767
        self.VarAttrs[vName['chlor_a'], iconv.OvalName()]['units'] = 'mg m^-3'
        self.VarAttrs[vName['chlor_a'], iconv.OerrName()]['units'] = 'mg m^-3'
        self.VarAttrs[vName['chlor_a'], iconv.OqcName()]['units'] = 'unitless'

        # apply thinning mask
        if self.thin > 0.0:
            mask_thin = np.random.uniform(size=len(lons)) > self.thin
            lons = lons[mask_thin]
            lats = lats[mask_thin]
            vals = vals[mask_thin]

        for i in range(len(vals)):
            locKey = lats[i], lons[i], GlobalAttrs['time_coverage_start']
            self.data[locKey][valKey] = vals[i]
            self.data[locKey][errKey] = vals[i] * 0.25
            self.data[locKey][qcKey] = 0


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

    # Read in the data
    chl = OCL3(args.input, fdate, args.thin)

    # Extract the obs data
    ObsVars, nlocs = iconv.ExtractObsData(chl.data, locationKeyList)

    # Set Attributes
    GlobalAttrs['thinning'] = args.thin
    GlobalAttrs['converter'] = os.path.basename(__file__)
    DimDict['nlocs'] = nlocs

    # Set up the writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # Write them out
    writer.BuildIoda(ObsVars, VarDims, chl.VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
