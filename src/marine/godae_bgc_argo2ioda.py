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
from pathlib import Path
import numpy.ma as ma

IODA_CONV_PATH = Path(__file__).parent/"../lib/pyiodaconv"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

vName = {
    'CHL': "mass_concentration_of_chlorophyll_in_sea_water",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("depth", "float"),
    ("datetime", "string")
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
        time = ncd.variables['JULD'][:]
        dpth = ncd.variables['PRES'][:]
        lons = ncd.variables['LONGITUDE'][:]
        lats = ncd.variables['LATITUDE'][:]
        vals = ncd.variables['CHLA_ADJUSTED'][:]
        errs = ncd.variables['CHLA_ADJUSTED_ERROR'][:]
        qcs = ncd.variables['CHLA_ADJUSTED_QC'][:]
        ncd.close()

        base_date = datetime(1950, 1, 1)

        for i in range(len(dpth[1])-1):

            if ma.getmask(vals)[1][i] == 1:
                continue

            valKey = vName['CHL'], iconv.OvalName()
            errKey = vName['CHL'], iconv.OerrName()
            qcKey = vName['CHL'], iconv.OqcName()

            dt = base_date + timedelta(days=float(time[1]))
            locKey = ma.getdata(lats)[1], ma.getdata(lons)[1], \
                ma.getdata(dpth)[1][i], dt.strftime("%Y-%m-%dT%H:%M:%SZ")
            self.data[locKey][valKey] = ma.getdata(vals)[1][i]
            self.data[locKey][errKey] = ma.getdata(errs)[1][i]
            self.data[locKey][qcKey] = 0


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Read BGC-Argo chlorophyll profile from godae'
            '  and convert to IODA v2 format.'
        )
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="name of BGC-Argo observation input file(s)",
        type=str, required=True)
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

    VarDims = {
        'mass_concentration_of_chlorophyll_in_sea_water': ['nlocs'],
    }

    # Read in argo profiles
    prof = Profile(args.input, fdate)

    # write them out
    ObsVars, nlocs = iconv.ExtractObsData(prof.data, locationKeyList)

    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('mass_concentration_of_chlorophyll_in_sea_water', 'ObsValue')]['units'] = 'mg m-3'
    VarAttrs[('mass_concentration_of_chlorophyll_in_sea_water', 'ObsError')]['units'] = 'mg m-3'
    VarAttrs[('mass_concentration_of_chlorophyll_in_sea_water', 'PreQC')]['units'] = 'unitless'
    VarAttrs[('mass_concentration_of_chlorophyll_in_sea_water', 'ObsValue')]['_FillValue'] = 999.
    VarAttrs[('mass_concentration_of_chlorophyll_in_sea_water', 'ObsError')]['_FillValue'] = 999.
    VarAttrs[('mass_concentration_of_chlorophyll_in_sea_water', 'PreQC')]['_FillValue'] = 999
    writer.BuildIoda(ObsVars, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
