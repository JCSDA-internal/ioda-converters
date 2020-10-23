#!/usr/bin/env python
#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict

vName = {
    'A': "smos_soil_moisture",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

AttrData = {}


class SMOS(object):

    def __init__(self, filename, mask, writer):
        self.filename = filename
        self.mask = mask
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.writer = writer
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        lons = ncd.variables['lon'][:].ravel()
        lats = ncd.variables['lat'][:].ravel()
        vals = ncd.variables['Soil_Moisture'][:].ravel()
        errs = ncd.variables['Soil_Moisture_Dqx'][:].ravel()
        qcall = ncd.variables['Science_Flags'][:].ravel()
        # we need to convert 1D lat/lon to 2D lat/lon.
        lons, lats = np.meshgrid(lons, lats, copy=False)
        lons = np.tile(lons.ravel(), 1).ravel()
        lats = np.tile(lats.ravel(), 1).ravel()
        if self.mask == "maskout":
            mask = np.logical_not(vals.mask)
            vals = vals[mask]
            lons = lons[mask]
            lats = lats[mask]
            errs = errs[mask]
            qcall = qcall[mask]
        ncd.close()

        valKey = vName['A'], self.writer.OvalName()
        errKey = vName['A'], self.writer.OerrName()
        qcKey = vName['A'], self.writer.OqcName()

        # get datetime from filename

        str_date = self.filename.split('_')[5]
        my_date = datetime.strptime(str_date, "%Y%m%dT%H%M%S")
        start_datetime = my_date.strftime('%Y-%m-%dT%H:%M:%S')
        base_datetime = start_datetime + 'Z'

        for i in range(len(lons)):
            locKey = lats[i], lons[i], base_datetime
            self.data[0][locKey][valKey] = vals[i]
            self.data[0][locKey][errKey] = errs[i]
            self.data[0][locKey][qcKey] = qcall[i]

            AttrData["observation_type"] = "SSM"
            AttrData["satellite"] = "SMOS"
            AttrData["sensor"] = "MIRAS"
            AttrData['date_time_string'] = base_datetime


def main():

    parser = argparse.ArgumentParser(
        description=('Read SMOS surface soil moisture file(s) and Converter'
                     ' of native NetCDF format for observations of soil'
                     ' moisture from SMOS to IODA netCDF format.')
    )
    parser.add_argument('-i', '--input',
                        help="name of smos soil moisture input file(s)",
                        type=str, required=True)
    parser.add_argument('-o', '--output',
                        help="name of ioda output file",
                        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-m', '--mask',
        help="maskout missing values: maskout/default, default=none",
        type=str, required=True)

    args = parser.parse_args()

    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the profiles
    ssm = SMOS(args.input, args.mask, writer)

    (ObsVars, LocMdata, VarMdata) = writer.ExtractObsData(ssm.data)

    writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, AttrData)


if __name__ == '__main__':
    main()
