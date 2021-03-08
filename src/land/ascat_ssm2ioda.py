#!/usr/bin/env python3

#
# (C) Copyright 2021 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
import os
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

obsvars = {
    'soil_moisture': 'soilMoistureNormalized',
}

AttrData = {
    'converter': os.path.basename(__file__),
}


class ascat(object):
    def __init__(self, filename, mask, writer):
        self.filename = filename
        self.mask = mask
        self.writer = writer
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.units = {}
        self._read()

    # Open input file and read relevant info
    def _read(self):
        # set up variable names for IODA
        for iodavar in ['soilMoistureNormalized']:
            self.varDict[iodavar]['valKey'] = iodavar, self.writer.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, self.writer.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, self.writer.OqcName()
            self.units[iodavar] = '%'
        # open input file name
        ncd = nc.Dataset(self.filename, 'r')
        # set and get global attributes
        AttrData["satellite"] = ncd.getncattr('source')
        AttrData['platform'] = ncd.getncattr('platform_long_name')

        lats = ncd.variables['lat'][:].ravel()
        lons = ncd.variables['lon'][:].ravel()
        vals = ncd.variables['soil_moisture'][:].ravel()
        errs = ncd.variables['soil_moisture_error'][:].ravel()
        wflg = ncd.variables['wetland_flag'][:].ravel()
        tflg = ncd.variables['topography_flag'][:].ravel()
        times = np.empty_like(vals, dtype=object)

        num_cells = ncd.dimensions['numCells'].size
        secs = ncd.variables['record_start_time'][:].ravel()
        secs = np.repeat(secs, num_cells)
        qflg = 0*vals.astype('int32')
        wflg = wflg.astype('int32')
        tflg = tflg.astype('int32')

        if self.mask == "maskout":
            mask = np.logical_not(vals.mask)
            vals = vals[mask]
            lons = lons[mask]
            lats = lats[mask]
            errs = errs[mask]
            qflg = qflg[mask]
            wflg = wflg[mask]
            tflg = tflg[mask]
            secs = secs[mask]
            times = times[mask]

        for i in range(len(lons)):
            base_date = datetime(2000, 1, 1) + timedelta(seconds=int(secs[i]))
            base_datetime = base_date.strftime("%Y-%m-%dT%H:%M:%SZ")
            AttrData['date_time_string'] = base_datetime
            times[i] = base_datetime

        self.loc_mdata['datetime'] = self.writer.FillNcVector(times, "datetime")
        self.loc_mdata['latitude'] = lats
        self.loc_mdata['longitude'] = lons
        self.loc_mdata['wetlandFraction'] = wflg
        self.loc_mdata['topographyComplexity'] = tflg
        for iodavar in ['soilMoistureNormalized']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg
        self.writer._nvars = len(obsvars)
        self.writer._nlocs = len(self.loc_mdata['datetime'])


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads ASCAT L2NRT SM netCDF file(s) provided by EUMETSAT'
            ' and converts into IODA formatted output file.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="name of ASCAT L2NRT SM observation netCDF input file(s)",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-m', '--mask',
        help="maskout missing values: maskout/default, default=none",
        type=str, required=True)

    args = parser.parse_args()

    # setup the IODA writer
    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the ssm data
    ssm = ascat(args.input, args.mask, writer)

    # write everything out
    writer.BuildNetcdf(ssm.outdata, ssm.loc_mdata, ssm.var_mdata, AttrData, ssm.units)


if __name__ == '__main__':
    main()
