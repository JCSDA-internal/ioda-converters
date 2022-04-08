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

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]

obsvars = {
    'soil_moisture': 'soilMoistureNormalized',
}

AttrData = {
    'converter': os.path.basename(__file__),
}

DimDict = {
}

VarDims = {
    'soilMoistureNormalized': ['Location'],
}

iso8601_string = '1970-01-01T00:00:00Z'
epoch = datetime.fromisoformat(iso8601_string[:-1])


class ascat(object):
    def __init__(self, filename, mask):
        self.filename = filename
        self.mask = mask
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    # Open input file and read relevant info
    def _read(self):
        # set up variable names for IODA
        for iodavar in ['soilMoistureNormalized']:
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = '1'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = '1'

        # open input file name
        ncd = nc.Dataset(self.filename, 'r')
        # set and get global attributes
        # A satellite ID is expected for satellite attribute, use sensor instead
        AttrData["sensor"] = ncd.getncattr('source')
        AttrData['platform'] = ncd.getncattr('platform_long_name')
        lats = ncd.variables['lat'][:].ravel()
        lons = ncd.variables['lon'][:].ravel()
        vals = ncd.variables['soil_moisture'][:].ravel()
        errs = ncd.variables['soil_moisture_error'][:].ravel()
        errs = errs*0.01
        wflg = ncd.variables['wetland_flag'][:].ravel()
        tflg = ncd.variables['topography_flag'][:].ravel()
        times = np.empty_like(vals, dtype=object)

        num_cells = ncd.dimensions['numCells'].size
        secs = ncd.variables['record_start_time'][:].ravel()
        secs = np.repeat(secs, num_cells)
        vals = vals.astype('float32')
        lats = lats.astype('float32')
        lons = lons.astype('float32')
        vals = vals*0.01
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

        for i in range(len(secs)):
            base_date = datetime(2000, 1, 1) + timedelta(seconds=int(secs[i]))
            time_offset = np.int64(round((base_date - epoch).total_seconds()))
            times[i] = time_offset

        self.varAttrs['dateTime', 'MetaData']['units'] = 'seconds since ' + iso8601_string

        # add metadata variables
        self.outdata[('dateTime', 'MetaData')] = np.array(times, dtype=np.int64)
        self.outdata[('latitude', 'MetaData')] = np.array(lats, dtype=np.float32)
        self.outdata[('longitude', 'MetaData')] = np.array(lons, dtype=np.float32)
        self.outdata[('wetlandFraction', 'MetaData')] = np.array(wflg, dtype=np.float32)
        self.outdata[('topographyComplexity', 'MetaData')] = np.array(tflg, dtype=np.float32)

        iodavar = 'soilMoistureNormalized'
        self.outdata[self.varDict[iodavar]['valKey']] = np.array(vals, dtype=np.float32)
        self.outdata[self.varDict[iodavar]['errKey']] = np.array(errs, dtype=np.float32)
        self.outdata[self.varDict[iodavar]['qcKey']] = np.array(qflg, dtype=np.int32)

        DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])


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

    # Read in the ssm data
    ssm = ascat(args.input, args.mask)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    ssm.varAttrs[('wetlandFraction', 'MetaData')]['units'] = '1'
    ssm.varAttrs[('topographyComplexity', 'MetaData')]['units'] = '1'

    # write everything out
    writer.BuildIoda(ssm.outdata, VarDims, ssm.varAttrs, AttrData)


if __name__ == '__main__':
    main()
