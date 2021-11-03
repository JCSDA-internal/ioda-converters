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
import re
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
    ("height", "float"),
    ("datetime", "string")
]

obsvars = {
    'snow_cover_fraction': 'snowCoverFraction',
    'total_snow_depth': 'totalSnowDepth',
}

AttrData = {
    'converter': os.path.basename(__file__),
}

DimDict = {
}

VarDims = {
    'snowCoverFraction': ['nlocs'],
    'totalSnowDepth': ['nlocs'],
}


class imsFV3(object):

    def __init__(self, filename):
        self.filename = filename
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    def _read(self):

        # set up variable names for IODA
        for iodavar in ['snowCoverFraction', 'totalSnowDepth']:
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            if iodavar == 'snowCoverFraction':
                self.varAttrs[iodavar, iconv.OvalName()]['units'] = '1'
                self.varAttrs[iodavar, iconv.OerrName()]['units'] = '1'
                self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'
                self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = -999.
                self.varAttrs[iodavar, iconv.OerrName()]['_FillValue'] = -999.
                self.varAttrs[iodavar, iconv.OqcName()]['_FillValue'] = -999

            if iodavar == 'totalSnowDepth':
                self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'mm'
                self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'mm'
                self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'
                self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = -999.
                self.varAttrs[iodavar, iconv.OerrName()]['_FillValue'] = -999.
                self.varAttrs[iodavar, iconv.OqcName()]['_FillValue'] = -999

        # read netcdf file
        ncd = nc.Dataset(self.filename)
        AttrData["sensor"] = "IMS Multisensor"
        lons = ncd.variables['lon'][:]
        lats = ncd.variables['lat'][:]
        oros = ncd.variables['oro'][:]
        sncv = ncd.variables['IMSscf'][:]
        sndv = ncd.variables['IMSsnd'][:]

        lons = lons.astype('float32')
        lats = lats.astype('float32')
        oros = oros.astype('float32')
        sncv = sncv.astype('float32')
        sndv = sndv.astype('float32')

        qcflg = 0*sncv.astype('int32')
        qdflg = 0*sndv.astype('int32')
        errsc = 0.0*sncv
        errsd = 0.0*sndv
        errsd[:] = 80.0
        ncd.close()

        times = np.empty_like(sncv, dtype=object)

        # get datetime from filename
        str_date = re.search(r'\d{8}', self.filename).group()
        my_date = datetime.strptime(str_date, "%Y%m%d")
        start_datetime = my_date.strftime('%Y-%m-%d')
        base_datetime = start_datetime + 'T18:00:00Z'
        AttrData['date_time_string'] = base_datetime

        for i in range(len(lats)):
            times[i] = base_datetime

        # add metadata variables
        self.outdata[('datetime', 'MetaData')] = times
        self.outdata[('latitude', 'MetaData')] = lats
        self.outdata[('longitude', 'MetaData')] = lons
        self.outdata[('height', 'MetaData')] = oros
        self.varAttrs[('height', 'MetaData')]['units'] = 'm'

        # add output variables
        for i in range(len(sncv)):
            for iodavar in ['snowCoverFraction', 'totalSnowDepth']:
                if iodavar == 'snowCoverFraction':
                    self.outdata[self.varDict[iodavar]['valKey']] = sncv
                    self.outdata[self.varDict[iodavar]['errKey']] = errsc
                    self.outdata[self.varDict[iodavar]['qcKey']] = qcflg
                if iodavar == 'totalSnowDepth':
                    self.outdata[self.varDict[iodavar]['valKey']] = sndv
                    self.outdata[self.varDict[iodavar]['errKey']] = errsd
                    self.outdata[self.varDict[iodavar]['qcKey']] = qdflg
        DimDict['nlocs'] = len(self.outdata[('datetime', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])


def main():

    parser = argparse.ArgumentParser(
        description=('Read imsFV3 snow cover and depth file(s) and Converter'
                     ' of native NetCDF format for observations of snow'
                     ' cover and depth from imsFV3 to IODA netCDF format.')
    )
    parser.add_argument('-i', '--input',
                        help="name of imsFV3 snow input file(s)",
                        type=str, required=True)
    parser.add_argument('-o', '--output',
                        help="name of ioda output file",
                        type=str, required=True)

    args = parser.parse_args()

    # Read in the imsFV3 snow data
    ims = imsFV3(args.input)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(ims.outdata, VarDims, ims.varAttrs, AttrData)


if __name__ == '__main__':
    main()
