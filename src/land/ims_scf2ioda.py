#!/usr/bin/env python3
#
# (C) Copyright 2021 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
import pygrib
import time, os, sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
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
    ("datetime", "string")
]

obsvars = {
    'snow_cover_fraction': 'snowCover',
}

AttrData = {
    'converter': os.path.basename(__file__),
}

DimDict = {
}

VarDims = {
    'snowCover': ['nlocs'],
}


class imsscf(object):

    def __init__(self, filename, mask):
        self.filename = filename
        self.mask = mask
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    def _read(self):

        # set up variable names for IODA
        for iodavar in ['snowCover']:
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'percent'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'percent'
            self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'

        # read self.filename to get data
        data = pygrib.open(self.filename)
        lat, lon = data[1].latlons()
        lons = lon[:].ravel()
        lats = lat[:].ravel()
        vals = data.select(name='Snow cover')[0].values[:].ravel()
        # defined errors and qc
        vals = vals.astype('float32')
        lats = lats.astype('float32')
        lons = lons.astype('float32')
        errs = 0.08*vals
        qflg = 0*vals.astype('int32')
        times = np.empty_like(vals, dtype=object)

        if self.mask == "maskout":
            mask = np.logical_not(vals.mask)
            vals = vals[mask]
            errs = errs[mask]
            qflg = qflg[mask]
            lons = lons[mask]
            lats = lats[mask]
            times = times[mask]
        # get global attributes
        start_datetime = data[1].analDate
        base_datetime = start_datetime.isoformat() + "Z"
        # grbs.close()

        # write global attributes out
        self.satellite = "POES/GOES"
        self.sensor = "IMS.Multisensor"
        AttrData["observation_type"] = "Snow Cover Fraction"
        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor
        AttrData['date_time_string'] = base_datetime

        for i in range(len(lons)):
            times[i] = base_datetime

        # add metadata variables
        self.outdata[('datetime', 'MetaData')] = times
        self.outdata[('latitude', 'MetaData')] = lats
        self.outdata[('longitude', 'MetaData')] = lons

        # add output variables
        for iodavar in ['snowCover']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg
        DimDict['nlocs'] = len(self.outdata[('datetime', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])


def main():

    parser = argparse.ArgumentParser(
        description=('Read IMS snow cover fraction file(s) and Converter'
                     ' of native grib2 format for observations of snow'
                     ' cover fraction to IODA netCDF format.')
    )
    parser.add_argument('-i', '--input',
                        help="name of ims snow cover input file(s)",
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

    # Read in the IMS snow cover data
    scf = imsscf(args.input, args.mask)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write the data out
    writer.BuildIoda(scf.outdata, VarDims, scf.varAttrs, AttrData)


if __name__ == '__main__':
    main()
