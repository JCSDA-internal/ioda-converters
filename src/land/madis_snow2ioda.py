#!/usr/bin/env python3
#
# (C) Copyright 2021-2024 EMC/NCEP/NWS/NOAA
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
import os
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("stationElevation", "float"),
    ("dateTime", "long")
]

obsvars = {
    'snow_depth': 'totalSnowDepth',
}

AttrData = {
}

DimDict = {
}

VarDims = {
    'totalSnowDepth': ['Location'],
}

iso8601_string = 'seconds since 1970-01-01T00:00:00Z'
epoch = datetime.fromisoformat(iso8601_string[14:-1])


class madis(object):
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
        for iodavar in ['totalSnowDepth']:
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'mm'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'mm'

        # open input file name
        ncd = nc.Dataset(self.filename, 'r')
        vals = ncd.variables['snowDepth'][:]
        _FillValue = ncd.variables['snowDepth'].getncattr('_FillValue')

        lats = ncd.variables['latitude'][:]
        lons = ncd.variables['longitude'][:]
        elvs = ncd.variables['elevation'][:]
        qflg = ncd.variables['snowDepthQCR'][:]
        obst = ncd.variables['observationTime'][:]
        sids = nc.chartostring(ncd.variables['stationId'][:, :])

        sites = np.empty_like(vals, dtype=object)
        for i in range(len(vals[:])):
            sites[i] = sids[i]
        times = np.empty_like(vals, dtype=np.int64)
        times = obst

        vals = vals.astype('float32')
        lats = lats.astype('float32')
        lons = lons.astype('float32')
        elvs = elvs.astype('float32')
        errs = 0.0*vals
        errs = errs.astype('float32')
        qflg = qflg.astype('int32')

        # use maskout options
        if self.mask == "maskout":

            with np.errstate(invalid='ignore'):
                mask = (vals >= 0.0) & (vals < 2000.0)
            vals = vals[mask]
            errs = errs[mask]
            qflg = qflg[mask]
            lons = lons[mask]
            lats = lats[mask]
            elvs = elvs[mask]
            sites = sites[mask]
            times = times[mask]

        for i in range(len(vals)):
            if vals[i] >= 0.0:
                errs[i] = 40.0
            else:
                errs[i] = _FillValue

        # add metadata variables
        self.outdata[('dateTime', 'MetaData')] = times
        self.outdata[('stationIdentification', 'MetaData')] = sites
        self.outdata[('latitude', 'MetaData')] = lats
        self.outdata[('longitude', 'MetaData')] = lons
        self.outdata[('stationElevation', 'MetaData')] = elvs
        self.varAttrs[('stationElevation', 'MetaData')]['units'] = 'm'

        for iodavar in ['totalSnowDepth']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.varAttrs[self.varDict[iodavar]['valKey']]['_FillValue'] = _FillValue
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.varAttrs[self.varDict[iodavar]['errKey']]['_FillValue'] = _FillValue
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg

        DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])


def main():

    parser = argparse.ArgumentParser(
        description=('Read MADIS snow depth file(s) and Converter'
                     ' of native NetCDF format for observations of total'
                     ' snow depth to IODA netCDF format.')
    )
    parser.add_argument('-i', '--input',
                        help="name of madis total snow depth input file(s)",
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

    # Read in the MADIS total snow depth data
    snod = madis(args.input, args.mask)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    # exit()
    # write everything out
    writer.BuildIoda(snod.outdata, VarDims, snod.varAttrs, AttrData)


if __name__ == '__main__':
    main()
