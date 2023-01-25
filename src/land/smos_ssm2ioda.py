#!/usr/bin/env python3
#
# (C) Copyright 2020-2022 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
import os
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta

import lib_python.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from lib_python.orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "string")
]

obsvars = {
    'soil_moisture': 'soilMoistureVolumetric',
}

AttrData = {
}

DimDict = {
}

VarDims = {
    'soilMoistureVolumetric': ['Location'],
}


class SMOS_L2NRT(object):

    def __init__(self, filename, mask):
        self.filename = filename
        self.mask = mask
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    def _read(self):

        # set up variable names for IODA
        for iodavar in ['soilMoistureVolumetric']:
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'm3 m-3'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'm3 m-3'

        # open input file name
        ncd = nc.Dataset(self.filename, 'r')
        # set and get global attributes
        satelliteID = 46
        sensorID = 176
        AttrData["platform"] = np.array([satelliteID], dtype=np.int32)
        AttrData["sensor"] = np.array([sensorID], dtype=np.int32)

        lons = ncd.variables['longitude'][:]
        lats = ncd.variables['latitude'][:]
        vals = ncd.variables['soil_moisture'][:]
        errs = ncd.variables['soil_moisture_uncertainty'][:]
        rfip = ncd.variables['RFI_probability'][:]
        ddys = ncd.variables['days_since_01-01-2000'][:]
        secs = ncd.variables['seconds_since_midnight'][:]
        times = np.empty_like(vals, dtype=object)

        vals = vals.astype('float32')
        lats = lats.astype('float32')
        lons = lons.astype('float32')
        errs = errs.astype('float32')
        qflg = rfip.astype('int32')

        if self.mask == "maskout":
            mask = vals >= 0.0
            vals = vals[mask]
            lons = lons[mask]
            lats = lats[mask]
            errs = errs[mask]
            qflg = qflg[mask]
            ddys = ddys[mask]
            secs = secs[mask]
            times = times[mask]
        ncd.close()

        for i in range(len(lons)):

            # defined QC flag(Kerr et al., 2016)
            if rfip[i] > 20.0:
                qflg[i] = 1
            else:
                qflg[i] = 0

            base_date = datetime(2000, 1, 1) + timedelta(days=int(ddys[i]))
            dt = base_date + timedelta(seconds=int(secs[i]))
            base_datetime = dt.strftime("%Y-%m-%dT%H:%M:%SZ")
            times[i] = base_datetime

        # add metadata variables
        self.outdata[('dateTime', 'MetaData')] = times
        self.outdata[('latitude', 'MetaData')] = lats
        self.outdata[('longitude', 'MetaData')] = lons

        for iodavar in ['soilMoistureVolumetric']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg

        DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])


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

    # Read in the SMOS soil moisture data
    ssm = SMOS_L2NRT(args.input, args.mask)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(ssm.outdata, VarDims, ssm.varAttrs, AttrData)


if __name__ == '__main__':
    main()
