#!/usr/bin/env python3
#
# (C) Copyright 2021 EMC/NCEP/NWS/NOAA
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
import time, os, sys
import argparse
import netCDF4 as nc
import numpy as np
import re
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
    ("depthBelowSoilSurface", "float"),
    ("datetime", "string")
]

obsvars = {
    'soil_moisture': 'soilMoistureVolumetric',
}

AttrData = {
    'converter': os.path.basename(__file__),
}

DimDict = {
}

VarDims = {
    'soilMoistureVolumetric': ['nlocs'],
}


class smap(object):
    def __init__(self, args):
        self.filename = args.input
        self.mask = args.maskMissing
        self.assumedSoilDepth = args.assumedSoilDepth
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    # Open input file and read relevant info
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
            self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'

        # open input file name
        ncd = nc.Dataset(self.filename, 'r')
        # set and get global attributes
        self.satellite = "SMAP"
        self.sensor = "radar and radiometer"
        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor

        data = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'][:]
        vals = data[:].ravel()
        _FillValue = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('_FillValue')
        valid_max = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('valid_max')
        valid_min = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('valid_min')

        lats = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['latitude'][:].ravel()
        lons = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['longitude'][:].ravel()
        errs = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture_error'][:].ravel()
        qflg = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['retrieval_qual_flag'][:].ravel()

        deps = np.full_like(vals, self.assumedSoilDepth)
        times = np.empty_like(vals, dtype=object)

        if self.mask:
            with np.errstate(invalid='ignore'):
                mask = (vals > valid_min) & (vals < valid_max)
            vals = vals[mask]
            lats = lats[mask]
            lons = lons[mask]
            deps = deps[mask]
            errs = errs[mask]
            qflg = qflg[mask]
            times = times[mask]

        # get datetime from filename
        str_split = self.filename.split("_")
        str_datetime = str_split[7]
        my_datetime = datetime.strptime(str_datetime, "%Y%m%dT%H%M%S")
        base_datetime = my_datetime.strftime('%Y-%m-%dT%H:%M:%SZ')
        vals = vals.astype('float32')
        lats = lats.astype('float32')
        lons = lons.astype('float32')
        deps = deps.astype('float32')
        errs = errs.astype('float32')
        qflg = qflg.astype('int32')
        AttrData['date_time_string'] = base_datetime

        for i in range(len(lons)):

            if vals[i] > 0.0:
                # assumed 4% SM rathern than -999.0
                errs[i] = 0.04*vals[i]
                if qflg[i] > 5:
                    qflg[i] = 0
                else:
                    qflg[i] = 1
            else:
                qflg[i] = 1

            times[i] = base_datetime

        # add metadata variables
        self.outdata[('datetime', 'MetaData')] = times
        self.outdata[('latitude', 'MetaData')] = lats
        self.outdata[('longitude', 'MetaData')] = lons
        self.outdata[('depthBelowSoilSurface', 'MetaData')] = deps
        self.varAttrs[('depthBelowSoilSurface', 'MetaData')]['units'] = 'm'

        for iodavar in ['soilMoistureVolumetric']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg

        DimDict['nlocs'] = len(self.outdata[('datetime', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])


def main():

    parser = argparse.ArgumentParser(
        description=('Read SMAP surface soil moisture file(s) and Converter'
                     ' of native h5 format for observations of surface'
                     ' soil moisture to IODA netCDF format.')
    )
    parser.add_argument('-i', '--input',
                        help="name of smap surface soil moisture input file(s)",
                        type=str, required=True)
    parser.add_argument('-o', '--output',
                        help="name of ioda output file",
                        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-m', '--maskMissing',
        help="switch to mask missing values: default=False",
        default=False, action='store_true', required=False)
    optional.add_argument(
        '-d', '--assumedSoilDepth',
        help="default assumed depth of soil moisture in meters",
        type=float, default=0.025, required=False)

    args = parser.parse_args()

    # Read in the SMAP volumetric soil moisture data
    ssm = smap(args)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(ssm.outdata, VarDims, ssm.varAttrs, AttrData)


if __name__ == '__main__':
    main()
