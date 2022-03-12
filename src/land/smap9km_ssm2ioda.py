#!/usr/bin/env python3
#
# (C) Copyright 2020-2022 EMC/NCEP/NWS/NOAA
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


class smap(object):
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
        self.satellite = 789
        self.sensor = 432
        AttrData["platform"] = np.array([self.satellite], dtype=np.int32)
        AttrData["sensor"] = np.array([self.sensor], dtype=np.int32)

        data = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'][:]
        vals = data[:].ravel()
        _FillValue = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('_FillValue')
        valid_max = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('valid_max')
        valid_min = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('valid_min')

        lats = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['latitude'][:].ravel()
        lons = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['longitude'][:].ravel()
        errs = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture_error'][:].ravel()
        qflg = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['retrieval_qual_flag'][:].ravel()
        sflg = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['surface_flag'][:].ravel()
        vegop = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['vegetation_opacity'][:].ravel()

        times = np.empty_like(vals, dtype=object)

        if self.mask == "maskout":
            with np.errstate(invalid='ignore'):
                mask = (vals > valid_min) & (vals < valid_max)
            vals = vals[mask]
            lats = lats[mask]
            lons = lons[mask]
            errs = errs[mask]
            qflg = qflg[mask]
            sflg = sflg[mask]
            vegop = vegop[mask]
            times = times[mask]

        # get datetime from filename
        str_split = self.filename.split("_")
        str_datetime = str_split[7]
        my_datetime = datetime.strptime(str_datetime, "%Y%m%dT%H%M%S")
        base_datetime = my_datetime.strftime('%Y-%m-%dT%H:%M:%SZ')
        vals = vals.astype('float32')
        lats = lats.astype('float32')
        lons = lons.astype('float32')
        errs = errs.astype('float32')
        qflg = qflg.astype('int32')
        sflg = sflg.astype('int32')
        vegop = vegop.astype('int32')

        for i in range(len(lons)):
            times[i] = base_datetime
            errs[i] = 0.04
        # add metadata variables
        self.outdata[('dateTime', 'MetaData')] = times
        self.outdata[('latitude', 'MetaData')] = lats
        self.outdata[('longitude', 'MetaData')] = lons
        self.varAttrs[('latitude', 'MetaData')]['units'] = 'degree_north'
        self.varAttrs[('longitude', 'MetaData')]['units'] = 'degree_east'
        self.outdata[('surfaceFlag', 'MetaData')] = sflg
        self.outdata[('vegetationOpacity', 'MetaData')] = vegop

        for iodavar in ['soilMoistureVolumetric']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg

        DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])


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
        '-m', '--mask',
        help="maskout missing values: maskout/default, default=none",
        type=str, required=True)

    args = parser.parse_args()

    # Read in the SMAP volumetric soil moisture data
    ssm = smap(args.input, args.mask)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(ssm.outdata, VarDims, ssm.varAttrs, AttrData)


if __name__ == '__main__':
    main()
