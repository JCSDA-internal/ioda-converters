#!/usr/bin/env python3
#
# (C) Copyright 2021-2022 EMC/NCEP/NWS/NOAA
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
import os
import argparse
import netCDF4 as nc
import numpy as np
import re
from datetime import datetime

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict

float_missing_value = iconv.get_default_fill_val(np.float32)

os.environ["TZ"] = "UTC"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("depthBelowSoilSurface", "float"),
    ("dateTime", "long")
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

iso8601_string = 'seconds since 1970-01-01T00:00:00Z'
epoch = datetime.fromisoformat(iso8601_string[14:-1])
# Usual reference time for these data is off j2000 base
# really, I'm seeing j2000 is from noon why 11:58:55.816?
j2000_string = 'second since 2000-01-01T11:58:55Z'
j2000_base_date = datetime(2000, 1, 1, 11, 58, 55, 816)


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
            self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = float_missing_value
            self.varAttrs[iodavar, iconv.OerrName()]['_FillValue'] = float_missing_value
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'm3 m-3'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'm3 m-3'

        # open input file name
        ncd = nc.Dataset(self.filename, 'r')
        # set and get global attributes
        satelliteID = 789
        sensorID = 432
        AttrData["platform"] = np.array([satelliteID], dtype=np.int32)
        AttrData["sensor"] = np.array([sensorID], dtype=np.int32)

        data = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'][:]
        vals = data[:].ravel()
        _FillValue = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('_FillValue')
        valid_max = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('valid_max')
        valid_min = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('valid_min')

        lats = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['latitude'][:].ravel()
        lons = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['longitude'][:].ravel()
        errs = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture_error'][:].ravel()
        qflg = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['retrieval_qual_flag'][:].ravel()

        times = get_observation_time(filename, ncd, vals)
        sflg_present, sflg, vegop, erowi, ecoli = get_ease_surface_param(ncd)
        deps = np.full_like(vals, self.assumedSoilDepth)

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
            refsec = refsec[mask]
            if sflg_present:
                sflg = sflg[mask]
                vegop = vegop[mask]
                erowi = erowi[mask]
                ecoli = ecoli[mask]
        # set fillValue to IODA missing
        vals[vals == _FillValue] = float_missing_value

        vals = vals.astype('float32')
        lats = lats.astype('float32')
        lons = lons.astype('float32')
        deps = deps.astype('float32')
        errs = errs.astype('float32')
        qflg = qflg.astype('int32')

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


        # add metadata variables
        self.outdata[('dateTime', 'MetaData')] = times
        self.varAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string
        self.outdata[('latitude', 'MetaData')] = lats
        self.outdata[('longitude', 'MetaData')] = lons
        self.outdata[('depthBelowSoilSurface', 'MetaData')] = deps
        self.varAttrs[('depthBelowSoilSurface', 'MetaData')]['units'] = 'm'
        if sflg_present:
            self.outdata[('surfaceQualifier', 'MetaData')] = sflg
            self.varAttrs[('surfaceQualifier', 'MetaData')]['units'] = 'unitless'
            self.outdata[('vegetationOpacity', 'MetaData')] = vegop
            self.outdata[('easeRowIndex', 'MetaData')] = erowi
            self.outdata[('easeColumnIndex', 'MetaData')] = ecoli

        for iodavar in ['soilMoistureVolumetric']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg

        DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])


def get_observation_time(filename, ncd, vals):
    # get observation time from file if present fallback to extraction from filename
    times = np.empty_like(vals, dtype=np.int64)
    if 'tb_time_seconds' in ncd.variables.keys():
        refsec = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['tb_time_seconds'][:].ravel()
        for i, isec in enumerate(refsec):
            dt = j2000_base_date + timedelta(seconds=isec)
            times[i] = round((dt - epoch).total_seconds())
    else:
        # get datetime from filename
        file_refTime = re.search(r"\d{8}T\d{6}", filename).group()
        file_refTime = datetime.strptime(file_refTime, "%Y%m%dT%H%M%S")
        times[:] = round((file_refTime - epoch).total_seconds())

    return times


def get_ease_surface_param(ncd):
        sflg_present = False
        # retrieve surface EASE grid parameters if present
        sflg = None
        vegop = None
        erowi = None
        ecoli = None
        if 'surface_flag' in ncd.groups['Soil_Moisture_Retrieval_Data'].variables.keys() and \
           'vegetation_opacity' in ncd.groups['Soil_Moisture_Retrieval_Data'].variables.keys() and \
           'EASE_row_index' in ncd.groups['Soil_Moisture_Retrieval_Data'].variables.keys() and \
           'EASE_column_index' in ncd.groups['Soil_Moisture_Retrieval_Data'].variables.keys():
            sflg_present = True
            sflg = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['surface_flag'][:].ravel()
            vegop = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['vegetation_opacity'][:].ravel()
            erowi = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['EASE_row_index'][:].ravel()
            ecoli = ncd.groups['Soil_Moisture_Retrieval_Data'].varoiables['EASE_column_index'][:].ravel()
        return sflg_present, sflg, vegop, erowi, ecoli


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
        '--maskMissing',
        help="switch to mask missing values: default=False",
        default=False, action='store_true', required=False)
    optional.add_argument(
        '--assumedSoilDepth',
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
