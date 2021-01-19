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

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict

vName = {
    'A': "soilMoistureVolumetric",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

AttrData = {}


class smap(object):

    def __init__(self, filename, mask, writer):
        self.filename = filename
        self.mask = mask
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.units_values = {}
        self.writer = writer
        self._read()

    def _read(self):

        ncd = nc.Dataset(self.filename, 'r')
        data = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'][:]
        vals = data[:].ravel()
        _FillValue = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('_FillValue')
        valid_max = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('valid_max')
        valid_min = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture'].getncattr('valid_min')

        lats = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['latitude'][:].ravel()
        lons = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['longitude'][:].ravel()
        errs = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['soil_moisture_error'][:].ravel()
        qflg = ncd.groups['Soil_Moisture_Retrieval_Data'].variables['retrieval_qual_flag'][:].ravel()

        if self.mask == "maskout":
            with np.errstate(invalid='ignore'):
                mask = (vals > valid_min) & (vals < valid_max)
            vals = vals[mask]
            lats = lats[mask]
            lons = lons[mask]
            errs = errs[mask]
            qflg = qflg[mask]

        # get datetime from filename
        str_split = self.filename.split("_")
        str_datetime = str_split[7]
        my_datetime = datetime.strptime(str_datetime, "%Y%m%dT%H%M%S")
        base_datetime = my_datetime.strftime('%Y-%m-%dT%H:%M:%SZ')

        qflg = qflg.astype(int)
        self.units_values[vName['A']] = 'm3 m-3'
        valKey = vName['A'], self.writer.OvalName()
        errKey = vName['A'], self.writer.OerrName()
        qcKey = vName['A'], self.writer.OqcName()

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

            locKey = lats[i], lons[i], base_datetime
            self.data[0][locKey][valKey] = vals[i]
            self.data[0][locKey][errKey] = errs[i]
            self.data[0][locKey][qcKey] = qflg[i]

        # write global attributes out
        self.satellite = "SMAP"
        self.sensor = "radar and radiometer"

        AttrData["satellite"] = self.satellite
        AttrData["sensor"] = self.sensor
        AttrData['date_time_string'] = base_datetime


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

    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the profiles
    ssm = smap(args.input, args.mask, writer)

    (ObsVars, LocMdata, VarMdata) = writer.ExtractObsData(ssm.data)

    writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, AttrData, ssm.units_values)


if __name__ == '__main__':
    main()
