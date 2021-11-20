#!/usr/bin/env python3

#
# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

from __future__ import print_function
import sys
import argparse
import netCDF4 as nc
from datetime import datetime, timedelta
import numpy as np
import numpy.matlib
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

vName = [
    "sea_water_temperature",
    "sea_water_salinity"]

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("depth", "float"),
    ("datetime", "string")
]

GlobalAttrs = {
    'odb_version': 1,
}


class Profile(object):

    def __init__(self, filename, thin, date):
        self.filename = filename
        self.thin = thin
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        dpth = (ncd.variables['ctd_depth'][:])
        time = ncd.variables['ctd_time'][:]
        lons = np.float32(ncd.variables['longitude'][:])
        lats = np.float32(ncd.variables['latitude'][:])
        with np.errstate(invalid='ignore'):
            temperature = np.float32(ncd.variables['temperature'][:])
        with np.errstate(invalid='ignore'):
            salinity = np.float32(ncd.variables['salinity'][:])
        errs = np.float32(np.matlib.repmat(0.2, len(lons), 1))
        Tqcs = ncd.variables['temperature_qc'][:]-1
        Sqcs = ncd.variables['salinity_qc'][:]-1
        errs = np.squeeze(errs)
        ncd.close()
        base_date = datetime(1970, 1, 1)
        dxy = 1.0
        dz = 1.0
        ii = int((98-8)/dxy)+10
        jj = int((45-2)/dxy)+10
        kk = int((1000)/dz)+10
        box = np.zeros((ii, jj, kk))
        for i in range(len(lons) - 1):
            if lats[i] > 2.0 and lats[i] < 45.0 and lons[i] < -8.0 and lons[i] > -98.0:
                m = int((lons[i] + 98) / dxy)
                n = int((lats[i] - 2) / dxy)
                k = int((dpth[i]) / dz)
                if box[m, n, k] == 0:
                    if self.thin == 'False':
                        box[m, n, k] = 0
                    elif self.thin == 'True':
                        box[m, n, k] = 1
                    for j in [0, 1]:
                        valKey = vName[j], iconv.OvalName()
                        errKey = vName[j], iconv.OerrName()
                        qcKey = vName[j], iconv.OqcName()
                        dt = base_date + timedelta(seconds=int(time[i]))
                        locKey = lats[i], lons[i], dpth[i], dt.strftime("%Y-%m-%dT%H:%M:%SZ")
                        if j == 0:
                            self.data[locKey][valKey] = temperature[i]
                            self.data[locKey][errKey] = errs[i]
                            self.data[locKey][qcKey] = Tqcs[i]
                        else:
                            self.data[locKey][valKey] = salinity[i]
                            self.data[locKey][errKey] = errs[i]
                            self.data[locKey][qcKey] = Sqcs[i]


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Read NOAA AOML Hurricane Glider Temperature and Salinity profile observation file(s) with thinning option'
        )
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="name of Glider observation input file(s)",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of ioda output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)
    required.add_argument(
        '-thin', '--thin',
        help="True to thin; False not to thin",
        type=bool, default='False')
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')
    VarDims = {
        'sea_water_temperature': ['nlocs'],
        'sea_water_salinity': ['nlocs']}
    prof = Profile(args.input, args.thin, fdate)
    GlobalAttrs['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")
    ObsVars, nlocs = iconv.ExtractObsData(prof.data, locationKeyList)
    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('sea_water_temperature', 'ObsValue')]['units'] = 'celsius'
    VarAttrs[('sea_water_salinity', 'ObsValue')]['units'] = 'psu'
    VarAttrs[('sea_water_temperature', 'ObsValue')]['_FillValue'] = -32767
    VarAttrs[('sea_water_salinity', 'ObsValue')]['_FillValue'] = -32767
    VarAttrs[('sea_water_temperature', 'PreQC')]['units'] = ''
    VarAttrs[('sea_water_salinity', 'ObsError')]['units'] = ''
    writer.BuildIoda(ObsVars, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
