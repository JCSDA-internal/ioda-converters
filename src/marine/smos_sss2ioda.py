#!/usr/bin/env python3

#
# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import sys
import argparse
import numpy as np
from datetime import datetime, timedelta
import netCDF4 as nc
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict


vName = "sea_surface_salinity"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

GlobalAttrs = {}


class Salinity(object):
    def __init__(self, filenames, date):
        self.filenames = filenames
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    # Open obs file and read/load relevant info
    def _read(self):
        valKey = vName, iconv.OvalName()
        errKey = vName, iconv.OerrName()
        qcKey = vName, iconv.OqcName()

        for f in self.filenames:
            print(" Reading file: ", f)
            ncd = nc.Dataset(f, 'r')

            source_var_name = {
                'lat': 'Latitude',
                'lon': 'Longitude',
                'sss': 'SSS_corr',
                'sss_err': 'Sigma_SSS_corr',
                'sss_qc': 'Dg_quality_SSS_corr'
            }
            lon = ncd.variables['Longitude'][:]
            lat = ncd.variables['Latitude'][:]
            sss = ncd.variables['SSS_corr'][:]
            sss_err = ncd.variables['Sigma_SSS_corr'][:]
            sss_qc = ncd.variables['Dg_quality_SSS_corr'][:]
            sss_qc = sss_qc.astype(int)

            mask = np.logical_not(sss.mask)
            lon = lon[mask]
            lat = lat[mask]
            sss = sss[mask]
            sss_err = sss_err[mask]
            sss_qc = sss_qc[mask]

            for i in range(len(lon)):
                if sss_qc[i] <= 150:
                    sss_qc[i] = 0
                else:
                    sss_qc[i] = 1
                # get date from filename
                n = f.find("SM_")
                date1 = f[n+19:n+19+8]
                HH1 = f[n+19+9:n+19+11]
                MM1 = f[n+19+11:n+19+13]
                SS1 = f[n+19+13:n+19+15]
                #
                seconds = (datetime.strptime(date1+HH1+MM1+SS1, '%Y%m%d%H%M%S') - datetime.strptime(
                           date1, '%Y%m%d')).total_seconds()
                basetime = datetime.strptime(date1, '%Y%m%d')
                obs_date = basetime + timedelta(seconds=int(seconds))
                locKey = lat[i], lon[i], obs_date.strftime("%Y-%m-%dT%H:%M:%SZ")
                self.data[locKey][valKey] = sss[i]
                self.data[locKey][errKey] = sss_err[i]
                self.data[locKey][qcKey] = sss_qc[i]
            ncd.close()


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Read JPL/RSS SMOS sea surface salinity (SSS) file(s) and convert'
            ' to a concatenated IODA formatted output file.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="name of sss input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="name of ioda output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')
#
    VarDims = {
        'sea_surface_salinity': ['nlocs'],
    }

    # Read in the salinity
    sal = Salinity(args.input, fdate)

    # write them out
    ObsVars, nlocs = iconv.ExtractObsData(sal.data, locationKeyList)

    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('sea_surface_salinity', 'ObsValue')]['units'] = 'PSU'
    VarAttrs[('sea_surface_salinity', 'ObsError')]['units'] = 'PSU'
    VarAttrs[('sea_surface_salinity', 'PreQC')]['units'] = 'unitless'
    VarAttrs[('sea_surface_salinity', 'ObsValue')]['_FillValue'] = 999
    VarAttrs[('sea_surface_salinity', 'ObsError')]['_FillValue'] = 999
    VarAttrs[('sea_surface_salinity', 'PreQC')]['_FillValue'] = 999
    writer.BuildIoda(ObsVars, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
