#!/usr/bin/env python3

#
# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
import sys
import os
import argparse
import numpy as np
from datetime import datetime, timedelta
import netCDF4 as nc

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

vName = "seaSurfaceSalinity"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]

GlobalAttrs = {}

iso8601_string = 'seconds since 1970-01-01T00:00:00Z'
epoch = datetime.fromisoformat(iso8601_string[14:-1])


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
            sss_qc = sss_qc.astype(np.int32)

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
                this_dt = datetime.strptime(date1+HH1+MM1+SS1, '%Y%m%d%H%M%S')
                time_offset = round((this_dt - epoch).total_seconds())
                locKey = lat[i], lon[i], time_offset
                self.data[locKey][valKey] = sss[i]
                self.data[locKey][errKey] = sss_err[i]
                self.data[locKey][qcKey] = sss_qc[i]
            ncd.close()


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Read JPL/RSS SMOS seaSurfaceSalinity (SSS) file(s) and convert'
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
        'seaSurfaceSalinity': ['Location'],
    }

    # Read in the seaSurfaceSalinity
    sal = Salinity(args.input, fdate)

    # write them out
    ObsVars, Location = iconv.ExtractObsData(sal.data, locationKeyList)

    DimDict = {'Location': Location}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string
    VarAttrs[('seaSurfaceSalinity', 'ObsValue')]['units'] = 'PSU'
    VarAttrs[('seaSurfaceSalinity', 'ObsError')]['units'] = 'PSU'
    VarAttrs[('seaSurfaceSalinity', 'ObsValue')]['_FillValue'] = 999
    VarAttrs[('seaSurfaceSalinity', 'ObsError')]['_FillValue'] = 999
    VarAttrs[('seaSurfaceSalinity', 'PreQC')]['_FillValue'] = 999
    writer.BuildIoda(ObsVars, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
