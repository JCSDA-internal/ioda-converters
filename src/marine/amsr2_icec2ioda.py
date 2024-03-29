#!/usr/bin/env python3

#
# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import os, sys
import argparse
import numpy as np
from datetime import datetime, timedelta
import netCDF4 as nc
from pathlib import Path

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

vName = "seaIceFraction"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]

icec_FillValue = None
icec_units = ''

GlobalAttrs = {}

iso8601_string = 'seconds since 1970-01-01T00:00:00Z'
epoch = datetime.fromisoformat(iso8601_string[14:-1])


class iceconc(object):
    def __init__(self, filenames, date):
        self.filenames = filenames
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    # Open obs file and read/load relevant info
    def _read(self):
        valKey = vName, iconv.OvalName()
        errKey = vName, iconv.OerrName()
        qcKey = vName, iconv.OqcName()

        global icec_FillValue
        global icec_units

        for f in self.filenames:
            print(" Reading file: ", f)
            ncd = nc.Dataset(f, 'r')

            source_var_name = {
                'lat': 'Latitude',
                'lon': 'Longitude',
                'icec': 'NASA_Team_2_Ice_Concentration',
                'icec_qc': 'Flags'
            }
            lon = ncd.variables['Longitude'][:]
            lat = ncd.variables['Latitude'][:]
            icec = ncd.variables['NASA_Team_2_Ice_Concentration'][:]
            icec_FillValue = ncd.variables['NASA_Team_2_Ice_Concentration']._FillValue
            icec_units = str(ncd.variables['NASA_Team_2_Ice_Concentration'].units)
            print(f" units: {icec_units}")
            icec_qc = ncd.variables['Flags'][:]
            qc_FillValue = ncd.variables['Flags']._FillValue
            qc_units = ncd.variables['Flags'].units
            icec_qc = icec_qc.astype(int)
            mask = np.logical_not(icec.mask)
            lon = lon[mask]
            lat = lat[mask]
            icec = icec[mask]
            icec_qc = icec_qc[mask].astype(np.int32)

            for i in range(len(lon)):
                # get date from filename
                datestart = ncd.getncattr('time_coverage_start')
                dateend = ncd.getncattr('time_coverage_end')
                date1 = datetime.strptime(datestart, "%Y-%m-%dT%H:%M:%S.%fZ")
                date2 = datetime.strptime(dateend, "%Y-%m-%dT%H:%M:%S.%fZ")
                avg = date1 + (date2 - date1) * 0.5
                time_offset = round((avg - epoch).total_seconds())
                locKey = lat[i], lon[i], time_offset
                self.data[locKey][valKey] = icec[i] * 0.01
                self.data[locKey][errKey] = 0.1
                self.data[locKey][qcKey] = icec_qc[i]
            ncd.close()


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Read AMSR-2 sea ice concentration file(s) and convert'
            ' to a concatenated IODA formatted output file.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="name of icec input file(s)",
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
        vName: ['Location'],
    }

    # Read in the Ice concentration
    icec = iceconc(args.input, fdate)

    # write them out
    ObsVars, nlocs = iconv.ExtractObsData(icec.data, locationKeyList)

    DimDict = {'Location': nlocs}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    icec.VarAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string
    icec.VarAttrs[(vName, 'ObsValue')]['units'] = icec_units
    icec.VarAttrs[(vName, 'ObsValue')]['_FillValue'] = icec_FillValue
    icec.VarAttrs[(vName, 'ObsError')]['units'] = icec_units
    icec.VarAttrs[(vName, 'ObsError')]['_FillValue'] = icec_FillValue
    writer.BuildIoda(ObsVars, VarDims, icec.VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
