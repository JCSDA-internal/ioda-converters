#!/usr/bin/env python3

#
# (C) Copyright 2019-2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import argparse
import netCDF4 as nc
from datetime import datetime, timedelta
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict


vName = {
    #          var name,            units
    2210: ["sea_water_temperature", "C"],
    2220: ["sea_water_salinity", "PSU"]
}

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

    def __init__(self, filename, date):
        self.filename = filename
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.floatDefFillVal = iconv.get_default_fill_val('float32')
        self.intDefFillVal = iconv.get_default_fill_val('int32')
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        time = ncd.variables['time'][:]
        obid = ncd.variables['obid'][:]
        dpth = ncd.variables['depth'][:]
        lons = ncd.variables['lon'][:]
        lats = ncd.variables['lat'][:]
        hrs = ncd.variables['hr'][:]
        vals = ncd.variables['val'][:]
        errs = ncd.variables['err'][:]
        qcs = ncd.variables['qc'][:]
        ncd.close()

        base_date = datetime(1970, 1, 1) + timedelta(seconds=int(time[0]))

        self.VarAttrs['depth', 'MetaData']['units'] = 'm'
        for i in range(len(hrs)):
            # there shouldn't be any bad obs, but just in case remove them all
            if qcs[i] != 0:
                continue

            varName = vName[obid[i]][0]
            varUnits = vName[obid[i]][1]
            self.VarAttrs[varName, iconv.OvalName()]['_FillValue'] = self.floatDefFillVal
            self.VarAttrs[varName, iconv.OerrName()]['_FillValue'] = self.floatDefFillVal
            self.VarAttrs[varName, iconv.OqcName()]['_FillValue'] = self.intDefFillVal
            self.VarAttrs[varName, iconv.OvalName()]['units'] = varUnits
            self.VarAttrs[varName, iconv.OerrName()]['units'] = varUnits
            self.VarAttrs[varName, iconv.OqcName()]['units'] = 'unitless'

            valKey = varName, iconv.OvalName()
            errKey = varName, iconv.OerrName()
            qcKey = varName, iconv.OqcName()

            dt = base_date + timedelta(hours=float(hrs[i]))
            locKey = lats[i], lons[i], dpth[i], dt.strftime(
                "%Y-%m-%dT%H:%M:%SZ")
            self.data[locKey][valKey] = vals[i]
            self.data[locKey][errKey] = errs[i]
            self.data[locKey][qcKey] = qcs[i]


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Read insitu T/S profile observation file(s) that have already'
            ' been QCd and thinned for use in Hybrid-GODAS system.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="name of HGODAS observation input file(s)",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of ioda output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    VarDims = {
        'sea_water_temperature': ['nlocs'],
        'sea_water_salinity': ['nlocs'],
    }

    # Read in the profiles
    prof = Profile(args.input, fdate)

    # write them out
    GlobalAttrs['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")
    ObsVars, nlocs = iconv.ExtractObsData(prof.data, locationKeyList)

    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(ObsVars, VarDims, prof.VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
