#!/usr/bin/env python3

#
# (C) Copyright 2019-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import argparse
import netCDF4 as nc
from datetime import datetime
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict


vName = {
    #          var name,            units
    2210: ["waterTemperature", "K"],
    2220: ["salinity", "unitless"]
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("depthBelowWaterSurface", "float"),
    ("dateTime", "long")
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
        
        self.VarAttrs['depthBelowWaterSurface', 'MetaData']['units'] = 'm'
        self.VarAttrs['latitude', 'MetaData']['units'] = 'degrees_north'
        self.VarAttrs['longitude', 'MetaData']['units'] = 'degrees_east'
        self.VarAttrs['dateTime', 'MetaData']['units'] = 'seconds since 1970-01-01T00:00:00Z'
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

        dt = qcs.astype('int64')
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

            valKey = varName, iconv.OvalName()
            errKey = varName, iconv.OerrName()
            qcKey = varName, iconv.OqcName()

            if obid[i] == 2210:
                vals[i] = vals[i] + 273.15

            dt[i] = time[0] + hrs[i]*3600
            locKey = lats[i], lons[i], dpth[i], dt[i]
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
        'watertemperature': ['Location'],
        'salinity': ['Location'],
    }

    # Read in the profiles
    prof = Profile(args.input, fdate)

    # write them out
    ObsVars, nlocs = iconv.ExtractObsData(prof.data, locationKeyList)

    DimDict = {'Location': nlocs}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(ObsVars, VarDims, prof.VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
