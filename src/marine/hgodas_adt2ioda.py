#!/usr/bin/env python3

#
# (C) Copyright 2019 UCAR
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


vName = "absolute_dynamic_topography"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

GlobalAttrs = {
}

VarDims = {
    vName: ['nlocs'],
}

DimDict = {
}


class Observation(object):

    def __init__(self, filename, date):
        self.filename = filename
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        time = ncd.variables['time'][:]
        lons = ncd.variables['lon'][:]
        lats = ncd.variables['lat'][:]
        hrs = ncd.variables['hr'][:]
        vals = ncd.variables['val'][:]
        errs = ncd.variables['err'][:]
        qcs = ncd.variables['qc'][:]
        ncd.close()

        base_date = datetime(1970, 1, 1) + timedelta(seconds=int(time[0]))

        valKey = vName, iconv.OvalName()
        errKey = vName, iconv.OerrName()
        qcKey = vName, iconv.OqcName()
        self.VarAttrs[vName, iconv.OvalName()]['_FillValue'] = -999.
        self.VarAttrs[vName, iconv.OerrName()]['_FillValue'] = -999.
        self.VarAttrs[vName, iconv.OqcName()]['_FillValue'] = -999
        self.VarAttrs[vName, iconv.OvalName()]['units'] = 'm'
        self.VarAttrs[vName, iconv.OerrName()]['units'] = 'm'
        self.VarAttrs[vName, iconv.OqcName()]['units'] = 'unitless'

        for i in range(len(hrs)):
            # there shouldn't be any bad obs, but just in case remove them all
            if qcs[i] != 0:
                continue

            dt = base_date + timedelta(hours=float(hrs[i]))
            locKey = lats[i], lons[i], dt.strftime("%Y-%m-%dT%H:%M:%SZ")
            self.data[locKey][valKey] = vals[i]
            self.data[locKey][errKey] = errs[i]
            self.data[locKey][qcKey] = qcs[i]


def main():
    parser = argparse.ArgumentParser(
        description=(
            'Read absolute dynamic topography (ADT) observations'
            ' file(s) that have already been QCd and thinned for use in'
            ' Hybrid-GODAS system.')
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

    # Read in the adt
    adt = Observation(args.input, fdate)

    # Extract Obsdata
    ObsVars, nlocs = iconv.ExtractObsData(adt.data, locationKeyList)

    # Set attributes
    DimDict['nlocs'] = nlocs

    # Set up the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write them out
    writer.BuildIoda(ObsVars, VarDims, adt.VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
