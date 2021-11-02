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
import dateutil.parser
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

vName = "obs_absolute_dynamic_topography"

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

        ncd = nc.MFDataset(self.filename)
        time = ncd.variables['time_mjd'][:]
        lons = ncd.variables['lon'][:]
        lats = ncd.variables['lat'][:]
        vals = ncd.variables['adt_xgm2016'][:]
        val_units = ncd.variables['adt_xgm2016'].units
        Fill_val = ncd.variables['adt_xgm2016']._FillValue
        scale_factor = ncd.variables['adt_xgm2016'].scale_factor
        units = ncd.variables['time_mjd'].units[-23:-4]
        reftime = dateutil.parser.parse(units)
        ncd.close()

        valKey = vName, iconv.OvalName()
        errKey = vName, iconv.OerrName()
        qcKey = vName, iconv.OqcName()
        self.VarAttrs[vName, iconv.OvalName()]['_FillValue'] = Fill_val
        self.VarAttrs[vName, iconv.OerrName()]['_FillValue'] = Fill_val
        self.VarAttrs[vName, iconv.OqcName()]['_FillValue'] = Fill_val
        self.VarAttrs[vName, iconv.OvalName()]['units'] = val_units
        self.VarAttrs[vName, iconv.OerrName()]['units'] = val_units
        self.VarAttrs[vName, iconv.OqcName()]['units'] = 'unitless'

        for i in range(len(lons)):

            obs_date = reftime + timedelta(days=time[i])

            locKey = lats[i], lons[i], obs_date.strftime("%Y-%m-%dT%H:%M:%SZ")
            self.data[locKey][valKey] = vals[i]
            self.data[locKey][errKey] = 0.1
            self.data[locKey][qcKey] = 0


def main():

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads absolute dynamic topography (ADT) observations'
            ' from NESDIS file(s) and converts into IODA formatted'
            ' output files')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="RADS observation input file(s)",
        type=str, nargs='+', required=True)
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

    # Read in the altimeter
    altim = Observation(args.input, fdate)

    # Extract Obsdata
    ObsVars, nlocs = iconv.ExtractObsData(altim.data, locationKeyList)

    # Set attributes
    DimDict['nlocs'] = nlocs

    # Set up the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # Write the obs out
    writer.BuildIoda(ObsVars, VarDims, altim.VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
