#!/usr/bin/env python3

#
# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import sys
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
import os
from pathlib import Path
from subprocess import call
import pandas as pd

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "string"),
]

obsvars = {
    'sith': 'sith',
}

AttrData = {
    'converter': os.path.basename(__file__),
}

DimDict = {
}

VarDims = {
    'sith': ['Location'],
}

iso8601_string = 'seconds since 1970-01-01T00:00:00Z'
epoch = datetime.fromisoformat(iso8601_string[14:-1])


class copernicus(object):
    def __init__(self, filename):
        # Create a simple data structure for Copernicus L4 sith product obtained from
        # https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-sea-level-global?tab=form
        ncd = nc.Dataset(filename, 'r')
        lons = ncd.variables['lon'][:].ravel()
        lats = ncd.variables['lat'][:].ravel()
        sith = np.squeeze(ncd.variables['sea_ice_thickness'][:]).ravel()
        err = np.squeeze(ncd.variables['uncertainty'][:]).ravel()
        self.time_coverage_start = ncd.getncattr('time_coverage_start')
        ncd.close()
        # masked out the Nan values
        sithdata = {'lon': lons, 'lat': lats, 'sith': sith, 'err': err}
        df = pd.DataFrame(sithdata)
        df2 = df.dropna().reset_index(drop=True)
        self.lons = df2['lon']
        self.lats = df2['lat']
        self.sith = df2['sith']
        self.err = df2['err']
        self.nlocs = len(self.sith)


class copernicus_l4icethk2ioda(object):

    def __init__(self, filename, datetime=None):
        self.filename = filename
        self.datetime = datetime
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.metaDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    # Open input file and read relevant info
    def _read(self):
        # set up variable names for IODA
        iodavar = 'seaIceThickness'
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.var_mdata[iodavar, iconv.OvalName()]['units'] = 'm'
        self.var_mdata[iodavar, iconv.OerrName()]['units'] = 'm'
        # read input filename
        sith = copernicus(self.filename)
        # put the time at 00 between start and end coverage time
        if sith.time_coverage_start is not None:
            this_datetime = datetime.strptime(sith.time_coverage_start[:-1], "%Y-%m-%dT%H:%M:%S")
            time_offset = round((this_datetime - epoch).total_seconds())
        else:
            try:
                this_datetime = datetime.strptime(sith.datetime, "%Y-%m-%dT%H:%M:%SZ")
                time_offset = round((this_datetime - epoch).total_seconds())
            except Exception:
                print(f"ABORT, failure to find timestamp; check format,"
                      " it should be like 2014-07-29T12:00:00Z whereas"
                      " you entered {self.datetime}")
                sys.exit()

        # Same time stamp for all obs within 1 file
        sith.datetime = np.empty_like(sith.sith, dtype=np.int64)
        sith.datetime[:] = time_offset
        # map copernicus to ioda data structure
        self.outdata[('dateTime', 'MetaData')] = sith.datetime
        self.var_mdata[('dateTime', 'MetaData')]['units'] = iso8601_string
        self.outdata[('latitude', 'MetaData')] = sith.lats
        self.outdata[('longitude', 'MetaData')] = sith.lons
        self.outdata[self.varDict[iodavar]['valKey']] = sith.sith
        self.outdata[self.varDict[iodavar]['errKey']] = sith.err
        self.outdata[self.varDict[iodavar]['qcKey']] = np.zeros(sith.nlocs, dtype=np.int32)

        DimDict['Location'] = sith.nlocs
        AttrData['Location'] = np.int32(DimDict['Location'])


def main():
    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads L4 sea-ice thickness provided by COPERNICUS'
            'and converts into IODA formatted output files.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of L4 sea-ice thickness observation netCDF input file",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="provide date in the format %Y-%m-%dT%H:%M:%SZ",
        type=str, required=False)

    args = parser.parse_args()
    # Read in the sea-ice thickness data
    sith = copernicus_l4icethk2ioda(args.input, datetime=args.date)
    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    # write everything out
    writer.BuildIoda(sith.outdata, VarDims, sith.var_mdata, AttrData)


if __name__ == '__main__':
    main()
