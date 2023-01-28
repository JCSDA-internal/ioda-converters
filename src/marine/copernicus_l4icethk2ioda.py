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

IODA_CONV_PATH = Path(__file__).parent/"../lib/pyiodaconv"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

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
    'nvars': np.int32(len(obsvars)),
}

DimDict = {
}

VarDims = {
    'sith': ['nlocs'],
}


class copernicus(object):
    def __init__(self, filename):
        # Create a simple data structure for Copernicus L4 sith product obtained from
        # https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-sea-level-global?tab=form
        ncd = nc.Dataset(filename, 'r')
        lons = ncd.variables['lon'][:].ravel()
        lats = ncd.variables['lat'][:].ravel()
        sith = np.squeeze(ncd.variables['sea_ice_thickness'][:]).ravel()
        err = np.squeeze(ncd.variables['uncertainty'][:]).ravel()
        self.date = ncd.getncattr('time_coverage_start')
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
        # Same time stamp for all obs within 1 file
        self.datetime = np.empty_like(self.sith, dtype=object)
        self.datetime[:] = self.date


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
        iodavar = 'sea_ice_thickness'
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.var_mdata[iodavar, iconv.OvalName()]['units'] = 'm'
        self.var_mdata[iodavar, iconv.OerrName()]['units'] = 'm'
        # read input filename
        sith = copernicus(self.filename)
        # put the time at 00 between start and end coverage time
        if self.datetime is not None:
            ymdh = datetime.strptime(self.datetime, "%Y%m%d%H")
            ymdhm = ymdh.strftime("%Y-%m-%dT%H:%M:%SZ")
            sith.datetime[:] = ymdhm
        # map copernicus to ioda data structure
        self.outdata[('dateTime', 'MetaData')] = sith.datetime
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
