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

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string"),
]

obsvars = {
    'adt': 'adt',
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
}

DimDict = {
}

VarDims = {
    'adt': ['nlocs'],
}


class copernicus(object):
    def __init__(self, filename):
        # Create a simple data structure for Copernicus L4 adt product obtained from
        # https://cds.climate.copernicus.eu/cdsapp#!/dataset/satellite-sea-level-global?tab=form
        ncd = nc.Dataset(filename, 'r')
        self.lons = ncd.variables['longitude'][:]
        self.lats = ncd.variables['latitude'][:]
        self.lons, self.lats = np.meshgrid(self.lons, self.lats)
        self.lons = self.lons.ravel()
        self.lats = self.lats.ravel()
        self.adt = np.squeeze(ncd.variables['adt'][:]).ravel()
        self.err = np.squeeze(ncd.variables['err'][:]).ravel()
        self.date = ncd.getncattr('time_coverage_start')
        ncd.close()

        # Remove observations out of sane bounds
        qci = np.where(np.abs(self.adt) < 3.0)
        self.nlocs = len(qci[0])
        self.lons = self.lons[qci]
        self.lats = self.lats[qci]
        self.adt = self.adt[qci].astype(np.single)
        self.err = self.err[qci].astype(np.single)

        # Same time stamp for all obs within 1 file
        self.datetime = np.empty_like(self.adt, dtype=object)
        self.datetime[:] = self.date


class copernicus_l4adt2ioda(object):
    def __init__(self, filename):
        self.filename = filename
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.metaDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    # Open input file and read relevant info
    def _read(self):
        # set up variable names for IODA
        iodavar = 'obs_absolute_dynamic_topography'
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.var_mdata[iodavar, iconv.OvalName()]['units'] = 'm'
        self.var_mdata[iodavar, iconv.OerrName()]['units'] = 'm'

        # read input filename
        adt = copernicus(self.filename)

        # map copernicus to ioda data structure
        self.outdata[('datetime', 'MetaData')] = adt.datetime
        self.outdata[('latitude', 'MetaData')] = adt.lats
        self.outdata[('longitude', 'MetaData')] = adt.lons
        self.outdata[self.varDict[iodavar]['valKey']] = adt.adt
        self.outdata[self.varDict[iodavar]['errKey']] = adt.err
        self.outdata[self.varDict[iodavar]['qcKey']] = np.zeros(adt.nlocs, dtype=np.int32)

        DimDict['nlocs'] = adt.nlocs
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads L4 ADT provided by COPERNICUS'
            'and converts into IODA formatted output files.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of L4 ADT observation netCDF input file",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    args = parser.parse_args()

    # Read in the ADT data
    adt = copernicus_l4adt2ioda(args.input)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(adt.outdata, VarDims, adt.var_mdata, AttrData)


if __name__ == '__main__':
    main()
