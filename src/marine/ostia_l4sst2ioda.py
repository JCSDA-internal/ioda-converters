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
    ("datetime", "string"),
]

obsvars = {
    'sst': 'sst',
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
}

DimDict = {
}

VarDims = {
    'sst': ['nlocs'],
}


class ostia(object):
    def __init__(self, filename):
        # Create a simple data structure for OSTIA L4 sst product
        ncd = nc.Dataset(filename, 'r')
        self.lons = ncd.variables['lon'][:].astype(np.single)
        self.lats = ncd.variables['lat'][:].astype(np.single)
        self.lons, self.lats = np.meshgrid(self.lons, self.lats)
        self.lons = self.lons.ravel()
        self.lats = self.lats.ravel()
        self.sst = np.squeeze(ncd.variables['analysed_sst'][:]).ravel()
        self.sst = self.sst-273.15
        self.err = np.squeeze(ncd.variables['analysis_error'][:]).ravel()
        self.time = ncd.variables['time'][:]
        ncd.close()

        # Same time stamp for all obs within 1 file
        self.datetime = np.empty_like(self.sst, dtype=object)
        base_date = datetime(1981, 1, 1)
        dt = base_date + timedelta(days=float(self.time/86400.0))
        self.datetime[:] = dt.strftime("%Y-%m-%dT%H:%M:%SZ")

        # Remove observations out of sane bounds
        qci = np.where(np.abs(self.sst) < 99.0)
        self.nlocs = len(qci[0])
        self.lons = self.lons[qci].astype(np.single)
        self.lats = self.lats[qci].astype(np.single)
        self.sst = self.sst[qci].astype(np.single)
        self.err = self.err[qci].astype(np.single)
        self.datetime = self.datetime[qci]


class ostia_l4sst2ioda(object):
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
        iodavar = 'sea_surface_temperature'
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.var_mdata[iodavar, iconv.OvalName()]['units'] = 'c'
        self.var_mdata[iodavar, iconv.OerrName()]['units'] = 'c'

        # read input filename
        sst = ostia(self.filename)

        # map ostia to ioda data structure
        self.outdata[('datetime', 'MetaData')] = sst.datetime
        self.outdata[('latitude', 'MetaData')] = sst.lats
        self.outdata[('longitude', 'MetaData')] = sst.lons
        self.outdata[self.varDict[iodavar]['valKey']] = sst.sst
        self.outdata[self.varDict[iodavar]['errKey']] = sst.err
        self.outdata[self.varDict[iodavar]['qcKey']] = np.zeros(sst.nlocs, dtype=np.int32)

        DimDict['nlocs'] = sst.nlocs
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads L4 ostia sst'
            'and converts into IODA formatted output files.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of L4 ostia SST observation netCDF input file",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    args = parser.parse_args()

    # Read in the SST data
    sst = ostia_l4sst2ioda(args.input)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(sst.outdata, VarDims, sst.var_mdata, AttrData)


if __name__ == '__main__':
    main()
