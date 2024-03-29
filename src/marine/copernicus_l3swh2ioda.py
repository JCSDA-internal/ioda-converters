#!/usr/bin/env python3

#
# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime
import pytz
import os

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

time_units = 'seconds since 2000-01-01T00:00:00Z'

obsvars = {
    'swh': 'swh',
}

AttrData = {
    'converter': os.path.basename(__file__),
}

DimDict = {
}

VarDims = {
    'swh': ['Location'],
}


class copernicus(object):
    def __init__(self, filename, factor):
        # Create a simple data structure for Copernicus L3 swh near-realtime product obtained from
        # ftp://nrt.cmems-du.eu/Core/WAVE_GLO_WAV_L3_SWH_NRT_OBSERVATIONS_014_001/
        ncd = nc.Dataset(filename, 'r')
        self.lons = ncd.variables['longitude'][:]
        self.lats = ncd.variables['latitude'][:]
        self.time = ncd.variables['time'][:]
        self.swh = ncd.variables['VAVH'][:]
        self.err = factor*self.swh
        self.date = ncd.getncattr('first_meas_time')
        ncd.close()

        # set time stamp for all obs
        self.datetime = np.empty_like(self.swh, dtype=np.int64)
        for t in range(len(self.datetime)):
            self.datetime[t] = self.time[t]

        # Remove observations out of sane bounds
        qci = np.where(self.swh > 0.0)
        self.nlocs = len(qci[0])
        self.lons = self.lons[qci].astype(np.single)
        self.lats = self.lats[qci].astype(np.single)
        self.datetime = self.datetime[qci]
        self.swh = self.swh[qci].astype(np.single)
        self.err = self.err[qci].astype(np.single)


class copernicus_l3swh2ioda(object):
    def __init__(self, filename, factor):
        self.filename = filename
        self.factor = factor
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.metaDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    # Open input file and read relevant info
    def _read(self):
        # set up variable names for IODA
        iodavar = 'waveHeightSignificant'
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.var_mdata[iodavar, iconv.OvalName()]['units'] = 'm'
        self.var_mdata[iodavar, iconv.OerrName()]['units'] = 'm'

        # read input filename
        swh = copernicus(self.filename, self.factor)

        # map copernicus to ioda data structure
        self.outdata[('dateTime', 'MetaData')] = swh.datetime
        self.var_mdata[('dateTime', 'MetaData')]['units'] = time_units
        self.outdata[('latitude', 'MetaData')] = swh.lats
        self.outdata[('longitude', 'MetaData')] = swh.lons
        self.outdata[self.varDict[iodavar]['valKey']] = swh.swh
        self.outdata[self.varDict[iodavar]['errKey']] = swh.err
        self.outdata[self.varDict[iodavar]['qcKey']] = np.zeros(swh.nlocs, dtype=np.int32)

        DimDict['Location'] = swh.nlocs


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads L3 SWH provided by COPERNICUS'
            'and converts into IODA formatted output files.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of L3 SWH observation netCDF input file",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)
    required.add_argument(
        '-f', '--factor',
        help="error factor, 0.1 means 10% of obs value, defalut is 0.1",
        type=float, required=False, default=0.1)

    args = parser.parse_args()

    # Read in the ADT data
    swh = copernicus_l3swh2ioda(args.input, args.factor)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(swh.outdata, VarDims, swh.var_mdata, AttrData)


if __name__ == '__main__':
    main()
