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
import os

import lib_python.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from lib_python.orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

obsvars = {
    'adt': 'adt',
}

AttrData = {
    'converter': os.path.basename(__file__),
}

DimDict = {
}

VarDims = {
    'adt': ['Location'],
}

iso8601_string = 'seconds since 1970-01-01T00:00:00Z'
epoch = datetime.fromisoformat(iso8601_string[14:-1])


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
        self.err = np.squeeze(ncd.variables['err_sla'][:]).ravel()
        self.time_coverage_start = ncd.getncattr('time_coverage_start')
        ncd.close()

        # Remove observations out of sane bounds
        qci = np.where(np.abs(self.adt) < 3.0)
        self.nlocs = len(qci[0])
        self.lons = self.lons[qci]
        self.lats = self.lats[qci]
        self.adt = self.adt[qci].astype(np.single)
        self.err = self.err[qci].astype(np.single)


class copernicus_l4adt2ioda(object):

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
        iodavar = 'absoluteDynamicTopography'
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.var_mdata[iodavar, iconv.OvalName()]['units'] = 'm'
        self.var_mdata[iodavar, iconv.OerrName()]['units'] = 'm'

        # read input filename
        adt = copernicus(self.filename)
        # put the time at 00 between start and end coverage time
        if adt.time_coverage_start is not None:
            this_datetime = datetime.strptime(adt.time_coverage_start[:-1], "%Y-%m-%dT%H:%M:%S")
            time_offset = round((this_datetime - epoch).total_seconds())
        else:
            try:
                this_datetime = datetime.strptime(adt.datetime, "%Y-%m-%dT%H:%M:%SZ")
                time_offset = round((this_datetime - epoch).total_seconds())
            except Exception:
                print(f"ABORT, failure to find timestamp; check format,"
                      " it should be like 2014-07-29T12:00:00Z whereas"
                      " you entered {self.datetime}")
                sys.exit()

        # Same time stamp for all obs within 1 file
        adt.datetime = np.empty_like(adt.adt, dtype=np.int64)
        adt.datetime[:] = time_offset

        # map copernicus to ioda data structure
        self.outdata[('dateTime', 'MetaData')] = adt.datetime
        self.var_mdata[('dateTime', 'MetaData')]['units'] = iso8601_string
        self.outdata[('latitude', 'MetaData')] = adt.lats
        self.outdata[('longitude', 'MetaData')] = adt.lons
        self.outdata[self.varDict[iodavar]['valKey']] = adt.adt
        self.outdata[self.varDict[iodavar]['errKey']] = adt.err
        self.outdata[self.varDict[iodavar]['qcKey']] = np.zeros(adt.nlocs, dtype=np.int32)

        DimDict['Location'] = adt.nlocs


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
    required.add_argument(
        '-d', '--date',
        help="provide date in the format %Y-%m-%dT%H:%M:%SZ",
        type=str, required=False)

    args = parser.parse_args()
    # Read in the ADT data
    adt = copernicus_l4adt2ioda(args.input, datetime=args.date)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(adt.outdata, VarDims, adt.var_mdata, AttrData)


if __name__ == '__main__':
    main()
