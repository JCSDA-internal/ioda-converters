#!/usr/bin/env python3
#
# (C) Copyright 2020-2022 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
import netCDF4 as nc
import numpy as np
import re
from datetime import datetime
import os

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)
metaDataName = iconv.MetaDataName()

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("height", "float"),
    ("dateTime", "string")
]

obsvars = {
    'snow_cover_fraction': 'snowCoverFraction',
    'total_snow_depth': 'totalSnowDepth',
}

AttrData = {
}

DimDict = {
}

VarDims = {
    'snowCoverFraction': ['Location'],
    'totalSnowDepth': ['Location'],
}


class imsFV3(object):

    def __init__(self, filename):
        self.filename = filename
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    def _read(self):

        # set up variable names for IODA
        for iodavar in ['snowCoverFraction', 'totalSnowDepth']:
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            if iodavar == 'snowCoverFraction':
                self.varAttrs[iodavar, iconv.OvalName()]['units'] = '1'
                self.varAttrs[iodavar, iconv.OerrName()]['units'] = '1'
                self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = float_missing_value
                self.varAttrs[iodavar, iconv.OerrName()]['_FillValue'] = float_missing_value
                self.varAttrs[iodavar, iconv.OqcName()]['_FillValue'] = int_missing_value

            if iodavar == 'totalSnowDepth':
                self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'mm'
                self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'mm'
                self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = float_missing_value
                self.varAttrs[iodavar, iconv.OerrName()]['_FillValue'] = float_missing_value
                self.varAttrs[iodavar, iconv.OqcName()]['_FillValue'] = int_missing_value

        # read netcdf file
        ncd = nc.Dataset(self.filename)
        lons = ncd.variables['lon'][:].ravel()
        lats = ncd.variables['lat'][:].ravel()
        oros = ncd.variables['oro'][:].ravel()
        sncv = ncd.variables['IMSscf'][:].ravel()
        sncv[sncv == -999.] = float_missing_value
        sndv = ncd.variables['IMSsnd'][:].ravel()
        sndv[sndv == -999.] = float_missing_value

        lons = lons.astype('float32')
        lats = lats.astype('float32')
        oros = oros.astype('float32')
        sncv = sncv.astype('float32')
        sndv = sndv.astype('float32')

        qcflg = 0*sncv.astype('int32')
        qdflg = 0*sndv.astype('int32')
        errsc = 0.0*sncv
        errsd = 0.0*sndv
        errsd[:] = 80.

        times = get_observation_time(self.filename, sncv, ncd)

        ncd.close()

        # add metadata variables
        self.outdata[('dateTime', metaDataName)] = times
        self.varAttrs[('dateTime', metaDataName)]['units'] = iso8601_string
        self.varAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value
        self.outdata[('latitude', metaDataName)] = lats
        self.outdata[('longitude', metaDataName)] = lons
        self.outdata[('stationElevation', metaDataName)] = oros
        self.varAttrs[('stationElevation', metaDataName)]['units'] = 'm'

        # add output variables
        for i in range(len(sncv)):
            for iodavar in ['snowCoverFraction', 'totalSnowDepth']:
                if iodavar == 'snowCoverFraction':
                    self.outdata[self.varDict[iodavar]['valKey']] = sncv
                    self.outdata[self.varDict[iodavar]['errKey']] = errsc
                    self.outdata[self.varDict[iodavar]['qcKey']] = qcflg
                if iodavar == 'totalSnowDepth':
                    self.outdata[self.varDict[iodavar]['valKey']] = sndv
                    self.outdata[self.varDict[iodavar]['errKey']] = errsd
                    self.outdata[self.varDict[iodavar]['qcKey']] = qdflg
        DimDict['Location'] = len(self.outdata[('dateTime', metaDataName)])


def get_observation_time(filename, sncv, ncd):

    # get the observation time as a long integer seconds from 1970-01-01T00:00:00Z
    # inputs:
    #   filename - only passed for fallback time from filename
    #   sncv - passed for length of output array
    #
    # outputs:
    #   times - array of long integer seconds

    # initialize and set to missing
    times = np.empty_like(sncv, dtype='int64')
    times[:] = long_missing_value

    if 'valid_epoch_time' in ncd.ncattrs():
        times[:] = ncd.valid_epoch_time
    elif 'valid_time_str' in ncd.ncattrs():
        my_date = datetime.strptime(ncd.valid_time_str, "%Y%m%d%H")
        times[:] = my_date.timestamp()
    else:
        print(f' ERROR: no time attribute found: {ncd.ncattrs()}')
        # from filename (last match of 8 consecutive digits)
        str_date = re.search(r'(\d{8})(?!.*\d{8})', filename).group()
        print(f'   ... could use last match for 8 consecutive digits: {str_date}')
        import sys
        sys.exit()
        my_date = datetime.strptime(str_date, "%Y%m%d")
        times[:] = my_date.timestamp()

    return times


def main():

    parser = argparse.ArgumentParser(
        description=('Read imsFV3 snow cover and depth file(s) and Converter'
                     ' of native NetCDF format for observations of snow'
                     ' cover and depth from imsFV3 to IODA netCDF format.')
    )
    parser.add_argument('-i', '--input',
                        help="name of imsFV3 snow input file(s)",
                        type=str, required=True)
    parser.add_argument('-o', '--output',
                        help="name of ioda output file",
                        type=str, required=True)

    args = parser.parse_args()

    # Read in the imsFV3 snow data
    ims = imsFV3(args.input)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(ims.outdata, VarDims, ims.varAttrs, AttrData)


if __name__ == '__main__':
    main()
