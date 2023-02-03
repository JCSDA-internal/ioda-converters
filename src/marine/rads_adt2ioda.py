#!/usr/bin/env python3

#
# (C) Copyright 2019-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import os
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
import dateutil.parser
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

varInfo = ['absoluteDynamicTopography', 'm']

locationKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z")
]
meta_keys = [m_item[0] for m_item in locationKeyList]

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
double_missing_value = nc.default_fillvals['f8']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'

missing_vals = {'string': string_missing_value,
                'integer': int_missing_value,
                'long': long_missing_value,
                'float': float_missing_value,
                'double': double_missing_value}
dtypes = {'string': object,
          'integer': np.int32,
          'long': np.int64,
          'float': np.float32,
          'double': np.float64}


class Observation(object):

    def __init__(self, filename, date):
        self.filename = filename
        self.date = date
        self._read()

    def _read(self):

        data = {}
        data['dateTime'] = []
        data['vals'] = []
        data['errs'] = []
        data['qcs'] = []

        ncd = nc.Dataset(self.filename)
        time = ncd.variables['time_mjd'][:]
        data['longitude'] = ncd.variables['lon'][:]
        data['latitude'] = ncd.variables['lat'][:]
        vals = ncd.variables['adt_xgm2016'][:]
        data['val_units'] = ncd.variables['adt_xgm2016'].units
        data['Fill_val'] = ncd.variables['adt_xgm2016']._FillValue
        scale_factor = ncd.variables['adt_xgm2016'].scale_factor
        units = ncd.variables['time_mjd'].units[-23:-4]
        reftime = dateutil.parser.parse(units)
        ncd.close()

        for i in range(len(time)):
            dt = reftime + timedelta(days=time[i])
            time_offset = np.int64(round((dt - epoch).total_seconds()))
            data['dateTime'].append(time_offset)
            data['vals'].append(vals[i])
            data['errs'].append(0.1)
            data['qcs'].append(0)

        self.data = data


class IODA(object):

    def __init__(self, file_input, filename, date, obs):

        self.file_input = file_input
        self.filename = filename
        self.date = date

        self.GlobalAttrs = {
            'converter': os.path.basename(__file__),
            'ioda_version': 2,
            'sourceFiles': self.file_input,
            'datetimeReference': self.date.strftime('%Y-%m-%dT%H:%M:%S%z'),
            'description': "Absolute Dynamic Topography (ADT) observations from NESDIS"
        }

        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        # Set units and FillValue attributes for groups associated with observed variable.
        self.varAttrs[(varInfo[0], obsValName)]['units'] = obs.data['val_units']
        self.varAttrs[(varInfo[0], obsErrName)]['units'] = obs.data['val_units']
        self.varAttrs[(varInfo[0], obsValName)]['_FillValue'] = obs.data['Fill_val']
        self.varAttrs[(varInfo[0], obsErrName)]['_FillValue'] = obs.data['Fill_val']
        self.varAttrs[(varInfo[0], qcName)]['_FillValue'] = int_missing_value

        # Set units of the MetaData variables and all _FillValues.
        for key in meta_keys:
            dtypestr = locationKeyList[meta_keys.index(key)][1]
            if locationKeyList[meta_keys.index(key)][2]:
                self.varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
            self.varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]

        # data is the dictionary containing IODA friendly data structure
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        for key in meta_keys:
            dtypestr = locationKeyList[meta_keys.index(key)][1]
            self.data[(key, metaDataName)] = np.array(obs.data[key], dtype=dtypes[dtypestr])

        # Fill up the final array of observed values, obsErrors, and Qc
        self.data[(varInfo[0], obsValName)] = np.array(obs.data['vals'], dtype=np.float32)
        self.data[(varInfo[0], obsErrName)] = np.array(obs.data['errs'], dtype=np.float32)
        self.data[(varInfo[0], qcName)] = np.array(obs.data['qcs'], dtype=np.int32)

        nlocs = len(obs.data['vals'])
        DimDict = {'Location': nlocs}
        varDims = {varInfo[0]: ['Location']}

        # Initialize the writer, then write the file.
        print(f"Writing the output file: {self.filename} with {nlocs} observations in it.")
        self.writer = iconv.IodaWriter(self.filename, locationKeyList, DimDict)
        self.writer.BuildIoda(self.data, varDims, self.varAttrs, self.GlobalAttrs)

        return


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
        help="name of RADS observation input file(s)",
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

    # Read in the adt (altimeter) data
    adt = Observation(args.input, fdate)

    # Write out the IODA output file.
    IODA(args.input, args.output, fdate, adt)


if __name__ == '__main__':
    main()
