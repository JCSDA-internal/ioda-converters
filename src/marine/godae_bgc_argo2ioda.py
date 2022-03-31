#!/usr/bin/env python3

#
# (C) Copyright 2021-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import os
import argparse
import numpy as np
import netCDF4 as nc
from datetime import datetime, timedelta
from pathlib import Path
import numpy.ma as ma

IODA_CONV_PATH = Path(__file__).parent/"../lib/pyiodaconv"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

varInfo = ['chlorophyllMassConcentration', 'mg m-3', 999.]
varDims = {varInfo[0]: ['Location']}

locationKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("depthBelowWaterSurface", "float", "m"),
    ("dateTime", "long", "seconds since 1950-01-01T00:00:00Z")
]
meta_keys = [m_item[0] for m_item in locationKeyList]

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = 999.   #   or  nc.default_fillvals['f4']
int_missing_value = 999      #   or  nc.default_fillvals['i4']
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


class Profile(object):

    def __init__(self, filename, date):
        self.filename = filename
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        ncd = nc.Dataset(self.filename)
        time = ncd.variables['JULD'][:]
        dpth = ncd.variables['PRES'][:]
        lons = ncd.variables['LONGITUDE'][:]
        lats = ncd.variables['LATITUDE'][:]
        vals = ncd.variables['CHLA_ADJUSTED'][:]
        errs = ncd.variables['CHLA_ADJUSTED_ERROR'][:]
        qcs = ncd.variables['CHLA_ADJUSTED_QC'][:]
        ncd.close()

        obs = {}
        for key in meta_keys:
            obs[key] = []
        obs['vals'] = []
        obs['errs'] = []
        obs['qc'] = []

        for i in range(len(dpth[1])-1):

            if ma.getmask(vals)[1][i] == 1:
                continue

            dt = epoch + timedelta(days=float(time[1]))
            obs['dateTime'].append(np.int64(round((dt - epoch).total_seconds())))
            obs['latitude'].append(ma.getdata(lats)[1])
            obs['longitude'].append(ma.getdata(lons)[1])
            obs['depthBelowWaterSurface'].append(ma.getdata(dpth)[1][i])
            obs['vals'].append(ma.getdata(vals)[1][i])
            obs['errs'].append(ma.getdata(errs)[1][i])
            obs['qc'].append(0)

        self.data = obs

        return


class IODA(object):

    def __init__(self, files_input, filename, date, obsList):

        '''
        Initialize IODA writer class,
        transform to IODA data structure and,
        write out to IODA file.
        '''

        self.filename = filename
        self.date = date

        self.GlobalAttrs = {
            'converter': os.path.basename(__file__),
            'ioda_version': 2,
            'sourceFiles': ", ".join(files_input),
            'datetimeReference': self.date.strftime('%Y-%m-%dT%H:%M:%S%z'),
            'description': "GODAE Profile Observations of chlorophyll (BGC-Argo)"
        }

        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        # Set units and FillValue attributes for groups associated with observed variable.
        self.varAttrs[(varInfo[0], obsValName)]['units'] = varInfo[1]
        self.varAttrs[(varInfo[0], obsErrName)]['units'] = varInfo[1]
        self.varAttrs[(varInfo[0], obsValName)]['_FillValue'] = varInfo[2]
        self.varAttrs[(varInfo[0], obsErrName)]['_FillValue'] = varInfo[2]
        self.varAttrs[(varInfo[0], qcName)]['_FillValue'] = int(varInfo[2])

        # data is the dictionary containing IODA friendly data structure
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        nobs = 0
        for obs in obsList:

            nobs += len(obs.data['vals'])
            if nobs <= 0:
                print('No observations for IODA!')
                continue

            # Set units of the MetaData variables and all _FillValues.
            for key in meta_keys:
                dtypestr = locationKeyList[meta_keys.index(key)][1]
                if locationKeyList[meta_keys.index(key)][2]:
                    self.varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
                self.varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]
                self.data[(key, metaDataName)] = np.array(obs.data[key], dtype=dtypes[dtypestr])

            # Fill up the final array of observed values, obsErrors, and Qc
            self.data[(varInfo[0], obsValName)] = np.array(obs.data['vals'], dtype=float)
            self.data[(varInfo[0], obsErrName)] = np.array(obs.data['errs'], dtype=float)
            self.data[(varInfo[0], qcName)] = np.array(obs.data['qc'], dtype=float)

        # Initialize the writer, then write the file.
        DimDict = {'Location': nobs}
        self.writer = iconv.IodaWriter(self.filename, locationKeyList, DimDict)
        self.writer.BuildIoda(self.data, varDims, self.varAttrs, self.GlobalAttrs)

        return


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Read BGC-Argo chlorophyll profile from godae'
            ' and convert to IODA v2 format.'
        )
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="name of BGC-Argo observation input file(s)",
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

    fList = args.input
    foutput = args.output
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    obsList = []
    for fname in fList:
        obs = Profile(fname, fdate)       # Read in argo profiles
        obsList.append(obs)

    # Write the output file.
    IODA(fList, foutput, fdate, obsList)


if __name__ == '__main__':
    main()
