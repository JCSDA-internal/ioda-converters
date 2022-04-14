#!/usr/bin/env python3

# (C) Copyright 2019-2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

"""
Convert GMAO ocean data to IODA netCDF4 format
"""

from __future__ import print_function
import sys
import os
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import netCDF4 as nc
import numpy as np
from datetime import datetime
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

# varDict has a numerical code to match a variable type attribute in the netCDF input file
# followed by an abbreviated name, IODA output var name, units, then acceptable min/max vals.
varDict = {
    5521: ['sal', 'salinity', '1', 0.0, 50.0],
    3073: ['temp', 'waterTemperature', 'K', 271.0, 325.0],
    5525: ['sst', 'seaSurfaceTemperature', 'K', 271.0, 325.0],
    5526: ['adt', 'absoluteDynamicTopography', 'm', -5.0, 5.0],
    5351: ['adt', 'absoluteDynamicTopography', 'm', -5.0, 5.0],   # not used
    6000: ['frac', 'seaIceFraction', '1', 0.0, 1.0],
    6001: ['thick', 'iceThickness', 'm', 0.001, 5000.0],
}

locationKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("depthBelowWaterSurface", "float", "m"),
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


class GMAOobs(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date

        # Read GMAO data
        self._read(self.date)

        return

    def _read(self, date):

        self.date = date

        data = {}

        ncd = nc.Dataset(self.filename)

        nobs = len(ncd.dimensions['nobs'])
        data['nobs'] = nobs

        # The input file(s) contain no date information, so take it from command line info.
        data['dateTime'] = np.full(nobs, np.int64(round((self.date - epoch).total_seconds())))

        data['longitude'] = ncd.variables['lon'][:].data
        data['latitude'] = ncd.variables['lat'][:].data
        data['depthBelowWaterSurface'] = ncd.variables['depth'][:].data
        types = ncd.variables['typ'][:].data
        values = ncd.variables['value'][:].data
        errors = ncd.variables['oerr'][:].data

        for key in varDict.keys():
            key_var = varDict[key][0] + "_vals"
            key_err = varDict[key][0] + "_errs"
            data[key_var] = np.full(nobs, float_missing_value)
            data[key_err] = np.full(nobs, float_missing_value)
            ind = np.where(types == key)
            data[key_var][ind] = values[ind]
            data[key_err][ind] = errors[ind]

        ncd.close()

        self.data = data

        return


class IODA(object):

    def __init__(self, files_input, filename, date, obsList):

        self.files_input = files_input
        self.filename = filename
        self.date = date

        GlobalAttrs = {
            'odb_version': 1,
            'converter': os.path.basename(__file__),
            'ioda_version': 2,
            'sourceFiles': ", ".join(self.files_input),
            'datetimeReference': self.date.strftime('%Y-%m-%dT%H:%M:%S%z'),
            'description': "GMAO Ocean Observations"
        }

        varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        # Set units of the MetaData variables and all _FillValues.
        for key in meta_keys:
            dtypestr = locationKeyList[meta_keys.index(key)][1]
            if locationKeyList[meta_keys.index(key)][2]:
                varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
            varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]

        # Set units and FillValue attributes for groups associated with observed variable.
        for key in varDict.keys():
            variable = varDict[key][1]
            units = varDict[key][2]
            varAttrs[(variable, obsValName)]['units'] = units
            varAttrs[(variable, obsErrName)]['units'] = units
            varAttrs[(variable, obsValName)]['_FillValue'] = float_missing_value
            varAttrs[(variable, obsErrName)]['_FillValue'] = float_missing_value
            varAttrs[(variable, qcName)]['_FillValue'] = int_missing_value

        # Skip out if there are no obs!
        totalObs = 0
        for obs in obsList:
            if obs.data['nobs'] <= 0:
                continue
            totalObs += obs.data['nobs']
        if totalObs == 0:
            print('No %s observations for IODA!' % varName)
            return

        # data is the dictionary containing IODA friendly data structure
        data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        nlocs = 0
        for obs in obsList:
            nlocs = obs.data['nobs']
            for n in range(nlocs):
                # Transfer the MetaData info into the IODA final data container.
                for key in meta_keys:
                    dtypestr = locationKeyList[meta_keys.index(key)][1]
                    val = obs.data[key][n]
                    varVals = np.array(val, dtype=dtypes[dtypestr])
                    # If on the first time through, set the data dict entry
                    # to a numpy array with a specified data type. Otherwise, append
                    # the incoming data to the current data.
                    if (key, metaDataName) in data:
                        data[(key, metaDataName)] = np.append(
                            data[(key, metaDataName)], varVals)
                    else:
                        data[(key, metaDataName)] = varVals

                varDims = {}
                # Fill up the final array of observed values and obsErrors
                for key in varDict.keys():
                    key_var = varDict[key][0] + "_vals"
                    key_err = varDict[key][0] + "_errs"
                    variable = varDict[key][1]
                    min_val = varDict[key][3]
                    max_val = varDict[key][4]

                    if all(x == float_missing_value for x in obs.data[key_var]):
                        continue

                    varDims = {key_var: ['Location']}

                    # ObsValue
                    varVals = np.array(obs.data[key_var][n], dtype=dtypes['float'])
                    if 'Temperature' in variable:
                        if varVals != float_missing_value:
                            varVals = varVals + 273.15
                    if (varVals < min_val or varVals > max_val):
                        varVals = float_missing_value
                    if (variable, obsValName) in data:
                        data[(variable, obsValName)] = np.append(
                            data[(variable, obsValName)], np.float32(varVals))
                    else:
                        data[(variable, obsValName)] = np.float32(varVals)

                    # ObsError
                    varVals = np.array(obs.data[key_err][n], dtype=dtypes['float'])
                    if (variable, obsErrName) in data:
                        data[(variable, obsErrName)] = np.append(
                            data[(variable, obsErrName)], np.float32(varVals))
                    else:
                        data[(variable, obsErrName)] = np.float32(varVals)

                    # QC (preQC) value (zero for now)
                    if (variable, qcName) in data:
                        data[(variable, qcName)] = np.append(data[(variable, qcName)], np.int32(0))
                    else:
                        data[(variable, qcName)] = np.int32(0)

        print(f"Found a total number of observations: {nlocs}")

        # Initialize the writer, then write the file.
        DimDict = {'Location': nlocs}
        self.writer = iconv.IodaWriter(self.filename, locationKeyList, DimDict)
        self.writer.BuildIoda(data, varDims, varAttrs, GlobalAttrs)

        return


def main():

    parser = ArgumentParser(
        description=__doc__,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the input GMAO ocean obs file(s)',
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output', help='template name of the output IODA file (one per type)',
        type=str, required=True)
    parser.add_argument(
        '-d', '--date', help='datetime at the middle of the window', metavar='YYYYMMDDHH',
        type=str, required=True)
    parser.add_argument(
        '--inputdates', help='dates of the input GMAO ocean obs file(s)',
        type=str, nargs='+', required=False, metavar='YYYYMMDDHH')

    args = parser.parse_args()

    fList = args.input
    dList = args.inputdates
    foutput = args.output
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    if dList:
        assert len(dList) == len(fList)
        dList = [datetime.strptime(d, '%Y%m%d%H') for d in dList]
    else:
        dList = [fdate] * len(fList)

    obsList = []
    for fname, idate in zip(fList, dList):
        if not os.path.isfile(fname):
            parser.error('Input (-i option) file: ', fname, ' does not exist')

        obsList.append(GMAOobs(fname, idate))

    IODA(fList, foutput, fdate, obsList)


if __name__ == '__main__':
    main()
