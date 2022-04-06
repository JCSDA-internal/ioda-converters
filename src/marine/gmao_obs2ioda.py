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
import netCDF4 as nc4
import numpy as np
from datetime import datetime
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

varDict = {
    5521: ['sal', 'salinity', '1'],
    3073: ['temp', 'waterTemperature', 'K'],
    5525: ['sst', 'seaSurfaceTemperature', 'K'],
    5526: ['adt', 'absoluteDynamicTopography', 'm'],
    5351: ['adt', 'absoluteDynamicTopography', 'm'],   # not used
    6000: ['frac', 'seaIceFraction', '1'],
    6001: ['thick', 'iceThickness', 'm'],
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
        self._read()

        return

    def _read(self):

        data = {}

        nc = nc4.Dataset(self.filename)

        data['nobs'] = len(nc.dimensions['nobs'])

        data['typ'] = nc.variables['typ'][:].data
        data['longitude'] = nc.variables['lon'][:].data
        data['latitude'] = nc.variables['lat'][:].data
        data['depthBelowWaterSurface'] = nc.variables['depth'][:].data
        data['vals'] = nc.variables['value'][:].data
        data['errs'] = nc.variables['oerr'][:].data

        nc.close()

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
            'sourceFiles': ", ".join(files_input),
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

        nlevels = 0
        nlocs = 0
        for obs in obsList:
            nlevels = obs.data['nobs']
            for n in range(nlevels):
                for k, depth in enumerate(obs.data['depthBelowWaterSurface'][n]):
                    nlocs += 1
                    # Transfer the MetaData info into the IODA final data container.
                    for key in meta_keys:
                        dtypestr = locationKeyList[meta_keys.index(key)][1]
                        if isinstance(obs.data[key][n], list):
                            val = obs.data[key][n][k]
                        else:
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

                    # Fill up the final array of observed values and obsErrors
                    for key in varDict.keys():
                        key_val = varDict[key][0] + "_vals"
                        key_err = varDict[key][0] + "_errs"
                        variable = varDict[key][1]

                        # ObsValue
                        varVals = np.array(obs.data[key_val][n][k], dtype=dtypes['float'])
                        if (variable, obsValName) in data:
                            data[(variable, obsValName)] = np.append(
                                data[(variable, obsValName)], varVals);
                        else:
                            data[(variable, obsValName)] = varVals

                        # ObsError
                        varVals = np.array(obs.data[key_err][n][k], dtype=dtypes['float'])
                        if (variable, obsErrName) in data:
                            data[(variable, obsErrName)] = np.append(
                                data[(variable, obsErrName)], varVals);
                        else:
                            data[(variable, obsErrName)] = varVals

        # Just fill in the QC value as zero everywhere.
        data[(variable, qcName)] = np.full(len(data[(variable, obsValName)]), 0, dtype=np.int32)

        print(f"Found a total number of observations: {nlocs}")

        # Initialize the writer, then write the file.
        DimDict = {'Location': nlocs}
        self.writer = iconv.IodaWriter(self.filename, locationKeyList, DimDict)
        self.writer.BuildIoda(data, varDims, varAttrs, GlobalAttrs)

        return


def discardOb(varName, obsValue):

    discardOb = True

    if varName in ["sea_water_salinity"]:
        if 0. <= obsValue <= 50.:
            discardOb = False
    elif varName in ["sea_water_temperature", "sea_surface_temperature"]:
        if -2. <= obsValue <= 100.:
            discardOb = False
    elif varName in ["absolute_dynamic_topography"]:
        if -5. <= obsValue <= 5.:
            discardOb = False
    elif varName in ["sea_ice_area_fraction"]:
        if 0. <= obsValue <= 1.:
            discardOb = False
    elif varName in ["sea_ice_thickness"]:
        discardOb = False
    else:
        raise SystemExit("Unknown observation variable %s" % varName)

    return discardOb


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
        obsList.append(GMAOobs(fname, idate))

    obsDict = separateObs(obsList)

    obsDictSorted = sortDict(obsDict)

    for key, value in varDict.items():
        fout = '%s_%s.nc' % (foutput, value)
        IODA(fout, fdate, key, obsDictSorted[key])


if __name__ == '__main__':
    main()
