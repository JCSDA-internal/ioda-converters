#!/usr/bin/env python3

# (C) Copyright 2019-2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

"""

This script converts GMAO ocean data to IODA format

GMAO input data could contain multiple variables from different sources including
ocean, sea-ice observations, altimeter data, and in-situ profilers. See varDict
for the full list. Hence, GMAO class reads the input data and creates an object
that contains with observations that are present in the input file. The IODA class
then reads the GMAO object, applies a simple filter, and creates the IODA file.

PreQC is set to 0 for all observations, all input data is vetted by ODAS scripts
before being converted to IODA format.

"""

from __future__ import print_function

import os
import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import netCDF4 as nc
import numpy as np
from datetime import datetime

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

# varDict has a numerical code to match a variable type attribute in the netCDF input file
# followed by an abbreviated name, IODA output var name, units, then acceptable min/max vals.
varDict = {
    3073: ['temp', 'waterTemperature', 'C', -2.0, 52.0],
    5521: ['sal', 'salinity', 'PSU', 0.0, 50.0],
    5522: ['sss', 'seaSurfaceSalinity', 'PSU', 0.0, 50.0],
    5525: ['sst', 'seaSurfaceTemperature', 'C', -2.0, 52.0],
    5526: ['adt', 'absoluteDynamicTopography', 'm', -4.0, 4.0],
    5351: ['adt', 'absoluteDynamicTopography', 'm', -4.0, 4.0],
    6000: ['frac', 'seaIceFraction', '1', 0.0, 1.0],
    6001: ['thick', 'iceThickness', 'm', 0.001, 5000.0],
}

locationKeyListBase = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z")
]
meta_keys = [m_item[0] for m_item in locationKeyListBase]

iso8601_string = locationKeyListBase[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

missing_vals = {'integer': int_missing_value,
                'long': long_missing_value,
                'float': float_missing_value,
                }

dtypes = {'integer': np.int32,
          'long': np.int64,
          'float': np.float32,
          }


class GMAOobs(object):

    def __init__(self, filename):

        self.filename = filename
        self.present_vars = set()

        # Read GMAO data
        self._read()

        return

    def _read(self):

        data = {}
        ncd = nc.Dataset(self.filename)
        nobs = len(ncd.dimensions['nobs'])
        data['nobs'] = nobs

        # The input file(s) must contain date information, fail if it does not
        if 'date_time' not in ncd.variables:
            print('date_time not found in %s' % self.filename)
            sys.exit(1)
        data['date_time'] = ncd.variables['date_time'][:].data
        data['dateTime'] = np.empty(data['nobs'], dtype=np.int64)

        # Convert date_time input format to seconds since epoch
        for n in range(data['nobs']):
            dtg_str = str(data['date_time'][n])
            dtg = datetime.strptime(dtg_str, '%Y%m%d%H')

            time_offset = np.int64(round((dtg - epoch).total_seconds()))
            data['dateTime'][n] = time_offset

        data['longitude'] = ncd.variables['lon'][:].data
        data['latitude'] = ncd.variables['lat'][:].data
        data['depth'] = ncd.variables['depth'][:].data
        types = ncd.variables['typ'][:].data
        values = ncd.variables['value'][:].data
        values[np.isnan(values)] = float_missing_value

        errors = ncd.variables['oerr'][:].data

        for key in varDict.keys():
            key_var = varDict[key][0] + "_vals"
            key_err = varDict[key][0] + "_errs"
            data[key_var] = np.ma.masked_all(nobs, dtype=dtypes['float'])
            data[key_err] = np.ma.masked_all(nobs, dtype=dtypes['float'])
            ind = np.where(types == key)

            # Mark if the variable is present in the file
            if ind[0].size > 0:
                data[key_var][ind] = values[ind]
                data[key_err][ind] = errors[ind]
                self.present_vars.add(key)
        ncd.close()

        self.data = data

        return


class IODA(object):

    def __init__(self, files_input, filename, obsList):
        self.files_input = files_input
        self.filename = filename

        GlobalAttrs = {}

        # This creates a `DefaultOrderedDict` where each new key accessed that doesn't
        # already exist in the dictionary will be automatically associated with a new `DefaultOrderedDict`
        # (which in turn will create a regular `dict` if a new key is accessed in it).
        varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        # Determine which variables are present in any of the input files
        present_vars = set().union(*(obs.present_vars for obs in obsList))

        # Filter varDict to only include present variables
        presentVarDict = {k: v for k, v in varDict.items() if k in present_vars}

        # Check if profiler data (3073 or 5521) are present
        include_depth = 3073 in present_vars or 5521 in present_vars

        # Update locationKeyList and meta_keys for the profiler data
        if include_depth:
            locationKeyList = locationKeyListBase + [("depth", "float", "m")]
        else:
            locationKeyList = locationKeyListBase
        meta_keys = [m_item[0] for m_item in locationKeyList]

        # Set units of the MetaData variables and all _FillValues.
        for key in meta_keys:
            dtypestr = locationKeyList[meta_keys.index(key)][1]
            if locationKeyList[meta_keys.index(key)][2]:
                varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
            varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]

        # Set units and FillValue attributes for groups associated with observed variable.
        for key, (_, variable, units, _, _) in presentVarDict.items():
            for attr_name in [obsValName, obsErrName]:
                varAttrs[(variable, attr_name)]['units'] = units
                varAttrs[(variable, attr_name)]['_FillValue'] = float_missing_value
            varAttrs[(variable, qcName)]['_FillValue'] = int_missing_value

        # Calculate total number of observations
        totalObs = sum(obs.data['nobs'] for obs in obsList)
        if totalObs == 0:
            print('No %s observations for IODA!' % varName)
            return

        # Create data dictionary with IODA friendly data structure
        data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        for key in meta_keys:
            dtypestr = locationKeyList[meta_keys.index(key)][1]
            fill_value = missing_vals[dtypestr]
            data[(key, metaDataName)] = np.full(totalObs, fill_value, dtype=dtypes[dtypestr])

        print(f"Present variable(s) and their attribute(s) in this input: \n{presentVarDict}")

        # Prefill data with missing values
        # QC (preQC) value is zero for now (we assume all ODAS data is good)
        for key, (_, variable, _, _, _) in presentVarDict.items():
            data[(variable, obsValName)] = np.full(totalObs, float_missing_value, dtype=dtypes['float'])
            data[(variable, obsErrName)] = np.full(totalObs, float_missing_value, dtype=dtypes['float'])
            data[(variable, qcName)] = np.full(totalObs, 0, dtype=dtypes['integer'])

        current_index = 0
        for obs in obsList:
            nlocs = obs.data['nobs']
            end_index = current_index + nlocs

            # Transfer the MetaData info
            for key in meta_keys:
                dtypestr = locationKeyList[meta_keys.index(key)][1]
                data[(key, metaDataName)][current_index:end_index] = obs.data[key].astype(dtypes[dtypestr])

            # Fill up the final array of observed values and obsErrors
            for key, (var_name, variable, _, min_val, max_val) in presentVarDict.items():

                key_var = var_name + "_vals"
                key_err = var_name + "_errs"

                if not np.all(obs.data[key_var] == float_missing_value):
                    # Ensure empty values values in masked arrays are filled with missing values
                    varVals = obs.data[key_var].filled(float_missing_value).astype(dtypes['float'])
                    errVals = obs.data[key_err].filled(float_missing_value).astype(dtypes['float'])

                    # Keep the indices of the values that are outside of the acceptable range
                    exclude = (varVals <= min_val) | (varVals >= max_val)
                    varVals = np.where(exclude, float_missing_value, varVals)
                    errVals = np.where(exclude, float_missing_value, errVals)

                    data[(variable, obsValName)][current_index:end_index] = varVals
                    data[(variable, obsErrName)][current_index:end_index] = errVals
                    # Replace the QC values with the preQC value 12 if the value is outside of the acceptable range
                    data[(variable, qcName)][current_index:end_index] = \
                        np.where(exclude, 12, data[(variable, qcName)][current_index:end_index])

            current_index = end_index

        print(f"Found a total number of observations: {totalObs}")

        # Initialize the writer, then write the file.
        DimDict = {'Location': totalObs}
        varDims = {varDict[key][0] + "_vals": ['Location'] for key in presentVarDict}

        self.writer = iconv.IodaWriter(self.filename, locationKeyList, DimDict)
        self.writer.BuildIoda(data, varDims, varAttrs, GlobalAttrs)

        return


def main():

    parser = ArgumentParser(
        description=(
            'Read GMAO ODAS observation file(s) that have already been QCd and thinned'
            ' and convert them to the IODA format.'),
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the input GMAO ocean obs file(s)',
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output', help='template name of the output IODA file (one per type)',
        type=str, required=True)

    args = parser.parse_args()

    fList = args.input
    foutput = args.output

    # obsList contains all the input GMAO obs abjects
    obsList = []
    for fname in fList:
        obsList.append(GMAOobs(fname))

    IODA(fList, foutput, obsList)


if __name__ == '__main__':
    main()
