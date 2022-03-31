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
import numpy as np
import netCDF4 as nc
from datetime import datetime
from scipy.io import FortranFile
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

varDict = {
    'ob_tmp': ['waterTemperature', 'K'],
    'ob_sal': ['salinity', 'g kg-1']
}

varDims = {
    'waterTemperature': ['Location'],
    'salinity': ['Location'],
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

float_missing_value = -999.   #   or  nc.default_fillvals['f4']
int_missing_value = -999      #   or  nc.default_fillvals['i4']
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


class profile(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date

        # Read profile data
        self._rd_prof()

        return

    def _rd_prof(self):
        '''
        Read the profile data
        Based on subroutine rd_prof in ocn_obs.f
        '''

        try:
            fh = FortranFile(self.filename, mode='r', header_dtype='>u4')
        except IOError:
            raise IOError('%s file not found!' % self.filename)
        except Exception:
            raise Exception('Unknown error opening %s' % self.filename)

        # data is the dictionary with data structure as in ocn_obs.f
        data = {}
        data['dateTime'] = []
        data['n_obs'], data['n_lvl'], data['n_vrsn'] = fh.read_ints('>i4')

        print('    number profiles: %d' % data['n_obs'])
        print('  max number levels: %d' % data['n_lvl'])
        print('file version number: %d' % data['n_vrsn'])

        if data['n_obs'] <= 0:
            print('No profile observations to process from %s' % self.filename)
            return

        data['ob_btm'] = fh.read_reals('>f4')
        data['latitude'] = fh.read_reals('>f4')
        data['longitude'] = fh.read_reals('>f4')
        data['ob_ls'] = fh.read_reals('>i4')
        data['ob_lt'] = fh.read_reals('>i4')
        data['ob_ssh'] = fh.read_reals('>f4')
        data['ob_sst'] = fh.read_reals('>f4')
        data['ob_sal_typ'] = fh.read_reals('>i4')
        data['ob_sal_qc'] = fh.read_reals('>f4')
        data['ob_tmp_typ'] = fh.read_reals('>i4')
        data['ob_tmp_qc'] = fh.read_reals('>f4')

        data['depthBelowWaterSurface'] = []
        data['ob_sal'] = []
        data['ob_sal_err'] = []
        data['ob_sal_prb'] = []
        data['ob_tmp'] = []
        data['ob_tmp_err'] = []
        data['ob_tmp_prb'] = []

        for n in range(data['n_obs']):
            data['depthBelowWaterSurface'].append(fh.read_reals('>f4'))
            data['ob_sal'].append(fh.read_reals('>f4'))
            data['ob_sal_err'].append(fh.read_reals('>f4'))
            data['ob_sal_prb'].append(fh.read_reals('>f4'))
            data['ob_tmp'].append(fh.read_reals('>f4'))
            data['ob_tmp_err'].append(fh.read_reals('>f4'))
            data['ob_tmp_prb'].append(fh.read_reals('>f4'))

        data['ob_dtg'] = fh.read_record('>S12').astype('U12')
        data['ob_rcpt'] = fh.read_record('>S12').astype('U12')
        data['ob_scr'] = fh.read_record('>S1').astype('U1')
        data['ob_sign'] = fh.read_record('>S7').astype('U7')

        data['ob_clm_sal'] = []
        data['ob_clm_tmp'] = []
        data['ob_clm_ssd'] = []
        data['ob_clm_tsd'] = []
        data['ob_glb_sal'] = []
        data['ob_glb_tmp'] = []
        data['ob_glb_ssd'] = []
        data['ob_glb_tsd'] = []
        data['ob_mds_sal'] = []
        data['ob_mds_tmp'] = []
        data['ob_rgn_sal'] = []
        data['ob_rgn_tmp'] = []
        data['ob_rgn_ssd'] = []
        data['ob_rgn_tsd'] = []

        for n in range(data['n_obs']):
            data['ob_clm_sal'].append(fh.read_reals('>f4'))
            data['ob_clm_tmp'].append(fh.read_reals('>f4'))
            data['ob_clm_ssd'].append(fh.read_reals('>f4'))
            data['ob_clm_tsd'].append(fh.read_reals('>f4'))
            data['ob_glb_sal'].append(fh.read_reals('>f4'))
            data['ob_glb_tmp'].append(fh.read_reals('>f4'))
            data['ob_glb_ssd'].append(fh.read_reals('>f4'))
            data['ob_glb_tsd'].append(fh.read_reals('>f4'))
            data['ob_mds_sal'].append(fh.read_reals('>f4'))
            data['ob_mds_tmp'].append(fh.read_reals('>f4'))
            data['ob_rgn_sal'].append(fh.read_reals('>f4'))
            data['ob_rgn_tmp'].append(fh.read_reals('>f4'))
            data['ob_rgn_ssd'].append(fh.read_reals('>f4'))
            data['ob_rgn_tsd'].append(fh.read_reals('>f4'))

        if data['n_vrsn'] > 1:
            data['ob_sal_xvl'] = fh.read_reals('>f4')
            data['ob_sal_xsd'] = fh.read_reals('>f4')
            data['ob_tmp_xvl'] = fh.read_reals('>f4')
            data['ob_tmp_xsd'] = fh.read_reals('>f4')

            if data['n_vrsn'] > 2:
                # ob_id is 10 characters long, with variable spaces in front.
                # How are these legitimate fortran spaces represented in
                # python? FortranFile.read_record has difficulty, because
                # dtype does not conform
                # Since ob_id is not used, treat it as an array of characters
                # Something like this would be ideal:
                # data['ob_id'] = fh.read_record('>S10').astype('U10')
                data['ob_id'] = fh.read_record('>S1')

        fh.close()

        # Transfer timestamp into seconds since epoch and convert Celcius to Kelvin
        for n in range(data['n_obs']):
            dtg = datetime.strptime(data['ob_dtg'][n], '%Y%m%d%H%M')
            time_offset = np.int64(round((dtg - epoch).total_seconds()))
            data['dateTime'].append(time_offset)

            for k, depth in enumerate(data['depthBelowWaterSurface'][n]):
                data['ob_tmp'][n][k] = data['ob_tmp'][n][k] + 273.15

        self.data = data

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

        GlobalAttrs = {
            'odb_version': 1,
            'converter': os.path.basename(__file__),
            'ioda_version': 2,
            'sourceFiles': ", ".join(files_input),
            'datetimeReference': self.date.strftime('%Y-%m-%dT%H:%M:%S%z'),
            'description': "GODAE Profile Observations of salinity and temperature"
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
            value = varDict[key][0]
            units = varDict[key][1]
            varAttrs[(value, obsValName)]['units'] = units
            varAttrs[(value, obsErrName)]['units'] = units
            varAttrs[(value, obsValName)]['_FillValue'] = float_missing_value
            varAttrs[(value, obsErrName)]['_FillValue'] = float_missing_value
            varAttrs[(value, qcName)]['_FillValue'] = int_missing_value*100

        # data is the dictionary containing IODA friendly data structure
        data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        nprofs = 0
        nlocs = 0
        for obs in obsList:

            nprofs = obs.data['n_obs']
            if nprofs <= 0:
                print('No observations for IODA!')
                continue

            for n in range(nprofs):
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

                    # Fill up the final array of observed values, obsErrors, and Qc
                    for key in varDict.keys():
                        value = varDict[key][0]
                        varErr = key + '_err'
                        varQc = key + '_qc'

                        # ObsValue
                        varVals = np.array(obs.data[key][n][k], dtype=dtypes['float'])
                        if (value, obsValName) in data:
                            data[(value, obsValName)] = np.append(
                                data[(value, obsValName)], varVals);
                        else:
                            data[(value, obsValName)] = varVals

                        # ObsError
                        varVals = np.array(obs.data[varErr][n][k], dtype=dtypes['float'])
                        if (value, obsErrName) in data:
                            data[(value, obsErrName)] = np.append(
                                data[(value, obsErrName)], varVals);
                        else:
                            data[(value, obsErrName)] = varVals

                        # QC
                        varVals = np.array(obs.data[varQc][n]*100, dtype=dtypes['integer'])
                        if (value, qcName) in data:
                            data[(value, qcName)] = np.append(
                                data[(value, qcName)], varVals);
                        else:
                            data[(value, qcName)] = varVals

        print(f"Found a total number of observations: {nlocs}")

        # Initialize the writer, then write the file.
        DimDict = {'Location': nlocs}
        self.writer = iconv.IodaWriter(self.filename, locationKeyList, DimDict)
        self.writer.BuildIoda(data, varDims, varAttrs, GlobalAttrs)

        return


def main():

    desc = 'Convert GODAE binary profile data to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the input binary GODAE profile file',
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output', help='name of the output netCDF GODAE profile file',
        type=str, required=True, default=None)
    parser.add_argument(
        '-d', '--date', help='file date', metavar='YYYYMMDDHH',
        type=str, required=True)

    args = parser.parse_args()

    fList = args.input
    foutput = args.output
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    obsList = []
    for fname in fList:
        obs = profile(fname, fdate)
        obsList.append(obs)

    IODA(fList, foutput, fdate, obsList)


if __name__ == '__main__':
    main()
