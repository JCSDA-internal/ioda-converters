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

varInfo = ['seaSurfaceTemperature', 'K', -999.]
varDims = {'seaSurfaceTemperature': ['Location']}

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

float_missing_value = -999.
int_missing_value = -99900
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


class ship(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date

        # Read ship data
        self._rd_ship()

        return

    def _rd_ship(self):
        '''
        Read the surface obs data
        Based on subroutine rd_ship in ocn_obs.f
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

        print('    number ship obs: %d' % data['n_obs'])
        print('  max number levels: %d' % data['n_lvl'])
        print('file version number: %d' % data['n_vrsn'])

        if data['n_obs'] <= 0:
            print('No ship observations to process from %s' % self.filename)
            return

        data['ob_wm'] = fh.read_reals('>i4')
        data['ob_glb'] = fh.read_reals('>f4')
        data['latitude'] = fh.read_reals('>f4')
        data['longitude'] = fh.read_reals('>f4')
        data['ob_age'] = fh.read_reals('>f4')
        data['ob_clm'] = fh.read_reals('>f4')
        data['ob_qc'] = fh.read_reals('>f4')
        data['ob_rgn'] = fh.read_reals('>f4')
        data['ob_sst'] = fh.read_reals('>f4')
        data['ob_typ'] = fh.read_reals('>i4')
        data['ob_dtg'] = fh.read_record('>S12').astype('U12')
        data['ob_rcpt'] = fh.read_record('>S12').astype('U12')
        data['ob_scr'] = fh.read_record('>S1').astype('U1')

        if data['n_vrsn'] <= 2:
            print('verify ob_sign for version = %d' % data['n_vrsn'])
            data['ob_sign'] = fh.read_record('S6').astype('U6')
        else:
            data['ob_sign'] = fh.read_record('>S7').astype('U7')

        if data['n_vrsn'] > 1:
            data['ob_csgm'] = fh.read_reals('>f4')
            data['ob_gsgm'] = fh.read_reals('>f4')
            data['ob_rsgm'] = fh.read_reals('>f4')
        else:
            minus999 = np.ones(data['n_obs'], dtype=np.float32) * -999.
            data['ob_csgm'] = minus999
            data['ob_gsgm'] = minus999
            data['ob_rsgm'] = minus999

        fh.close()

        # Transfer timestamp into seconds since epoch and convert Celcius to Kelvin
        for n in range(data['n_obs']):
            dtg = datetime.strptime(data['ob_dtg'][n], '%Y%m%d%H%M')
            time_offset = np.int64(round((dtg - epoch).total_seconds()))
            data['dateTime'].append(time_offset)
            data['ob_sst'][n] = data['ob_sst'][n] + 273.15

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

        self.GlobalAttrs = {
            'converter': os.path.basename(__file__),
            'ioda_version': 2,
            'sourceFiles': ", ".join(files_input),
            'datetimeReference': self.date.strftime('%Y-%m-%dT%H:%M:%S%z'),
            'description': "GODAE Ship Observations of sea surface temperature"
        }

        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        # Set units and FillValue attributes for groups associated with observed variable.
        self.varAttrs[(varInfo[0], obsValName)]['units'] = varInfo[1]
        self.varAttrs[(varInfo[0], obsErrName)]['units'] = varInfo[1]
        self.varAttrs[(varInfo[0], obsValName)]['_FillValue'] = varInfo[2]
        self.varAttrs[(varInfo[0], obsErrName)]['_FillValue'] = varInfo[2]
        self.varAttrs[(varInfo[0], qcName)]['_FillValue'] = int_missing_value

        # data is the dictionary containing IODA friendly data structure
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        nobs = 0
        for obs in obsList:

            nobs += obs.data['n_obs']
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
            self.data[(varInfo[0], obsValName)] = np.array(obs.data['ob_sst'], dtype=np.float32)
            self.data[(varInfo[0], obsErrName)] = np.full(nobs, 0.5, dtype=np.float32)
            self.data[(varInfo[0], qcName)] = np.array(obs.data['ob_qc']*100, dtype=np.int32)

        # Initialize the writer, then write the file.
        DimDict = {'Location': nobs}
        self.writer = iconv.IodaWriter(self.filename, locationKeyList, DimDict)
        self.writer.BuildIoda(self.data, varDims, self.varAttrs, self.GlobalAttrs)

        return


def main():

    desc = 'Convert GODAE binary ship data to IODA netCDF4 format'
    parser = ArgumentParser(description=desc,
                            formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the binary GODAE ship file',
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output', help='name of the output netCDF GODAE ship file',
        type=str, required=True, default=None)
    parser.add_argument(
        '-d', '--date', help='file date',
        type=str, metavar='YYYYMMDDHH', required=True)

    args = parser.parse_args()

    fList = args.input
    foutput = args.output
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    obsList = []
    for fname in fList:
        obsList.append(ship(fname, fdate))

    IODA(fList, foutput, fdate, obsList)


if __name__ == '__main__':
    main()
