#!/usr/bin/env python3

#
# (C) Copyright 2019-2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import numpy as np
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

unitDict = {
    'ob_sst': 'degree_C',
    'ob_sal': 'PSU',
    'ob_uuu': 'm/s',
    'ob_vvv': 'm/s'
}


class trak(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date

        # Read trak data
        self._rd_trak()

        return

    def _rd_trak(self):
        '''
        Read the trak data
        Based on subroutine rd_trak in ocn_obs.f
        '''

        try:
            fh = FortranFile(self.filename, mode='r', header_dtype='>u4')
        except IOError:
            raise IOError('%s file not found!' % self.filename)
        except Exception:
            raise Exception('Unknown error opening %s' % self.filename)

        # data is the dictionary with data structure as in ocn_obs.f
        data = {}

        data['n_obs'], data['n_lvl'], data['n_vrsn'] = fh.read_ints('>i4')

        print('    number trak obs: %d' % data['n_obs'])
        print('  max number levels: %d' % data['n_lvl'])
        print('file version number: %d' % data['n_vrsn'])

        if data['n_obs'] <= 0:
            print('No trak observations to process from %s' % self.filename)
            return

        data['ob_wm'] = fh.read_reals('>i4')
        data['ob_gsal'] = fh.read_reals('>f4')
        data['ob_gsst'] = fh.read_reals('>f4')
        data['ob_lat'] = fh.read_reals('>f4')
        data['ob_lon'] = fh.read_reals('>f4')
        data['ob_age'] = fh.read_reals('>f4')
        data['ob_csal'] = fh.read_reals('>f4')
        data['ob_csst'] = fh.read_reals('>f4')
        data['ob_qc_sal'] = fh.read_reals('>f4')
        data['ob_qc_sst'] = fh.read_reals('>f4')
        data['ob_qc_vel'] = fh.read_reals('>f4')
        data['ob_rsal'] = fh.read_reals('>f4')
        data['ob_rsst'] = fh.read_reals('>f4')
        data['ob_sal'] = fh.read_reals('>f4')
        data['ob_sst'] = fh.read_reals('>f4')
        data['ob_typ'] = fh.read_reals('>i4')
        data['ob_uuu'] = fh.read_reals('>f4')
        data['ob_vvv'] = fh.read_reals('>f4')
        data['ob_dtg'] = fh.read_record('>S12').astype('U12')
        data['ob_rcpt'] = fh.read_record('>S12').astype('U12')
        data['ob_scr'] = fh.read_record('>S1').astype('U1')
        data['ob_sgn'] = fh.read_record('>S6').astype('U6')
        data['ob_csgm'] = fh.read_reals('>f4')
        data['ob_gsgm'] = fh.read_reals('>f4')
        data['ob_rsgm'] = fh.read_reals('>f4')

        fh.close()

        self.data = data

        return


class IODA(object):

    def __init__(self, filename, date, varDict, varDims, obsList):
        '''
        Initialize IODA writer class,
        transform to IODA data structure and,
        write out to IODA file.
        '''

        self.filename = filename
        self.date = date
        self.varDict = varDict

        self.locKeyList = [
            ("latitude", "float"),
            ("longitude", "float"),
            ("datetime", "string")
        ]

        self.GlobalAttrs = {
        }

        self.keyDict = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        for key in self.varDict.keys():
            value = self.varDict[key]
            self.keyDict[key]['valKey'] = value, iconv.OvalName()
            self.keyDict[key]['errKey'] = value, iconv.OerrName()
            self.keyDict[key]['qcKey'] = value, iconv.OqcName()
            self.varAttrs[value, iconv.OvalName()]['_FillValue'] = -999.
            self.varAttrs[value, iconv.OerrName()]['_FillValue'] = -999.
            self.varAttrs[value, iconv.OqcName()]['_FillValue'] = -999
            self.varAttrs[value, iconv.OvalName()]['units'] = unitDict[key]
            self.varAttrs[value, iconv.OerrName()]['units'] = unitDict[key]
            self.varAttrs[value, iconv.OqcName()]['units'] = 'unitless'

        # data is the dictionary containing IODA friendly data structure
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        for obs in obsList:

            if obs.data['n_obs'] <= 0:
                print('No trak observations for IODA!')
                continue

            for n in range(obs.data['n_obs']):

                lat = obs.data['ob_lat'][n]
                lon = obs.data['ob_lon'][n]
                dtg = datetime.strptime(obs.data['ob_dtg'][n], '%Y%m%d%H%M')

                locKey = lat, lon, dtg.strftime("%Y-%m-%dT%H:%M:%SZ")

                for key in self.varDict.keys():

                    if key in ['ob_uuu', 'ob_vvv']:
                        varName = 'vel'
                    else:
                        varName = key.split('_')[-1]

                    val = obs.data[key][n]
                    err = 1.0
                    qc = (100*obs.data['ob_qc_'+varName][n]).astype('i4')

                    valKey = self.keyDict[key]['valKey']
                    errKey = self.keyDict[key]['errKey']
                    qcKey = self.keyDict[key]['qcKey']

                    self.data[locKey][valKey] = val
                    self.data[locKey][errKey] = err
                    self.data[locKey][qcKey] = qc
        # Extract obs
        ObsVars, nlocs = iconv.ExtractObsData(self.data, self.locKeyList)
        DimDict = {'nlocs': nlocs}

        # Set up IODA writer
        self.writer = iconv.IodaWriter(self.filename, self.locKeyList, DimDict)

        # Write out observations
        self.writer.BuildIoda(ObsVars, varDims, self.varAttrs, self.GlobalAttrs)

        return


def main():

    desc = 'Convert GODAE binary track data to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the binary GODAE track file',
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
        obsList.append(trak(fname, fdate))

    varDict = {
        'ob_sst': 'sea_surface_temperature',
        'ob_sal': 'sea_surface_salinity',
        'ob_uuu': 'sea_surface_zonal_wind',
        'ob_vvv': 'sea_surface_meriodional_wind'
    }

    varDims = {
        'sea_surface_temperature': ['nlocs'],
        'sea_surface_salinity': ['nlocs'],
        'sea_surface_zonal_wind': ['nlocs'],
        'sea_surface_meriodional_wind': ['nlocs']
    }

    IODA(foutput, fdate, varDict, varDims, obsList)


if __name__ == '__main__':
    main()
