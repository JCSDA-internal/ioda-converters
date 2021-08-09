#!/usr/bin/env python3

#
# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
from datetime import datetime
from scipy.io import FortranFile
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict


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

        data['n_obs'], data['n_lvl'], data['n_vrsn'] = fh.read_ints('>i4')

        print('    number profiles: %d' % data['n_obs'])
        print('  max number levels: %d' % data['n_lvl'])
        print('file version number: %d' % data['n_vrsn'])

        if data['n_obs'] <= 0:
            print('No profile observations to process from %s' % self.filename)
            return

        data['ob_btm'] = fh.read_reals('>f4')
        data['ob_lat'] = fh.read_reals('>f4')
        data['ob_lon'] = fh.read_reals('>f4')
        data['ob_ls'] = fh.read_reals('>i4')
        data['ob_lt'] = fh.read_reals('>i4')
        data['ob_ssh'] = fh.read_reals('>f4')
        data['ob_sst'] = fh.read_reals('>f4')
        data['ob_sal_typ'] = fh.read_reals('>i4')
        data['ob_sal_qc'] = fh.read_reals('>f4')
        data['ob_tmp_typ'] = fh.read_reals('>i4')
        data['ob_tmp_qc'] = fh.read_reals('>f4')

        data['ob_lvl'] = []
        data['ob_sal'] = []
        data['ob_sal_err'] = []
        data['ob_sal_prb'] = []
        data['ob_tmp'] = []
        data['ob_tmp_err'] = []
        data['ob_tmp_prb'] = []

        for n in range(data['n_obs']):
            data['ob_lvl'].append(fh.read_reals('>f4'))
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

        self.data = data

        return


class IODA(object):

    def __init__(self, filename, date, varDict, obsList):
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
            ("depth", "float"),
            ("datetime", "string")
        ]

        self.AttrData = {
            'odb_version': 1,
            'date_time_string': self.date.strftime("%Y-%m-%dT%H:%M:%SZ")
        }

        self.writer = iconv.NcWriter(self.filename, self.locKeyList)

        self.keyDict = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        for key in self.varDict.keys():
            value = self.varDict[key]
            self.keyDict[key]['valKey'] = value, self.writer.OvalName()
            self.keyDict[key]['errKey'] = value, self.writer.OerrName()
            self.keyDict[key]['qcKey'] = value, self.writer.OqcName()

        # data is the dictionary containing IODA friendly data structure
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        recKey = 0

        for obs in obsList:

            if obs.data['n_obs'] <= 0:
                print('No profile observations for IODA!')
                continue

            for n in range(obs.data['n_obs']):

                lat = obs.data['ob_lat'][n]
                lon = obs.data['ob_lon'][n]
                dtg = datetime.strptime(obs.data['ob_dtg'][n], '%Y%m%d%H%M')

                for ilev, lvl in enumerate(obs.data['ob_lvl'][n]):

                    locKey = lat, lon, lvl, dtg.strftime("%Y-%m-%dT%H:%M:%SZ")

                    for key in self.varDict.keys():

                        val = obs.data[key][n][ilev]
                        err = obs.data[key+'_err'][n][ilev]
                        qc = (100 * obs.data[key+'_qc'][n]).astype('i4')

                        valKey = self.keyDict[key]['valKey']
                        errKey = self.keyDict[key]['errKey']
                        qcKey = self.keyDict[key]['qcKey']

                        self.data[recKey][locKey][valKey] = val
                        self.data[recKey][locKey][errKey] = err
                        self.data[recKey][locKey][qcKey] = qc

                recKey += 1

        (ObsVars, LocMdata, VarMdata) = self.writer.ExtractObsData(self.data)
        self.writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, self.AttrData)

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

    varDict = {
        'ob_tmp': 'sea_water_temperature',
        'ob_sal': 'sea_water_salinity'
    }

    IODA(foutput, fdate, varDict, obsList)


if __name__ == '__main__':
    main()
