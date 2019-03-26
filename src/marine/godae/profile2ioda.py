#!/usr/bin/env python

from __future__ import print_function
import numpy as np
from datetime import datetime
from scipy.io import FortranFile
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from collections import defaultdict
import ioda_conv_ncio as iconv


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

        # odata is the dictionary with data structure as in ocn_obs.f
        odata = {}

        odata['n_obs'], odata['n_lvl'], odata['n_vrsn'] = fh.read_ints('>i4')

        print('    number profiles: %d' % odata['n_obs'])
        print('  max number levels: %d' % odata['n_lvl'])
        print('file version number: %d' % odata['n_vrsn'])

        if odata['n_obs'] <= 0:
            print('No profile observations to process from %s' % self.filename)
            return

        odata['ob_btm'] = fh.read_reals('>f4')
        odata['ob_lat'] = fh.read_reals('>f4')
        odata['ob_lon'] = fh.read_reals('>f4')
        odata['ob_ls'] = fh.read_reals('>i4')
        odata['ob_lt'] = fh.read_reals('>i4')
        odata['ob_ssh'] = fh.read_reals('>f4')
        odata['ob_sst'] = fh.read_reals('>f4')
        odata['ob_sal_typ'] = fh.read_reals('>i4')
        odata['ob_sal_qc'] = fh.read_reals('>f4')
        odata['ob_tmp_typ'] = fh.read_reals('>i4')
        odata['ob_tmp_qc'] = fh.read_reals('>f4')

        odata['ob_lvl'] = []
        odata['ob_sal'] = []
        odata['ob_sal_err'] = []
        odata['ob_sal_prb'] = []
        odata['ob_tmp'] = []
        odata['ob_tmp_err'] = []
        odata['ob_tmp_prb'] = []

        for n in range(odata['n_obs']):
            odata['ob_lvl'].append(fh.read_reals('>f4'))
            odata['ob_sal'].append(fh.read_reals('>f4'))
            odata['ob_sal_err'].append(fh.read_reals('>f4'))
            odata['ob_sal_prb'].append(fh.read_reals('>f4'))
            odata['ob_tmp'].append(fh.read_reals('>f4'))
            odata['ob_tmp_err'].append(fh.read_reals('>f4'))
            odata['ob_tmp_prb'].append(fh.read_reals('>f4'))

        odata['ob_dtg'] = fh.read_record('>S12').astype('U12')
        odata['ob_rcpt'] = fh.read_record('>S12').astype('U12')
        odata['ob_scr'] = fh.read_record('>S1').astype('U1')
        odata['ob_sign'] = fh.read_record('>S7').astype('U7')

        odata['ob_clm_sal'] = []
        odata['ob_clm_tmp'] = []
        odata['ob_clm_ssd'] = []
        odata['ob_clm_tsd'] = []
        odata['ob_glb_sal'] = []
        odata['ob_glb_tmp'] = []
        odata['ob_glb_ssd'] = []
        odata['ob_glb_tsd'] = []
        odata['ob_mds_sal'] = []
        odata['ob_mds_tmp'] = []
        odata['ob_rgn_sal'] = []
        odata['ob_rgn_tmp'] = []
        odata['ob_rgn_ssd'] = []
        odata['ob_rgn_tsd'] = []

        for n in range(odata['n_obs']):
            odata['ob_clm_sal'].append(fh.read_reals('>f4'))
            odata['ob_clm_tmp'].append(fh.read_reals('>f4'))
            odata['ob_clm_ssd'].append(fh.read_reals('>f4'))
            odata['ob_clm_tsd'].append(fh.read_reals('>f4'))
            odata['ob_glb_sal'].append(fh.read_reals('>f4'))
            odata['ob_glb_tmp'].append(fh.read_reals('>f4'))
            odata['ob_glb_ssd'].append(fh.read_reals('>f4'))
            odata['ob_glb_tsd'].append(fh.read_reals('>f4'))
            odata['ob_mds_sal'].append(fh.read_reals('>f4'))
            odata['ob_mds_tmp'].append(fh.read_reals('>f4'))
            odata['ob_rgn_sal'].append(fh.read_reals('>f4'))
            odata['ob_rgn_tmp'].append(fh.read_reals('>f4'))
            odata['ob_rgn_ssd'].append(fh.read_reals('>f4'))
            odata['ob_rgn_tsd'].append(fh.read_reals('>f4'))

        if odata['n_vrsn'] > 1:
            odata['ob_sal_xvl'] = fh.read_reals('>f4')
            odata['ob_sal_xsd'] = fh.read_reals('>f4')
            odata['ob_tmp_xvl'] = fh.read_reals('>f4')
            odata['ob_tmp_xsd'] = fh.read_reals('>f4')

            if odata['n_vrsn'] > 2:
                # ob_id is 10 characters long, with variable spaces in front.
                # How are these legitimate fortran spaces represented in
                # python? FortranFile.read_record has difficulty, because
                # dtype does not conform
                # Since ob_id is not used, treat it as an array of characters
                # Something like this would be ideal:
                # odata['ob_id'] = fh.read_record('>S10').astype('U10')
                odata['ob_id'] = fh.read_record('>S1')

        fh.close()

        self.odata = odata

        return

    def to_ioda(self, foutput):
        '''
        Selectively convert odata into idata.
        idata is the IODA required data structure
        '''

        if self.odata['n_obs'] <= 0:
            print('No profile observations for IODA!')
            return

        locationKeyList = [
            ("latitude", "float"),
            ("longitude", "float"),
            ("level", "float"),
            ("date_time", "string")
        ]

        AttrData = {
            'odb_version': 1,
            'date_time_string': self.date.strftime("%Y-%m-%dT%H:%M:%SZ")
        }

        writer = iconv.NcWriter(foutput, [], locationKeyList)

        # idata is the dictionary containing IODA friendly data structure
        idata = defaultdict(lambda: defaultdict(dict))

        tmpName = 'sea_water_temperature'
        tmp_valKey = tmpName, writer.OvalName()
        tmp_errKey = tmpName, writer.OerrName()
        tmp_qcKey = tmpName, writer.OqcName()

        salName = 'sea_water_salinity'
        sal_valKey = salName, writer.OvalName()
        sal_errKey = salName, writer.OerrName()
        sal_qcKey = salName, writer.OqcName()

        for n in range(self.odata['n_obs']):

            lat = self.odata['ob_lat'][n]
            lon = self.odata['ob_lon'][n]
            dtg = datetime.strptime(self.odata['ob_dtg'][n], '%Y%m%d%H%M')

            tmp_qc = self.odata['ob_tmp_qc'][n]
            sal_qc = self.odata['ob_sal_qc'][n]

            for l, lvl in enumerate(self.odata['ob_lvl'][n]):

                locKey = lat, lon, lvl, dtg.strftime("%Y-%m-%dT%H:%M:%SZ")

                idata[n][locKey][tmp_valKey] = self.odata['ob_tmp'][n][l]
                idata[n][locKey][tmp_errKey] = self.odata['ob_tmp_err'][n][l]
                idata[n][locKey][tmp_qcKey] = tmp_qc

                idata[n][locKey][sal_valKey] = self.odata['ob_sal'][n][l]
                idata[n][locKey][sal_errKey] = self.odata['ob_sal_err'][n][l]
                idata[n][locKey][sal_qcKey] = sal_qc

        writer.BuildNetcdf(idata, AttrData)

        return


if __name__ == '__main__':

    desc = 'Read GODAE binary profile file and convert to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the input binary GODAE profile file',
        type=str, required=True)
    parser.add_argument(
        '-o', '--output', help='name of the output netCDF GODAE profile file',
        type=str, required=False, default=None)
    parser.add_argument(
        '-d', '--date', help='file date', metavar='YYYYMMDDHH',
        type=str, required=True)

    args = parser.parse_args()

    finput = args.input
    foutput = args.output if args.output is not None else finput+'.nc'
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    prof = profile(finput, fdate)
    prof.to_ioda(foutput)
