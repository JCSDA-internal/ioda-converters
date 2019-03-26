#!/usr/bin/env python

from __future__ import print_function
import numpy as np
from datetime import datetime
from scipy.io import FortranFile
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from collections import defaultdict
import ioda_conv_ncio as iconv


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
            fh = FortranFile(self.filename, mode='r', header_dtype='>i4')
        except IOError:
            raise IOError('%s file not found!' % self.filename)
        except Exception:
            raise Exception('Unknown error opening %s' % self.filename)

        # odata is the dictionary with data structure as in ocn_obs.f
        odata = {}

        odata['n_obs'], odata['n_lvl'], odata['n_vrsn'] = fh.read_ints('>i4')

        print('    number trak obs: %d' % odata['n_obs'])
        print('  max number levels: %d' % odata['n_lvl'])
        print('file version number: %d' % odata['n_vrsn'])

        if odata['n_obs'] <= 0:
            print('No trak observations to process from %s' % self.filename)
            return

        odata['ob_wm'] = fh.read_reals('>i4')
        odata['ob_gsal'] = fh.read_reals('>f4')
        odata['ob_gsst'] = fh.read_reals('>f4')
        odata['ob_lat'] = fh.read_reals('>f4')
        odata['ob_lon'] = fh.read_reals('>f4')
        odata['ob_age'] = fh.read_reals('>f4')
        odata['ob_csal'] = fh.read_reals('>f4')
        odata['ob_csst'] = fh.read_reals('>f4')
        odata['ob_qc_sal'] = fh.read_reals('>f4')
        odata['ob_qc_sst'] = fh.read_reals('>f4')
        odata['ob_qc_vel'] = fh.read_reals('>f4')
        odata['ob_rsal'] = fh.read_reals('>f4')
        odata['ob_rsst'] = fh.read_reals('>f4')
        odata['ob_sal'] = fh.read_reals('>f4')
        odata['ob_sst'] = fh.read_reals('>f4')
        odata['ob_typ'] = fh.read_reals('>i4')
        odata['ob_uuu'] = fh.read_reals('>f4')
        odata['ob_vvv'] = fh.read_reals('>f4')
        odata['ob_dtg'] = fh.read_record('>S12').astype('U12')
        odata['ob_rcpt'] = fh.read_record('>S12').astype('U12')
        odata['ob_scr'] = fh.read_record('>S1').astype('U1')
        odata['ob_sgn'] = fh.read_record('>S6').astype('U6')
        odata['ob_csgm'] = fh.read_reals('>f4')
        odata['ob_gsgm'] = fh.read_reals('>f4')
        odata['ob_rsgm'] = fh.read_reals('>f4')

        fh.close()

        self.odata = odata

        return

    def to_ioda(self):
        '''
        Selectively convert odata into idata.
        idata is the IODA required data structure
        '''

        if self.odata['n_obs'] <= 0:
            print('No trak observations for IODA!')
            return

        locationKeyList = [
            ("latitude", "float"),
            ("longitude", "float"),
            ("date_time", "string")
        ]

        AttrData = {
            'odb_version': 1,
            'date_time_string': self.date.strftime("%Y-%m-%dT%H:%M:%SZ")
        }

        writer = iconv.NcWriter(self.filename+'.nc', [], locationKeyList)

        # idata is the dictionary containing IODA friendly data structure
        idata = defaultdict(lambda: defaultdict(dict))

        var1Name = 'sea_surface_temperature'
        val1Key = var1Name, writer.OvalName()
        err1Key = var1Name, writer.OerrName()
        qc1Key = var1Name, writer.OqcName()

        var2Name = 'sea_surface_salinity'
        val2Key = var2Name, writer.OvalName()
        err2Key = var2Name, writer.OerrName()
        qc2Key = var2Name, writer.OqcName()

        var3Name = 'sea_surface_zonal_wind'
        val3Key = var3Name, writer.OvalName()
        err3Key = var3Name, writer.OerrName()
        qc3Key = var3Name, writer.OqcName()

        var4Name = 'sea_surface_meriodional_wind'
        val4Key = var4Name, writer.OvalName()
        err4Key = var4Name, writer.OerrName()
        qc4Key = var4Name, writer.OqcName()

        for n in range(self.odata['n_obs']):

            lat = self.odata['ob_lat'][n]
            lon = self.odata['ob_lon'][n]
            dtg = datetime.strptime(self.odata['ob_dtg'][n], '%Y%m%d%H%M')

            locKey = lat, lon, dtg.strftime("%Y-%m-%dT%H:%M:%SZ")

            idata[0][locKey][val1Key] = self.odata['ob_sst'][n]
            idata[0][locKey][err1Key] = 1.0
            idata[0][locKey][qc1Key] = self.odata['ob_qc_sst'][n]

            idata[0][locKey][val2Key] = self.odata['ob_sal'][n]
            idata[0][locKey][err2Key] = 1.0
            idata[0][locKey][qc2Key] = self.odata['ob_qc_sal'][n]

            idata[0][locKey][val3Key] = self.odata['ob_uuu'][n]
            idata[0][locKey][err3Key] = 1.0
            idata[0][locKey][qc3Key] = self.odata['ob_qc_vel'][n]

            idata[0][locKey][val4Key] = self.odata['ob_vvv'][n]
            idata[0][locKey][err4Key] = 1.0
            idata[0][locKey][qc4Key] = self.odata['ob_qc_vel'][n]

        writer.BuildNetcdf(idata, AttrData)

        return


if __name__ == '__main__':

    desc = 'Read GODAE binary track file and convert to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-n', '--name', help='name of the binary GODAE track file (path)',
        type=str, required=True)
    parser.add_argument(
        '-d', '--date', help='file date', type=str, metavar='YYYYMMDDHH', required=True)

    args = parser.parse_args()

    fname = args.name
    fdate = datetime.strptime(args.date, '%Y%m%d%H%M')

    track = trak(fname, fdate)
    track.to_ioda()
