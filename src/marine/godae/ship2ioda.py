#!/usr/bin/env python

from __future__ import print_function
import numpy as np
from datetime import datetime
from scipy.io import FortranFile
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from collections import defaultdict
import ioda_conv_ncio as iconv


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
            fh = FortranFile(self.filename, mode='r', header_dtype='>i4')
        except IOError:
            raise IOError('%s file not found!' % self.filename)
        except Exception:
            raise Exception('Unknown error opening %s' % self.filename)

        # odata is the dictionary with data structure as in ocn_obs.f
        odata = {}

        odata['n_obs'], odata['n_lvl'], odata['n_vrsn'] = fh.read_ints('>i4')

        print('    number ship obs: %d' % odata['n_obs'])
        print('  max number levels: %d' % odata['n_lvl'])
        print('file version number: %d' % odata['n_vrsn'])

        if odata['n_obs'] <= 0:
            print('No ship observations to process from %s' % self.filename)
            return

        odata['ob_wm'] = fh.read_reals('>i4')
        odata['ob_glb'] = fh.read_reals('>f4')
        odata['ob_lat'] = fh.read_reals('>f4')
        odata['ob_lon'] = fh.read_reals('>f4')
        odata['ob_age'] = fh.read_reals('>f4')
        odata['ob_clm'] = fh.read_reals('>f4')
        odata['ob_qc'] = fh.read_reals('>f4')
        odata['ob_rgn'] = fh.read_reals('>f4')
        odata['ob_sst'] = fh.read_reals('>f4')
        odata['ob_typ'] = fh.read_reals('>i4')
        odata['ob_dtg'] = fh.read_record('>S12').astype('U12')
        odata['ob_rcpt'] = fh.read_record('>S12').astype('U12')
        odata['ob_scr'] = fh.read_record('>S1').astype('U1')

        if odata['n_vrsn'] <= 2:
            print('verify ob_sign for version = %d' % odata['n_vrsn'])
            odata['ob_sign'] = fh.read_record('>S7').astype('U7')
        else:
            odata['ob_sign'] = fh.read_record('>S7').astype('U7')

        if odata['n_vrsn'] > 1:
            odata['ob_csgm'] = fh.read_reals('>f4')
            odata['ob_gsgm'] = fh.read_reals('>f4')
            odata['ob_rsgm'] = fh.read_reals('>f4')
        else:
            minus999 = np.ones(odata['n_obs'], dtype=np.float32) * -999.
            odata['ob_csgm'] = minus999
            odata['ob_gsgm'] = minus999
            odata['ob_rsgm'] = minus999

        fh.close()

        self.odata = odata

        return

    def to_ioda(self):
        '''
        Selectively convert odata into idata.
        idata is the IODA required data structure
        '''

        if self.odata['n_obs'] <= 0:
            print('No ship observations for IODA!')
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

        varName = 'sea_surface_temperature'
        valKey = varName, writer.OvalName()
        errKey = varName, writer.OerrName()
        qcKey = varName, writer.OqcName()

        for n in range(self.odata['n_obs']):

            lat = self.odata['ob_lat'][n]
            lon = self.odata['ob_lon'][n]
            dtg = datetime.strptime(self.odata['ob_dtg'][i], '%Y%m%d%H%M')

            locKey = lat, lon, dtg.strftime("%Y-%m-%dT%H:%M:%SZ")

            idata[0][locKey][valKey] = self.odata['ob_sst'][n]
            idata[0][locKey][errKey] = 0.5
            idata[0][locKey][qcKey] = self.odata['ob_qc'][n]

        writer.BuildNetcdf(idata, AttrData)

        return


if __name__ == '__main__':

    desc = 'Read GODAE binary ship file and convert to IODA netCDF4 format'
    parser = ArgumentParser(description=desc,
                            formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-n', '--name', help='name of the binary GODAE ship file (path)',
        type=str, required=True)
    parser.add_argument(
        '-d', '--date', help='file date', type=str, metavar='YYYYMMDDHH', required=True)

    args = parser.parse_args()

    fname = args.name
    fdate = datetime.strptime(args.date, '%Y%m%d%H%M')

    sfcobs = ship(fname, fdate)
    sfcobs.to_ioda()
