#!/usr/bin/env python

from __future__ import print_function
import numpy as np
from datetime import datetime
from scipy.io import FortranFile, netcdf
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter


class sfcobs(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date

        # Read sfcobs data
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

    def _to_nlocs(self):
        '''
        Selectively (e.g. salinity, temperature) convert odata into idata
        idata is the IODA required data structure
        '''

        # idata is the dictionary containing IODA friendly data structure
        idata = self.odata

        # Diff. between ob. time and time in the file (hrs)!
        for i in range(self.odata['n_obs']):
            dtg = datetime.strptime(self.odata['ob_dtg'][i], '%Y%m%d%H%M')
            idata['ob_dtg'][i] = (dtg - self.date).seconds/3600.

        idata['nlocs'] = len(idata['ob_dtg'])

        self.idata = idata

        return

    def to_ioda(self):

        if self.odata['n_obs'] <= 0:
            print('No sfcobs observations for IODA!')
            return

        self._to_nlocs()

        ncid = netcdf.netcdf_file(self.filename + '.nc', mode='w')

        ncid.createDimension('nlocs', self.idata['nlocs'])  # no. of obs per var type
        ncid.createDimension('nrecs', 1)  # no. of obs.
        ncid.createDimension('nvars', 1)  # no. of variables
        ncid.createDimension('nobs', 1*self.idata['nlocs'])  # total no. of obs

        longitudes = ncid.createVariable('longitude@MetaData', 'f4', ('nlocs',))
        longitudes.units = 'degrees_east'

        latitudes = ncid.createVariable('latitude@MetaData', 'f4', ('nlocs',))
        latitudes.units = 'degrees_north'

        times = ncid.createVariable('time@MetaData', 'f4', ('nlocs',))
        times.long_name = 'observation time from date_time'
        times.units = 'hours'
        times.description = 'why does this have to be with a reference to, why not absolute?'

        tmps = ncid.createVariable(
            'sea_surface_temperature@ObsValue', 'f4', ('nlocs',))
        tmps.units = 'degree_celcius'
        tmps.description = ''

        tmp_errs = ncid.createVariable(
            'sea_surface_temperature@ObsError', 'f4', ('nlocs',))
        tmp_errs.units = '(degree_celcius)^2'
        tmp_errs.description = 'Standard deviation of observation error; Fixed at 0.5'

        ncid.date_time = int(datetime.strftime(self.date, '%Y%m%d%H%M')[0:10])

        longitudes[:] = np.asarray(self.idata['ob_lon'], dtype=np.float32)
        latitudes[:] = np.asarray(self.idata['ob_lat'], dtype=np.float32)
        times[:] = np.asarray(self.idata['ob_dtg'], dtype=np.float32)

        tmps[:] = np.asarray(self.idata['ob_sst'], dtype=np.float32)
        tmp_errs[:] = 0.5

        ncid.close()

        return


if __name__ == '__main__':

    desc = 'Read GODAE binary ship file and convert to IODA netCDF4 format'
    parser = ArgumentParser(description=desc,
                            formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-f', '--filename', help='name of the binary GODAE ship file (path)',
        type=str, required=True)
    parser.add_argument(
        '-d', '--date', help='file date', type=str, required=True)

    args = parser.parse_args()

    fname = args.filename
    fdate = datetime.strptime(args.date, '%Y%m%d%H%M')

    sfcobs = sfcobs(fname, fdate)
    sfcobs.to_ioda()
