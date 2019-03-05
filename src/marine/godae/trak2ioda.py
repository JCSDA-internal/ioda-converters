#!/usr/bin/env python

from __future__ import print_function
import numpy as np
from datetime import datetime
from scipy.io import FortranFile, netcdf
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter


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
            print('No trak observations for IODA!')
            return

        self._to_nlocs()

        ncid = netcdf.netcdf_file(self.filename + '.nc', mode='w')

        ncid.createDimension('nlocs', self.idata['nlocs'])  # no. of obs per var type
        ncid.createDimension('nrecs', 1)  # no. of track obs
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

        sal = ncid.createVariable('sea_surface_salinity@ObsValue', 'f4', ('nlocs',))
        sal.units = 'g/kg'
        sal.description = ''

        sal_errs = ncid.createVariable(
            'sea_surface_salinity@ObsError', 'f4', ('nlocs',))
        sal_errs.units = '(g/kg)^2'
        sal_errs.description = 'Standard deviation of observation error'

        sst = ncid.createVariable(
            'sea_surface_temperature@ObsValue', 'f4', ('nlocs',))
        sst.units = 'degree_celcius'
        sst.description = ''

        sst_errs = ncid.createVariable(
            'sea_surface_temperature@ObsError', 'f4', ('nlocs',))
        sst_errs.units = '(degree_celcius)^2'
        sst_errs.description = 'Standard deviation of observation error'

        u = ncid.createVariable(
            'sea_surface_zonal_wind@ObsValue', 'f4', ('nlocs',))
        u.units = 'm/s'
        u.description = ''

        u_errs = ncid.createVariable(
            'sea_surface_zonal_wind@ObsError', 'f4', ('nlocs',))
        u_errs.units = '(m/s)^2'
        u_errs.description = 'Standard deviation of observation error'

        v = ncid.createVariable(
            'sea_surface_meridional_wind@ObsValue', 'f4', ('nlocs',))
        v.units = 'm/s'
        v.description = ''

        v_errs = ncid.createVariable(
            'sea_surface_meridional_wind@ObsError', 'f4', ('nlocs',))
        v_errs.units = '(m/s)^2'
        v_errs.description = 'Standard deviation of observation error'

        ncid.date_time = int(datetime.strftime(self.date, '%Y%m%d%H%M')[0:10])

        longitudes[:] = np.asarray(self.idata['ob_lon'], dtype=np.float32)
        latitudes[:] = np.asarray(self.idata['ob_lat'], dtype=np.float32)
        times[:] = np.asarray(self.idata['ob_dtg'], dtype=np.float32)

        sal[:] = np.asarray(self.idata['ob_sal'], dtype=np.float32)
        sal_errs[:] = 1.0

        sst[:] = np.asarray(self.idata['ob_sst'], dtype=np.float32)
        sst_errs[:] = 1.0

        u[:] = np.asarray(self.idata['ob_uuu'], dtype=np.float32)
        u_errs[:] = 1.0

        v[:] = np.asarray(self.idata['ob_vvv'], dtype=np.float32)
        v_errs[:] = 1.0

        ncid.close()

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
        '-d', '--date', help='file date', type=str, required=True)

    args = parser.parse_args()

    fname = args.name
    fdate = datetime.strptime(args.date, '%Y%m%d%H%M')

    track = trak(fname, fdate)
    track.to_ioda()
