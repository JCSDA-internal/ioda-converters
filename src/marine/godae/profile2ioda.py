#!/usr/bin/env python

from __future__ import print_function
import numpy as np
from datetime import datetime
from scipy.io import FortranFile, netcdf
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from collections import defaultdict
import ioda_conv_ncio as iconv


vName = "obs_absolute_dynamic_topography",

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
            fh = FortranFile(self.filename, mode='r', header_dtype='>i4')
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

    def _to_nlocs(self):
        '''
        Selectively (e.g. salinity, temperature) convert odata into idata
        idata is the IODA required data structure, where profiles are stacked
        on top of each other into a single vector.
        '''

        # idata is the dictionary containing IODA friendly data structure
        idata = {}

        idata['ob_lat'] = []
        idata['ob_lon'] = []
        idata['ob_lvl'] = []
        idata['ob_dtg'] = []
        idata['ob_sal'] = []
        idata['ob_sal_err'] = []
        idata['ob_tmp'] = []
        idata['ob_tmp_err'] = []

        for n in range(self.odata['n_obs']):

            lat = self.odata['ob_lat'][n]
            lon = self.odata['ob_lon'][n]
            dtg = datetime.strptime(self.odata['ob_dtg'][n], '%Y%m%d%H%M')

            # Diff. between ob. time and time in the file (hrs)!
            #ddtg = (dtg - self.date).total_seconds()/3600.

            for l, lvl in enumerate(self.odata['ob_lvl'][n]):

                idata['ob_lat'].append(lat)
                idata['ob_lon'].append(lon)
                #idata['ob_dtg'].append(ddtg)
                idata['ob_dtg'].append(dtg)  # provide absolute time
                idata['ob_lvl'].append(lvl)
                idata['ob_sal'].append(self.odata['ob_sal'][n][l])
                idata['ob_sal_err'].append(self.odata['ob_sal_err'][n][l])
                idata['ob_tmp'].append(self.odata['ob_tmp'][n][l])
                idata['ob_tmp_err'].append(self.odata['ob_tmp_err'][n][l])

        idata['nlocs'] = len(idata['ob_dtg'])

        self.idata = idata

        return

    def to_ioda(self):

        if self.odata['n_obs'] <= 0:
            print('No profile observations for IODA!')
            return

        self._to_nlocs()

        ncid = netcdf.netcdf_file(self.filename + '.nc', mode='w')

        ncid.createDimension('nlocs', self.idata['nlocs'])  # no. of obs per var type
        ncid.createDimension('nrecs', 1)  # no. of profiles
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

        depths = ncid.createVariable('ocean_depth@MetaData', 'f4', ('nlocs',))
        depths.units = 'meters'
        depths.positive = 'down'

        sals = ncid.createVariable('ocean_salinity@ObsValue', 'f4', ('nlocs',))
        sals.units = 'g/kg'
        sals.description = ''

        sal_errs = ncid.createVariable(
            'ocean_salinity@ObsError', 'f4', ('nlocs',))
        sal_errs.units = '(g/kg)'
        sal_errs.description = 'Standard deviation of observation error'

        tmps = ncid.createVariable(
            'insitu_temperature@ObsValue', 'f4', ('nlocs',))
        tmps.units = 'degree_celcius'
        tmps.description = ''

        tmp_errs = ncid.createVariable(
            'insitu_temperature@ObsError', 'f4', ('nlocs',))
        tmp_errs.units = '(degree_celcius)'
        tmp_errs.description = 'Standard deviation of observation error'

        ncid.date_time = int(datetime.strftime(self.date, '%Y%m%d%H%M')[0:10])

        longitudes[:] = np.asarray(self.idata['ob_lon'], dtype=np.float32)
        latitudes[:] = np.asarray(self.idata['ob_lat'], dtype=np.float32)
        depths[:] = np.asarray(self.idata['ob_lvl'], dtype=np.float32)
        times[:] = np.asarray(self.idata['ob_dtg'], dtype=np.float32)

        sals[:] = np.asarray(self.idata['ob_sal'], dtype=np.float32)
        sal_errs[:] = np.asarray(self.idata['ob_sal_err'], dtype=np.float32)

        tmps[:] = np.asarray(self.idata['ob_tmp'], dtype=np.float32)
        tmp_errs[:] = 0.5 + np.asarray(self.idata['ob_tmp_err'], dtype=np.float32)

        ncid.close()

        return

    def _to_NcWriter(self, writer):
        '''
        Selectively (e.g. salinity, temperature) convert odata into idata
        idata is the IODA required data structure, where profiles are stacked
        on top of each other into a single vector.
        '''

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

                print("val = %f" % self.odata['ob_tmp'][n][l])

                idata[n][locKey][tmp_valKey] = self.odata['ob_tmp'][n][l]
                idata[n][locKey][tmp_errKey] = self.odata['ob_tmp_err'][n][l]
                idata[n][locKey][tmp_qcKey] = tmp_qc

                idata[n][locKey][sal_valKey] = self.odata['ob_sal'][n][l]
                idata[n][locKey][sal_errKey] = self.odata['ob_sal_err'][n][l]
                idata[n][locKey][sal_qcKey] = sal_qc

        self.idata = idata

        return

    def to_iodaNcWriter(self):

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

        writer = iconv.NcWriter(self.filename+'.nc', [], locationKeyList)

        self._to_NcWriter(writer)

        writer.BuildNetcdf(self.idata, AttrData)

        return


if __name__ == '__main__':

    desc = 'Read GODAE binary profile file and convert to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-n', '--name', help='name of the binary GODAE profile file (path)',
        type=str, required=True)
    parser.add_argument(
        '-d', '--date', help='file date', type=str, metavar='YYYYMMDDHH', required=True)

    args = parser.parse_args()

    fname = args.name
    fdate = datetime.strptime(args.date, '%Y%m%d%H%M')

    prof = profile(fname, fdate)
    #prof.to_ioda()
    prof.to_iodaNcWriter()
