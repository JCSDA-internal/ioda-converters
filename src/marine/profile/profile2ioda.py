#!/usr/bin/env python

from __future__ import print_function
import os
import sys
import numpy as np
from scipy.io import FortranFile
from netCDF4 import Dataset

def rd_prof(fname, datetime='YYYYMMDDHH'):

    try:
        fh = FortranFile(fname, mode='r', header_dtype='>i4')
    except IOError:
        raise IOError('%s file not found!' % fname)
    except Exception:
        raise Exception('Unknown error opening %s' % fname)

    # Dictionary to hold all data from file
    odata = {}

    odata['datetime'] = datetime

    odata['n_obs'], odata['n_lvl'], odata['n_vrsn'] = fh.read_ints('>i4')

    print('    number profiles: %d' % odata['n_obs'])
    print('  max number levels: %d' % odata['n_lvl'])
    print('file version number: %d' % odata['n_vrsn'])

    if odata['n_obs'] <= 0:
        print('No profile observations to process from %s' % fname)
        return odata

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

        #if odata['n_vrsn'] > 2:
        #    For some reason ob_id has characters that don't conform.
        #    ob_id is not used, so comment out for now
        #    odata['ob_id'] = fh.read_record('>S10').astype('U10')

    fh.close()

    return odata


def to_nlocs(idata):


    odata = {}

    odata['datetime'] = idata['datetime']

    odata['ob_lat'] = []
    odata['ob_lon'] = []
    odata['ob_lvl'] = []
    odata['ob_dtg'] = []
    odata['ob_sal'] = []
    odata['ob_sal_err'] = []
    odata['ob_tmp'] = []
    odata['ob_tmp_err'] = []

    for n in range(idata['n_obs']):

        lat = idata['ob_lat'][n]
        lon = idata['ob_lon'][n]
        dtg = idata['ob_dtg'][n]

        for l, lvl in enumerate(idata['ob_lvl'][n]):

            odata['ob_lat'].append(lat)
            odata['ob_lon'].append(lon)
            odata['ob_dtg'].append(dtg)
            odata['ob_lvl'].append(lvl)
            odata['ob_sal'].append(idata['ob_sal'][n][l])
            odata['ob_sal_err'].append(idata['ob_sal_err'][n][l])
            odata['ob_tmp'].append(idata['ob_tmp'][n][l])
            odata['ob_tmp_err'].append(idata['ob_tmp_err'][n][l])

    return odata


def toioda(fname, odata):

    ncid = Dataset(fname, 'w', format='NETCDF4')

    ncid.createDimension('nlocs', len(odata['ob_lat']))

    longitudes = ncid.createVariable('longitude', 'f4', ('nlocs'))
    longitudes.units = 'degrees_east'
    latitudes = ncid.createVariable('latitude', 'f4', ('nlocs'))
    latitudes.units = 'degrees_north'
    times = ncid.createVariable('time', 'f4', ('nlocs'))
    times.long_name = 'observation_time_YYYYMMDDHHMMSS'
    levels = ncid.createVariable('level', 'f4', ('nlocs'))
    levels.units = 'meters'
    levels.positive = 'down'
    sals = ncid.createVariable('ocean_salinity@ObsValue', 'f4', ('nlocs'))
    sals.units = 'g/kg?'
    sal_errs = ncid.createVariable('ocean_salinity@ObsError', 'f4', ('nlocs'))
    sal_errs.units = 'sal.units^2'
    tmps = ncid.createVariable('insitu_temperature@ObsValue', 'f4', ('nlocs'))
    tmps.units = 'degree_celcius'
    tmp_errs = ncid.createVariable('insitu_temperature@ObsError', 'f4', ('nlocs'))
    tmp_errs.units = 'degree_celcius^2'

    ncid.date_time = odata['datetime']

    longitudes[:] = np.asarray(odata['ob_lon'], dtype=np.float32)
    latitudes[:] = np.asarray(odata['ob_lat'], dtype=np.float32)
    levels[:] = np.asarray(odata['ob_lvl'], dtype=np.float32)
    times[:] = np.asarray(odata['ob_lvl'], dtype=np.float32) * 0.0 # To do!
    sals[:] = np.asarray(odata['ob_sal'], dtype=np.float32)
    sal_errs[:] = np.asarray(odata['ob_sal_err'], dtype=np.float32)
    tmps[:] = np.asarray(odata['ob_tmp'], dtype=np.float32)
    tmp_errs[:] = np.asarray(odata['ob_tmp_err'], dtype=np.float32)

    ncid.close()

    return


profname = sys.argv[1]
iodaname = sys.argv[2]
datetime = sys.argv[3]

odata = rd_prof(profname, datetime=datetime)
odata2 = to_nlocs(odata)
toioda(iodaname, odata2)


