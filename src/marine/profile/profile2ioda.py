#!/usr/bin/env python

import os
import sys
import numpy as np
from scipy.io import FortranFile

def rd_prof(fname):

    try:
        fh = FortranFile(fname, mode='r', header_dtype='>i4')
    except IOError:
        raise IOError('%s file not found!' % fname)
    except Exception:
        raise Exception('Unknown error opening %s' % fname)

    n_obs, n_lvl, n_vrsn = fh.read_ints('>i4')

    print('    number profiles: %d' % n_obs)
    print('  max number levels: %d' % n_lvl)
    print('file version number: %d' % n_vrsn)

    if n_obs <= 0:
        print('No profile observations to process from %s' % fname)
        print('EXITING!')
        sys.exit(0)

    # Dictionary to hold all data
    odata = {}

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

    for n in range(n_obs):
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

    for n in range(n_obs):
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

    if n_vrsn > 1:
        odata['ob_sal_xvl'] = fh.read_reals('>f4')
        odata['ob_sal_xsd'] = fh.read_reals('>f4')
        odata['ob_tmp_xvl'] = fh.read_reals('>f4')
        odata['ob_tmp_xsd'] = fh.read_reals('>f4')

        #if n_vrsn > 2:
        #    For some reason ob_id has characters that don't conform.
        #    ob_id is not used, so comment out for now
        #    odata['ob_id'] = fh.read_record('>S10').astype('U10')

    fh.close()

    return odata

fname = sys.argv[1]
odata = rd_prof(fname)




