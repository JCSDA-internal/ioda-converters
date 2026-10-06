#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

# Description:
#        This code reads the MUSICA IASI "AllTargetProducts" standard output
#        (IASIx_MUSICA_*_L2_AllTargetProducts_*.nc) and writes the retrieved
#        deltaD (deuterium) profile, its error, a priori, averaging kernel,
#        geolocation and quality information into IODA format.
#
#        Unlike the dedicated MUSICA IASI water vapour isotopologue pair
#        product ("swi"), the AllTargetProducts file does NOT contain
#        musica_deltad* variables or a pre-built deltaD averaging kernel.
#        Everything is therefore derived here from the retrieved water
#        vapour state {[H2O], [HDO]}:
#
#          deltaD(z)        = 1000 * ( [HDO](z) / [H2O](z) - 1 )          [permil]
#          deltaDApriori(z) = 1000 * ( [HDO_ap](z) / [H2O_ap](z) - 1 )   [permil]
#
#        The [HDO] profile stored in musica_wv is already normalised by the
#        SMOW ratio, so [HDO]/[H2O] == 1 corresponds to deltaD == 0.
#
#        The deltaD averaging kernel is the averaging kernel of the MUSICA
#        proxy state component  p2 = ln[HDO] - ln[H2O]  ( ~= deltaD/1000 ).
#        It is reconstructed from the singular value decomposition of the
#        {ln[H2O], ln[HDO]} averaging kernel stored in the input file:
#
#          A_deltaD[n,i,j] = sum_k val[n,k] * Lp2[n,k,i] * Rp2[n,k,j]
#            Lp2 =        lvec[:,HDO] - lvec[:,H2O]
#            Rp2 = 0.5 * (rvec[:,HDO] - rvec[:,H2O])
#
#        This reconstruction is validated by the fact that trace(A_deltaD)
#        reproduces musica_wvp_dofs[:, deltaD] and that the row sums of
#        A_deltaD reproduce musica_wvp_response[:, deltaD, :].
#
#        The deltaD error is derived from the proxy error covariance
#        (musica_wvp_error, natural-log / relative units) by
#          sigma(deltaD) = (1000 + deltaD) * sigma(p2)                    [permil]
#
# Usage:
#        python musica_iasi_deltad2ioda.py -i musica_iasi_file.nc [file2.nc ...] \
#             -o musica_iasi_ioda.nc -q 2 -n 0.0 -e noise
#        -i: one or more MUSICA IASI L2 AllTargetProducts netCDF input files
#        -o: IODA output file path
#        -q: optional minimum musica_fit_quality_flag to keep, default 2 (fair)
#        -n: optional random thinning fraction from 0.0 to 1.0, default 0.0
#        -e: optional deltaD error component, default "noise"
#              noise       : retrieval fit noise            (musica_wvp_error param 1)
#              instrument  : IASI instrumental noise         (musica_wvp_error param 2)
#              temperature : atmospheric temperature a priori (musica_wvp_error param 3)
#              total       : sqrt(noise**2 + temperature**2)
#        --max-error   : level deltaD error above this (permil) is flagged PreQC bad, default 40
#
# Supported versions: MUSICA IASI full retrieval v3.2.1, v3.3.0 and v3.3.1
# (29 atmospheric levels). Files from these versions can be concatenated.
#
# TODO: MUSICA IASI v4.0.0 (Full_v040000_beta2025) is NOT supported yet; the
# converter stops with an error on any file whose data_version is not 3.x.
# v4 differs from v3 in:
#   - 20 atmospheric levels instead of 29, so v3 and v4 files cannot be mixed
#   - species dimension renamed musica_wv_species_id, with 3 members {H2O, HDO, T}
#   - proxy state {ln[H2O], ln[HDO]-ln[H2O], ln[H2O]-ln[H2Osat]}, so the deltaD
#     averaging kernel right vector becomes Rp2 = rvec[:,HDO] (no 0.5*(HDO-H2O))
#   - musica_wvp_dofs is per level (AVK diagonal), not per sounding
#   - musica_wvp_error parameters 2/3 are RH / humidity a priori, not
#     instrument / temperature
#   - musica_wvp_response is not reproduced by the row sums of the
#     reconstructed kernel; its definition needs confirmation from KIT
#
# The output layout mirrors test/testoutput/tropomi_no2_total.nc:
#     MetaData/                 (Location)
#     ObsValue/deltaDeuterium           (Location, Layer)
#     ObsError/deltaDeuterium           (Location, Layer)
#     PreQC/deltaDeuterium              (Location, Layer)
#     RetrievalAncillaryData/   averagingKernel_<j> (Location, Layer)  [row j of the
#                                   deltaD averaging kernel matrix, j = 0..Layer-1]
#                               deltaDeuteriumApriori / airPressure / altitude (Location, Layer)
#                               measurementResponse (Location, Layer)  [row sum of the
#                                   deltaD averaging kernel, reference only, not used in PreQC]
#
# NOTE: the deltaD averaging kernel is a Layer x Layer matrix per sounding.
# The IODA python writer only supports up to 2-D arrays, so the matrix is
# stored one row per variable (averagingKernel_<j>), following the same
# convention used by tropess_co_nc2ioda.py for profile averaging kernels.
# The kernel is dimensionless and refers to the natural-log proxy state
# (ln[HDO] - ln[H2O]), consistent with the MUSICA proxy definition.
#

import argparse
import netCDF4 as nc
import numpy as np
import os
import sys
from datetime import datetime, timezone

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

# constants
# IODA default fill values, keep them in sync with the IODA engines
float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

# MUSICA IASI reports time as seconds since 2000-01-01, IODA dateTime is
# seconds since the JEDI epoch (1970-01-01T00:00:00Z, see def_jedi_utils)
musica_epoch_string = 'seconds since 2000-01-01 00:00:00'
musica_epoch = datetime(2000, 1, 1, tzinfo=timezone.utc)
epoch_offset = np.int64((musica_epoch - epoch).total_seconds())

# musica_species_id indices of the retrieved water vapour state {[H2O], [HDO]}
# and, equivalently, of the proxy state {0.5*(ln[H2O]+ln[HDO]), (ln[HDO]-ln[H2O])}
species_id = {"h2o": 0, "deltad": 1}

# musica_fit_quality_flag values, soundings with a flag below the requested
# level are dropped
fit_quality = {"poor": 0, "restricted": 1, "fair": 2, "good": 3}

# WMO satellite identifiers (Common Code Table C-5) of the Metop platforms,
# keyed by the MUSICA instrument flag (1: IASI-A, 2: IASI-B, 3: IASI-C)
wmo_satellite_id = {1: 4, 2: 3, 3: 5}

# musica_wvp_error parameter indices (error_parameter dimension)
error_param = {"noise": 0, "instrument": 1, "temperature": 2}

# PreQC values written for each retrieval level
qc_good = 0
qc_bad = 1

# deltaD, its error and its a priori are written in units of permil.

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

obsVar = {
    'musica_deltad': 'deltaDeuterium',
}

varDims = {
    'deltaDeuterium': ['Location', 'Layer'],
    'deltaDeuteriumApriori': ['Location', 'Layer'],
    'airPressure': ['Location', 'Layer'],
    'altitude': ['Location', 'Layer'],
    'measurementResponse': ['Location', 'Layer'],
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(1),
}

DimDict = {}


class musica(object):
    def __init__(self, filenames, min_quality, thin, err_kind, max_error):
        self.filenames = filenames
        self.min_quality = min_quality
        self.thin = thin
        self.err_kind = err_kind
        self.max_error = max_error
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.nlevs = None
        self._read()

    def _set_var_metadata(self):
        iodavar = obsVar['musica_deltad']
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'permil'
        self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'permil'
        self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = float_missing_value
        self.varAttrs[iodavar, iconv.OerrName()]['_FillValue'] = float_missing_value
        self.varAttrs[iodavar, iconv.OqcName()]['_FillValue'] = int_missing_value

    @staticmethod
    def _clean(arr):
        """
        Return the array as float64 with missing entries set to 0.0, and the
        mask of those missing entries.

        The SVD arrays are padded beyond each sounding's rank (and beyond
        musica_nol in the level dimension). The padding is NaN in v3 files,
        although a _FillValue is declared, and masked in v4 files. Setting it
        to 0.0 removes it from the reconstruction sum.
        """
        out = np.ma.filled(arr, np.nan).astype(np.float64)
        missing = ~np.isfinite(out)
        out[missing] = 0.0
        return out, missing

    def _reconstruct_avk(self, ncd, nol):
        """
        Reconstruct the deltaD averaging kernel matrix (nlocs, nlevs, nlevs)
        from the singular value decomposition of the {ln[H2O], ln[HDO]}
        averaging kernel stored in the input file.

        The MUSICA proxy transform is
            p1 = 0.5 * (ln[H2O] + ln[HDO])
            p2 =        ln[HDO] - ln[H2O]        (~= deltaD / 1000)
        so the p2 -> p2 block of the proxy averaging kernel is

            A[n,i,j] = sum_k val[n,k] * Lp2[n,k,i] * Rp2[n,k,j]
            Lp2 =        lvec[:,HDO] - lvec[:,H2O]
            Rp2 = 0.5 * (rvec[:,HDO] - rvec[:,H2O])

        Validated by trace(A) == musica_wvp_dofs[:, deltaD] and
        A.sum(axis=2) == musica_wvp_response[:, deltaD, :].

        Returns the kernel and a (nlocs,) mask of soundings whose SVD arrays
        have missing entries inside the valid region (k < musica_wv_avk_rank,
        level < musica_nol). Zeroing those would give a wrong kernel, so the
        caller drops these soundings.
        """
        lvec, lmiss = self._clean(ncd['musica_wv_avk_lvec'][:])  # (nlocs, 2, rank, nlevs)
        rvec, rmiss = self._clean(ncd['musica_wv_avk_rvec'][:])  # (nlocs, 2, rank, nlevs)
        val, vmiss = self._clean(ncd['musica_wv_avk_val'][:])    # (nlocs, rank)

        # valid region of the SVD arrays
        rank = np.ma.filled(ncd['musica_wv_avk_rank'][:], 0).astype(np.int32)
        in_rank = np.arange(val.shape[1])[np.newaxis, :] < rank[:, np.newaxis]
        in_lev = np.arange(lvec.shape[-1])[np.newaxis, :] < nol[:, np.newaxis]
        in_vec = in_rank[:, np.newaxis, :, np.newaxis] & in_lev[:, np.newaxis, np.newaxis, :]
        bad = (vmiss & in_rank).any(axis=1) \
            | (lmiss & in_vec).any(axis=(1, 2, 3)) \
            | (rmiss & in_vec).any(axis=(1, 2, 3))
        if bad.any():
            print('WARNING: %d sounding(s) have missing averaging kernel SVD entries '
                  'inside their rank / level range and are dropped' % bad.sum(), flush=True)

        h2o = species_id['h2o']
        hdo = species_id['deltad']
        lp2 = lvec[:, hdo, :, :] - lvec[:, h2o, :, :]
        rp2 = 0.5 * (rvec[:, hdo, :, :] - rvec[:, h2o, :, :])
        avk = np.einsum('nk,nki,nkj->nij', val, lp2, rp2,
                        optimize=True).astype(np.float32)
        return avk, bad

    def _read_file(self, f):
        print("Reading: {}".format(f), flush=True)
        ncd = nc.Dataset(f, 'r')

        nlocs = ncd.dimensions['observation_id'].size
        nlevs = ncd.dimensions['atmospheric_levels'].size

        # only the v3 layout is supported (see the TODO in the header)
        data_version = ncd.getncattr('data_version')
        if not data_version.startswith('3.'):
            print('ERROR: unsupported MUSICA IASI data_version "%s" in %s' % (data_version, f))
            print('       only v3.2.1, v3.3.0 and v3.3.1 are supported')
            sys.exit(1)
        if self.nlevs is not None and nlevs != self.nlevs:
            print('ERROR: %s has %d atmospheric levels, previous input files have %d'
                  % (f, nlevs, self.nlevs))
            print('       files with different numbers of levels cannot be concatenated')
            sys.exit(1)
        self.nlevs = nlevs

        # global attributes
        AttrData['sensor'] = ncd.getncattr('experiment')
        AttrData['platform'] = ncd.getncattr('name_of_platform')
        AttrData['musica_data_version'] = data_version
        AttrData['institution'] = ncd.getncattr('institution')
        AttrData['title'] = ncd.getncattr('title')

        # geolocation
        lats = np.ma.filled(ncd['lat'][:], float_missing_value).astype(np.float32)
        lons = np.ma.filled(ncd['lon'][:], float_missing_value).astype(np.float32)

        # time: seconds since 2000-01-01 -> seconds since 1970-01-01
        time_units = ncd['time'].units
        if time_units != musica_epoch_string:
            print('ERROR: unexpected time units in %s' % (f))
            print('       expected "%s", found "%s"' % (musica_epoch_string, time_units))
            sys.exit(1)
        times = ncd['time'][:].astype(np.int64) + epoch_offset

        # per-sounding fields
        nol = np.ma.filled(ncd['musica_nol'][:], 0).astype(np.int32)
        fit_flag = np.ma.filled(ncd['musica_fit_quality_flag'][:],
                                fit_quality['poor']).astype(np.int32)
        vza = np.ma.filled(ncd['platform_zenith_angle'][:], float_missing_value).astype(np.float32)
        instrument = np.ma.filled(ncd['instrument'][:], int_missing_value).astype(np.int32)
        sat_id = np.full(nlocs, int_missing_value, dtype=np.int32)
        for flag, wmo_id in wmo_satellite_id.items():
            sat_id[instrument == flag] = wmo_id
        scan_pos = np.ma.filled(ncd['across_track'][:], int_missing_value).astype(np.int32)
        # cloud cover is stored in percent (despite units "1") and is NaN when
        # not available, convert to a fraction and map NaN to the IODA fill value
        cloud = np.ma.filled(ncd['eumetsat_cloud_area_fraction'][:], np.nan).astype(np.float32)
        cloud = np.where(np.isfinite(cloud), 0.01 * cloud, float_missing_value).astype(np.float32)
        sfc_type = np.ma.filled(ncd['eumetsat_surface_type_flag'][:],
                                int_missing_value).astype(np.int32)
        dofs = np.ma.filled(ncd['musica_wvp_dofs'][:, species_id['deltad']],
                            float_missing_value).astype(np.float32)

        # retrieved and a priori water vapour state {[H2O], [HDO]} (nlocs, 2, nlevs)
        wv = np.ma.filled(ncd['musica_wv'][:], np.nan).astype(np.float64)
        wv_ap = np.ma.filled(ncd['musica_wv_apriori'][:], np.nan).astype(np.float64)
        h2o = wv[:, species_id['h2o'], :]
        hdo = wv[:, species_id['deltad'], :]
        h2o_ap = wv_ap[:, species_id['h2o'], :]
        hdo_ap = wv_ap[:, species_id['deltad'], :]

        # deltaD from the SMOW-normalised isotopologue ratio, in permil
        with np.errstate(invalid='ignore', divide='ignore'):
            deltad = 1000.0 * (hdo / h2o - 1.0)
            deltad_ap = 1000.0 * (hdo_ap / h2o_ap - 1.0)

        # deltaD error: proxy error (natural-log / relative) -> permil
        #   sigma(deltaD) = (1000 + deltaD) * sigma(p2)
        wvp_err = np.ma.filled(ncd['musica_wvp_error'][:], np.nan).astype(np.float64)
        wvp_err = wvp_err[:, :, species_id['deltad'], :]  # (nlocs, error_parameter, nlevs)
        if self.err_kind == 'total':
            sig_p2 = np.sqrt(wvp_err[:, error_param['noise'], :] ** 2
                             + wvp_err[:, error_param['temperature'], :] ** 2)
        else:
            sig_p2 = wvp_err[:, error_param[self.err_kind], :]
        deltad_err = (1000.0 + deltad) * sig_p2

        # measurement response (row sum of the deltaD averaging kernel)
        # written as ancillary data for reference, not used in PreQC
        response = np.ma.filled(ncd['musica_wvp_response'][:, species_id['deltad'], :],
                                float_missing_value).astype(np.float32)

        # profile grid
        pressure = np.ma.filled(ncd['musica_pressure_levels'][:],
                                float_missing_value).astype(np.float32)
        altitude = np.ma.filled(ncd['musica_altitude_levels'][:],
                                float_missing_value).astype(np.float32)

        # valid-level mask: the first nol levels (TOA -> surface) hold data
        levidx = np.arange(nlevs)[np.newaxis, :]
        valid_lev = levidx < nol[:, np.newaxis]
        valid_lev &= np.isfinite(deltad) & np.isfinite(deltad_err)

        # PreQC: qc_good on levels with a deltaD error below the requested
        # threshold, qc_bad otherwise, fill value on levels that are not part
        # of the retrieval.
        good_lev = valid_lev & (np.abs(deltad_err) <= self.max_error)
        preqc = np.where(good_lev, qc_good, qc_bad).astype(np.int32)
        preqc[~valid_lev] = int_missing_value

        # cast profile fields and blank out invalid levels
        deltad = deltad.astype(np.float32)
        deltad_err = deltad_err.astype(np.float32)
        deltad_ap = deltad_ap.astype(np.float32)
        for arr in (deltad, deltad_err, deltad_ap, pressure, altitude, response):
            arr[~valid_lev] = float_missing_value

        # averaging kernel (nlocs, nlevs, nlevs), rows/cols outside the
        # retrieval blanked out
        avk, avk_bad = self._reconstruct_avk(ncd, nol)
        avk[~valid_lev, :] = float_missing_value
        avk[np.broadcast_to(~valid_lev[:, np.newaxis, :], avk.shape)] = float_missing_value

        ncd.close()

        # sounding selection: fit quality, at least one valid level, a complete
        # averaging kernel, thinning
        keep = (fit_flag >= self.min_quality) & (nol > 0) & valid_lev.any(axis=1) & ~avk_bad
        if self.thin > 0.0:
            keep = np.logical_and(keep, np.random.uniform(size=nlocs) > self.thin)

        data = {
            ('latitude', 'MetaData'): lats[keep],
            ('longitude', 'MetaData'): lons[keep],
            ('dateTime', 'MetaData'): times[keep],
            ('sensorZenithAngle', 'MetaData'): vza[keep],
            ('sensorScanPosition', 'MetaData'): scan_pos[keep],
            ('satelliteIdentifier', 'MetaData'): sat_id[keep],
            ('cloudCoverTotal', 'MetaData'): cloud[keep],
            ('surfaceQualifier', 'MetaData'): sfc_type[keep],
            ('numberLevels', 'MetaData'): nol[keep],
            ('fitQualityFlag', 'MetaData'): fit_flag[keep],
            ('degreesOfFreedom', 'MetaData'): dofs[keep],
            self.varDict['deltaDeuterium']['valKey']: deltad[keep],
            self.varDict['deltaDeuterium']['errKey']: deltad_err[keep],
            self.varDict['deltaDeuterium']['qcKey']: preqc[keep],
            ('deltaDeuteriumApriori', 'RetrievalAncillaryData'): deltad_ap[keep],
            ('airPressure', 'RetrievalAncillaryData'): pressure[keep],
            ('altitude', 'RetrievalAncillaryData'): altitude[keep],
            ('measurementResponse', 'RetrievalAncillaryData'): response[keep],
        }
        # store the averaging kernel matrix one row per variable (2-D writer limit)
        avk = avk[keep]
        for j in range(nlevs):
            data[('averagingKernel_'+str(j), 'RetrievalAncillaryData')] = avk[:, j, :]
        return data

    def _read(self):
        self._set_var_metadata()

        first = True
        for f in self.filenames:
            data = self._read_file(f)
            if first:
                for key, val in data.items():
                    self.outdata[key] = val
                first = False
            else:
                for key, val in data.items():
                    self.outdata[key] = np.concatenate((self.outdata[key], val))

        nlocs = len(self.outdata[('dateTime', 'MetaData')])
        DimDict['Location'] = nlocs
        DimDict['Layer'] = self.nlevs
        AttrData['Location'] = np.int32(nlocs)
        AttrData['Layer'] = np.int32(self.nlevs)

        # per-level averaging kernel rows share the (Location, Layer) dims
        for j in range(self.nlevs):
            varDims['averagingKernel_'+str(j)] = ['Location', 'Layer']

        # MetaData units / attributes
        self.varAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string
        self.varAttrs[('dateTime', 'MetaData')]['_FillValue'] = long_missing_value
        self.varAttrs[('latitude', 'MetaData')]['units'] = 'degrees_north'
        self.varAttrs[('longitude', 'MetaData')]['units'] = 'degrees_east'
        self.varAttrs[('sensorZenithAngle', 'MetaData')]['units'] = 'degree'
        self.varAttrs[('cloudCoverTotal', 'MetaData')]['units'] = '1'

        # RetrievalAncillaryData attributes
        for j in range(self.nlevs):
            vkey = ('averagingKernel_'+str(j), 'RetrievalAncillaryData')
            self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
            self.varAttrs[vkey]['units'] = '1'
            self.varAttrs[vkey]['_FillValue'] = float_missing_value
        vkey = ('deltaDeuteriumApriori', 'RetrievalAncillaryData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'permil'
        self.varAttrs[vkey]['_FillValue'] = float_missing_value
        vkey = ('airPressure', 'RetrievalAncillaryData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'Pa'
        self.varAttrs[vkey]['_FillValue'] = float_missing_value
        vkey = ('altitude', 'RetrievalAncillaryData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'm'
        self.varAttrs[vkey]['_FillValue'] = float_missing_value
        vkey = ('measurementResponse', 'RetrievalAncillaryData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = '1'
        self.varAttrs[vkey]['_FillValue'] = float_missing_value


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Reads MUSICA IASI AllTargetProducts netCDF files and converts the '
            'retrieved deltaD (deuterium) profile, its averaging kernel and a '
            'priori into IODA formatted output. deltaD, its error and its '
            'averaging kernel are derived from the retrieved water vapour state '
            '{[H2O], [HDO]}. Multiple files can be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of MUSICA IASI L2 AllTargetProducts netCDF input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-q', '--quality',
        help="minimum musica_fit_quality_flag to retain a sounding "
        "(0:poor, 1:restricted, 2:fair, 3:good). (default: %(default)s)",
        type=int, default=fit_quality['fair'], dest='min_quality')
    optional.add_argument(
        '-n', '--thin',
        help="fraction of random thinning from 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)
    optional.add_argument(
        '-e', '--error',
        help="deltaD error component taken from musica_wvp_error: "
        "'noise' (retrieval fit noise), 'instrument' (IASI instrumental "
        "noise), 'temperature' (atmospheric temperature a priori) or "
        "'total' = sqrt(noise**2 + temperature**2). (default: %(default)s)",
        type=str, choices=['noise', 'instrument', 'temperature', 'total'],
        default='noise', dest='err_kind')
    optional.add_argument(
        '--max-error',
        help="level deltaD error (permil) above which the level is flagged "
        "PreQC bad. (default: %(default)s)",
        type=float, default=40.0, dest='max_error')

    args = parser.parse_args()

    var = musica(args.input, args.min_quality, args.thin, args.err_kind,
                 args.max_error)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    print("Writing: {}".format(args.output), flush=True)
    writer.BuildIoda(var.outdata, varDims, var.varAttrs, AttrData)


if __name__ == '__main__':
    main()
