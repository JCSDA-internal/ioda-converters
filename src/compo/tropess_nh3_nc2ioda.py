#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

# Description:
#        Reads TROPESS AIRS/CrIS L2 *Summary* product netCDF files for
#        ammonia (NH3) and converts the total column retrieval into IODA
#        format. Multiple input files can be concatenated.
#
#        Unlike the TROPESS *Standard* product (see tropess_co_nc2ioda.py),
#        the Summary product does not carry the full averaging kernel
#        matrix or the observation_ops group. Instead it provides a
#        linearized *column* averaging kernel (ak_col, units mol m-2)
#        that applies to the total column `col` and operates on
#        perturbations in ln(VMR):
#
#            c_hat = c_a + sum_l ak_col[l] * ( ln(x_model[l]) - ln(xa[l]) )
#
#        where c_a is the a priori total column, computed here from the
#        product contents as:
#
#            c_a = col_dry_air * sum_l pwf_col[l] * xa[l]
#
#        To keep the UFO forward model simple this converter precomputes
#        the constant part and stores it as RtrvlAncData/aprioriTerm:
#
#            aprioriTerm = c_a - sum_l ak_col[l] * ln(xa[l])
#
#        so that the obs operator only needs:
#
#            hofx = aprioriTerm + sum_l ak_col[l] * ln(x_model[l])
#
#        with x_model the dry-air VMR profile interpolated to the
#        retrieval pressure levels (RtrvlAncData/pressureLevel).
#
#        Vertical ordering is kept in the native TROPESS convention:
#        level index 0 is the surface (nominal 1040 hPa) and the last
#        index is the top of the atmosphere (0.1 hPa). Retrieval levels
#        below the surface are fill values in the product; here the
#        corresponding averaging kernel and pressure weighting entries
#        are set to zero so they contribute nothing to the column sum.
#
# Usage:
#        python tropess_nh3_nc2ioda.py -i TROPESS_CrIS-SNPP_L2_Summary_NH3_*.nc \
#             -o tropess_cris_nh3.nc [--land-only] [--day-only] [-n 0.5]

import argparse
import os

import numpy as np
import xarray as xr
from numpy import log as ln

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict

# constants
HPA2PA = 1E2
FLOAT_FILL = 9.969209968386869e+36  # netCDF default float fill used by ioda engines

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "string"),
]

obsvar = "ammoniaTotalColumn"


class tropess_nh3(object):

    def __init__(self, filenames, land_only, day_only, thin):
        self.filenames = filenames
        self.land_only = land_only
        self.day_only = day_only
        self.thin = thin
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.AttrData = {
            'converter': os.path.basename(__file__),
            'nvars': np.int32(1),
        }
        self.DimDict = {}
        self._setup_var_dicts()
        self._read()

    def _setup_var_dicts(self):
        self.varDict[obsvar]['valKey'] = obsvar, iconv.OvalName()
        self.varDict[obsvar]['errKey'] = obsvar, iconv.OerrName()
        self.varDict[obsvar]['qcKey'] = obsvar, iconv.OqcName()
        for key in ['valKey', 'errKey', 'qcKey']:
            self.varAttrs[self.varDict[obsvar][key]]['coordinates'] = 'longitude latitude'
        self.varAttrs[self.varDict[obsvar]['valKey']]['units'] = 'mol m-2'
        self.varAttrs[self.varDict[obsvar]['errKey']]['units'] = 'mol m-2'

    def _read(self):

        first = True
        for filename in self.filenames:

            print('FILENAME: ', filename, flush=True)

            try:
                ds = xr.open_dataset(filename)
                ds_geo = xr.open_dataset(filename, group='geolocation')
                ds_phy = xr.open_dataset(filename, group='geophysical')
            except IOError:
                raise IOError('%s file not found!' % filename)
            except Exception:
                raise Exception('Unknown error opening %s' % filename)

            # global attributes
            self.AttrData['sensor'] = ds.attrs['Instrument']
            self.AttrData['platform'] = ds.attrs['Platform']
            self.AttrData['product_source'] = ds.attrs.get('source', '')

            # coordinates
            lats = ds['latitude'].values
            lons = ds['longitude'].values

            # time: use time_tai93 decoded by xarray. Note the product's
            # datetime_utc variable is known to be offset by ~10 s relative
            # to time_tai93 (leap seconds); as in tropess_co_nc2ioda.py we
            # use `time`, which is more than accurate enough for DA windows.
            times = ds['time'].dt.strftime("%Y-%m-%dT%H:%M:%SZ").values.astype(object)

            # pressure levels, surface -> TOA (hPa in file)
            pressure = ds['pressure'].values
            nlocs, nlevs = pressure.shape

            # retrieved and a priori VMR profiles (mol/mol dry air)
            x = ds['x'].values
            xa = ds['xa'].values

            # column quantities (mol m-2)
            col = ds['col'].values
            col_error = ds['col_error'].values
            col_dry_air = ds['col_dry_air'].values

            # linearized column averaging kernel (mol m-2, applies to ln VMR
            # perturbations) and pressure weighting function (unitless)
            ak_col = ds['ak_col'].values
            pwf_col = ds['pwf_col'].values

            # ancillary flags / geometry
            view_ang = ds_geo['cris_view_ang'].values if 'cris_view_ang' in ds_geo \
                else np.full(nlocs, FLOAT_FILL, dtype=np.float32)
            land_flag = ds_phy['land_flag'].values
            day_flag = ds_phy['day_night_flag'].values
            target_id = ds['target_id'].values

            ds.close()
            ds_geo.close()
            ds_phy.close()

            # --- a priori column and precomputed a priori term ---
            # Levels below the surface are fill (NaN after decoding); zero
            # out their AK/PWF contribution so sums are well defined.
            ak_valid = np.isfinite(ak_col)
            pwf_valid = np.isfinite(pwf_col)
            ak_z = np.where(ak_valid, ak_col, 0.0)
            pwf_z = np.where(pwf_valid, pwf_col, 0.0)

            # ln(xa) only where AK contributes; guard against xa <= 0
            with np.errstate(divide='ignore', invalid='ignore'):
                ln_xa = np.where(ak_valid & (xa > 0), ln(np.where(xa > 0, xa, 1.0)), 0.0)
                apriori_col = col_dry_air * np.sum(pwf_z * np.where(pwf_valid, xa, 0.0), axis=1)
                apriori_term = apriori_col - np.sum(ak_z * ln_xa, axis=1)

            # a location is bad if the AK is nonzero where xa is unusable
            bad_ap = np.any(ak_valid & ~(xa > 0), axis=1)

            # --- QC / selection flags ---
            good = np.isfinite(col) & np.isfinite(col_error) & (col > 0.0) & (col_error > 0.0)
            good = good & np.isfinite(apriori_term) & ~bad_ap
            good = good & np.isfinite(lats) & np.isfinite(lons)
            if self.land_only:
                good = good & (land_flag == 1)
            if self.day_only:
                good = good & (day_flag == 1)
            if self.thin > 0.0:
                good = good & (np.random.uniform(size=nlocs) > self.thin)

            # --- prepare per-level output arrays ---
            preslev = np.where(np.isfinite(pressure), HPA2PA * pressure, FLOAT_FILL)
            xa_out = np.where(np.isfinite(xa), xa, FLOAT_FILL)

            # PreQC: 0 everywhere (no formal QA flag in the Summary product;
            # convergence/sensitivity screening is applied upstream by TROPESS)
            qcflg = np.zeros(nlocs, dtype=np.int32)

            # cast
            lats = lats.astype('float32')
            lons = lons.astype('float32')
            col = col.astype('float32')
            col_error = col_error.astype('float32')
            apriori_term = apriori_term.astype('float32')
            apriori_col = apriori_col.astype('float32')
            ak_z = ak_z.astype('float32')
            pwf_z = pwf_z.astype('float32')
            preslev = preslev.astype('float32')
            xa_out = xa_out.astype('float32')
            view_ang = np.where(np.isfinite(view_ang), view_ang, FLOAT_FILL).astype('float32')
            land_flag = land_flag.astype('int32')
            day_flag = day_flag.astype('int32')
            target_id = target_id.astype('int64')

            print('  nlocs in file: %d,  kept after QC/thinning: %d'
                  % (nlocs, int(good.sum())), flush=True)

            metadata_vars = [
                ('dateTime', times),
                ('latitude', lats),
                ('longitude', lons),
                ('sensorViewAngle', view_ang),
                ('landFlag', land_flag),
                ('daytimeFlag', day_flag),
                ('targetId', target_id),
            ]
            ancillary_vars = [
                ('averagingKernel', ak_z),
                ('pressureWeightingFunction', pwf_z),
                ('pressureLevel', preslev),
                ('aprioriProfile', xa_out),
                ('aprioriTerm', apriori_term),
                ('aprioriColumn', apriori_col),
            ]

            if first:
                for name, vals in metadata_vars:
                    self.outdata[(name, 'MetaData')] = vals[good]
                for name, vals in ancillary_vars:
                    self.outdata[(name, 'RtrvlAncData')] = vals[good]
                self.outdata[self.varDict[obsvar]['valKey']] = col[good]
                self.outdata[self.varDict[obsvar]['errKey']] = col_error[good]
                self.outdata[self.varDict[obsvar]['qcKey']] = qcflg[good]
            else:
                for name, vals in metadata_vars:
                    key = (name, 'MetaData')
                    self.outdata[key] = np.concatenate((self.outdata[key], vals[good]))
                for name, vals in ancillary_vars:
                    key = (name, 'RtrvlAncData')
                    self.outdata[key] = np.concatenate((self.outdata[key], vals[good]))
                for key in ['valKey', 'errKey', 'qcKey']:
                    okey = self.varDict[obsvar][key]
                    add = {'valKey': col, 'errKey': col_error, 'qcKey': qcflg}[key]
                    self.outdata[okey] = np.concatenate((self.outdata[okey], add[good]))

            first = False

        # dimensions
        self.DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])
        self.AttrData['Location'] = np.int32(self.DimDict['Location'])
        self.DimDict['Layer'] = nlevs
        self.AttrData['Layer'] = np.int32(nlevs)

        # variable attributes
        vkey = ('averagingKernel', 'RtrvlAncData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'mol m-2'
        self.varAttrs[vkey]['description'] = \
            'linearized column averaging kernel; applies to ln(VMR); ordered surface to TOA'

        vkey = ('pressureWeightingFunction', 'RtrvlAncData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = '1'
        self.varAttrs[vkey]['description'] = \
            'column pressure weighting function; ordered surface to TOA'

        vkey = ('pressureLevel', 'RtrvlAncData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'Pa'
        self.varAttrs[vkey]['description'] = \
            'retrieval pressure levels; ordered surface to TOA; fill below surface'

        vkey = ('aprioriProfile', 'RtrvlAncData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'mol/mol'

        vkey = ('aprioriTerm', 'RtrvlAncData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'mol m-2'
        self.varAttrs[vkey]['description'] = \
            'aprioriColumn - sum(averagingKernel * ln(aprioriProfile)); ' \
            'hofx = aprioriTerm + sum(averagingKernel * ln(model VMR))'

        vkey = ('aprioriColumn', 'RtrvlAncData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'mol m-2'

        self.varAttrs[('sensorViewAngle', 'MetaData')]['units'] = 'degree'
        self.varAttrs[('landFlag', 'MetaData')]['description'] = '1 = land, 0 = water'
        self.varAttrs[('daytimeFlag', 'MetaData')]['description'] = '1 = day, 0 = night'


def get_parser():
    parser = argparse.ArgumentParser(
        description=(
            'Reads TROPESS AIRS/CrIS L2 Summary NH3 netCDF files provided '
            'by NASA GES DISC and converts the total column retrieval into '
            'IODA formatted output files. Multiple files are able to be '
            'concatenated.'),
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.print_usage = parser.print_help

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of TROPESS L2 Summary NH3 observation netCDF input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--land-only',
        help="keep only observations over land (recommended for CrIS NH3; "
        "ocean retrievals can be unphysical per the product user guide)",
        action='store_true', default=False)
    optional.add_argument(
        '--day-only',
        help="keep only daytime observations",
        action='store_true', default=False)
    optional.add_argument(
        '-n', '--thin',
        help="percentage of random thinning from 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)

    return parser


def main():

    parser = get_parser()
    args = parser.parse_args()

    # read the NH3 data
    nh3 = tropess_nh3(args.input, args.land_only, args.day_only, args.thin)

    varDims = {
        obsvar: ['Location'],
        'averagingKernel': ['Location', 'Layer'],
        'pressureWeightingFunction': ['Location', 'Layer'],
        'pressureLevel': ['Location', 'Layer'],
        'aprioriProfile': ['Location', 'Layer'],
    }

    # setup the IODA writer and write everything out
    writer = iconv.IodaWriter(args.output, locationKeyList, nh3.DimDict)
    writer.BuildIoda(nh3.outdata, varDims, nh3.varAttrs, nh3.AttrData)


if __name__ == '__main__':
    main()
