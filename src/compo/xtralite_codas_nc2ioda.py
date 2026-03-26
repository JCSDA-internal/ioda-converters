#!/usr/bin/env python3

#
# (C) Copyright 2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0
#
# Generic converter from xtralite CoDAS NetCDF output to IODA format.
#
# All supported products share the same CoDAS variable layout after xtralite
# translation:
#   lat, lon           (nsound)          - location
#   date               (nsound)          - YYYYMMDD int32
#   time               (nsound)          - hhmmss int32
#   obs                (nsound)          - retrieved column
#   uncert             (nsound)          - observation uncertainty
#   avgker             (nsound, navg)    - column averaging kernel
#   priorobs           (nsound)          - a priori total column
#   priorpro           (nsound, navg)    - a priori profile     [optional]
#   isbad              (nsound)          - QC flag, 0=good      [optional]
#   zeavg  or  peavg   (nsound, nedge)   - vertical edge coords [product-specific]
#
# Supported products (-p / --product):
#   tropomi_ch4   tropomi_co   tropomi_hcho   tropomi_so2   tropomi_no2   tropomi_o3
#   iasi_co       iasi_ch4
#   mopitt_co
#   tropess_co
#
# Usage:
#   python xtralite_codas_nc2ioda.py \
#       -p tropomi_ch4 \
#       -i tropomi_ch4_20230115_03z.nc [file2.nc ...] \
#       -o tropomi_ch4_ioda.nc \
#       -r 2023011500 2023011506
#

import argparse
import netCDF4 as nc
import numpy as np
import os
from collections import defaultdict, namedtuple, OrderedDict
from datetime import datetime, timedelta

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

# ─────────────────────────────────────────────────────────────────────────────
# Product configuration table
#
# Fields:
#   ioda_varname   - variable name used in ObsValue/ObsError/PreQC groups
#   vert_coord     - CoDAS vertical edge variable ('zeavg' or 'peavg')
#   vert_ioda_name - IODA name for the vertical edge variable
#   has_priorpro   - whether priorpro (nsound, navg) is present in the file
#   has_isbad      - whether isbad quality flag is present (0 = good)
# ─────────────────────────────────────────────────────────────────────────────
_ProductConfig = namedtuple('ProductConfig',
    ['ioda_varname', 'vert_coord', 'vert_ioda_name', 'has_priorpro', 'has_isbad'])

PRODUCT_CONFIG = {
    #                   ioda_varname              vert_coord  vert_ioda_name      has_priorpro  has_isbad
    'tropomi_ch4':  _ProductConfig('methaneTotal',          'zeavg', 'altitudeVertice',  True,  False),
    'tropomi_co':   _ProductConfig('carbonmonoxideTotal',   'zeavg', 'altitudeVertice',  False, False),
    'tropomi_hcho': _ProductConfig('formaldehydeTotal',     'peavg', 'pressureVertice',  True,  False),
    'tropomi_so2':  _ProductConfig('sulfurdioxideTotal',    'peavg', 'pressureVertice',  True,  False),
    'tropomi_no2':  _ProductConfig('nitrogenDioxideTotal',  'peavg', 'pressureVertice',  False, False),
    'tropomi_o3':   _ProductConfig('ozoneTotal',            'peavg', 'pressureVertice',  True,  False),
    'iasi_co':      _ProductConfig('carbonmonoxideTotal',   'zeavg', 'altitudeVertice',  True,  True),
    'iasi_ch4':     _ProductConfig('methaneTotal',          'peavg', 'pressureVertice',  False, True),
    'mopitt_co':    _ProductConfig('carbonmonoxideTotal',   'peavg', 'pressureVertice',  True,  True),
    'tropess_co':   _ProductConfig('carbonmonoxideTotal',   'peavg', 'pressureVertice',  True,  False),
}

# Sensor/platform metadata keyed on the instrument prefix of the product name
_SENSOR_PLATFORM = {
    'tropomi': ('TROPOMI', 'Sentinel-5P'),
    'iasi':    ('IASI',    'MetOp'),
    'mopitt':  ('MOPITT',  'Terra'),
    'tropess': ('TROPESS', 'CrIS'),
}

# Required by IodaWriter
locationKeyList = [
    ("latitude",  "float"),
    ("longitude", "float"),
    ("dateTime",  "long"),
]

# IODA epoch
_EPOCH = datetime(1993, 1, 1, 0, 0, 0)

# Module-level dicts populated during _read() (required by IodaWriter)
DimDict  = {}
AttrData = {'converter': os.path.basename(__file__)}


# ─────────────────────────────────────────────────────────────────────────────
# Helpers
# ─────────────────────────────────────────────────────────────────────────────

def _to_ioda_datetime(date_vals, time_vals):
    """Convert CoDAS date (YYYYMMDD) + time (hhmmss) arrays to int64 seconds
    since 1993-01-01T00:00:00Z."""
    offsets = np.empty(len(date_vals), dtype=np.int64)
    for i, (d, t) in enumerate(zip(date_vals, time_vals)):
        ds = str(int(d))
        ts = str(int(t)).zfill(6)
        dt = datetime(int(ds[0:4]), int(ds[4:6]), int(ds[6:8]),
                      int(ts[0:2]), int(ts[2:4]), int(ts[4:6]))
        offsets[i] = int((dt - _EPOCH).total_seconds())
    return offsets


def _time_filter(ioda_times, date_start, date_end):
    """Return boolean mask selecting observations within [date_start, date_end)."""
    dts = [_EPOCH + timedelta(seconds=int(s)) for s in ioda_times]
    return np.array([(d >= date_start) and (d < date_end) for d in dts])


# ─────────────────────────────────────────────────────────────────────────────
# Converter class
# ─────────────────────────────────────────────────────────────────────────────

class CoDASConverter(object):

    def __init__(self, product, filenames, time_range):
        if product not in PRODUCT_CONFIG:
            raise ValueError(
                f"Unknown product '{product}'. "
                f"Supported: {sorted(PRODUCT_CONFIG)}")

        self.cfg       = PRODUCT_CONFIG[product]
        self.filenames = filenames
        self.date_start = datetime.strptime(time_range[0], "%Y%m%d%H")
        self.date_end   = datetime.strptime(time_range[1], "%Y%m%d%H")

        self.varDict  = defaultdict(lambda: defaultdict(dict))
        self.outdata  = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        # Set instrument metadata in AttrData
        prefix = product.split('_')[0]
        sensor, platform = _SENSOR_PLATFORM.get(prefix, (product, 'unknown'))
        AttrData['sensor']   = sensor
        AttrData['platform'] = platform
        AttrData['nvars']    = np.int32(1)

        self._read()

    # ── variable key setup ──────────────────────────────────────────────────

    def _setup_var_keys(self, units):
        iodavar = self.cfg.ioda_varname
        cfg = self.cfg

        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey']  = iodavar, iconv.OqcName()

        for grp in [iconv.OvalName(), iconv.OerrName()]:
            self.varAttrs[iodavar, grp]['units']       = units
            self.varAttrs[iodavar, grp]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OqcName()]['units']       = 'unitless'
        self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'

        # Averaging kernel
        ak_key = ('averagingKernel', 'RetrievalAncillaryData')
        self.varAttrs[ak_key]['units']       = '1'
        self.varAttrs[ak_key]['coordinates'] = 'longitude latitude'

        # A priori total column and profile (only present when priorpro exists;
        # _generic_end in tropomi.py creates priorobs only if priorpro is present)
        if cfg.has_priorpro:
            ap_key = ('aprioriTerm', 'RetrievalAncillaryData')
            self.varAttrs[ap_key]['units']       = units
            self.varAttrs[ap_key]['coordinates'] = 'longitude latitude'

        if cfg.has_priorpro:
            pp_key = ('aprioriProfile', 'RetrievalAncillaryData')
            self.varAttrs[pp_key]['units']       = units
            self.varAttrs[pp_key]['coordinates'] = 'longitude latitude'

        # Vertical edge coordinate
        vc_key = (cfg.vert_ioda_name, 'RetrievalAncillaryData')
        vc_units = 'm' if cfg.vert_coord == 'zeavg' else 'hPa'
        self.varAttrs[vc_key]['units']       = vc_units
        self.varAttrs[vc_key]['coordinates'] = 'longitude latitude'

    # ── read all input files ─────────────────────────────────────────────────

    def _read(self):
        cfg     = self.cfg
        iodavar = cfg.ioda_varname
        first   = True
        nlay    = None

        for f in self.filenames:
            ncd = nc.Dataset(f, 'r')

            # Read core variables
            lats      = ncd.variables['lat'][:]
            lons      = ncd.variables['lon'][:]
            date_vals = ncd.variables['date'][:]
            time_vals = ncd.variables['time'][:]
            obs       = ncd.variables['obs'][:]
            uncert    = ncd.variables['uncert'][:]
            avgker    = ncd.variables['avgker'][:]
            vert      = ncd.variables[cfg.vert_coord][:]

            # Grab units from the file the first time
            if first:
                units = getattr(ncd.variables['obs'], 'units', 'unknown')
                self._setup_var_keys(units)

            # Optional variables (priorobs only exists when priorpro does)
            priorobs = ncd.variables['priorobs'][:] if cfg.has_priorpro else None
            priorpro = ncd.variables['priorpro'][:] if cfg.has_priorpro else None
            isbad    = ncd.variables['isbad'][:]     if cfg.has_isbad    else None

            ncd.close()

            nlay = avgker.shape[1]

            # Convert date/time to IODA epoch seconds
            ioda_times = _to_ioda_datetime(date_vals, time_vals)

            # Build selection mask: time window + quality filter
            mask = _time_filter(ioda_times, self.date_start, self.date_end)
            if isbad is not None:
                mask = mask & (np.array(isbad) == 0)

            # Slice and cast to single precision
            def f32(arr):     return np.array(arr[mask], dtype='float32')
            def i32(arr):     return np.array(arr[mask], dtype='int32')

            lats_s     = f32(lats)
            lons_s     = f32(lons)
            times_s    = ioda_times[mask]           # keep int64
            obs_s      = f32(obs)
            uncert_s   = f32(uncert)
            avgker_s   = f32(avgker)
            vert_s     = f32(vert)
            priorobs_s = f32(priorobs) if priorobs is not None else None
            priorpro_s = f32(priorpro) if priorpro is not None else None

            # PreQC: 0=passed explicit QC (isbad filtered), 2=unchecked (no isbad)
            preqc_val = 0 if cfg.has_isbad else 2
            preqc_s   = np.full(obs_s.shape, preqc_val, dtype='int32')

            if first:
                self.outdata[('latitude',  'MetaData')] = lats_s
                self.outdata[('longitude', 'MetaData')] = lons_s
                self.outdata[('dateTime',  'MetaData')] = times_s

                self.outdata[self.varDict[iodavar]['valKey']] = obs_s
                self.outdata[self.varDict[iodavar]['errKey']] = uncert_s
                self.outdata[self.varDict[iodavar]['qcKey']]  = preqc_s

                self.outdata[('averagingKernel', 'RetrievalAncillaryData')] = avgker_s
                self.outdata[(cfg.vert_ioda_name,'RetrievalAncillaryData')] = vert_s

                if priorobs_s is not None:
                    self.outdata[('aprioriTerm',    'RetrievalAncillaryData')] = priorobs_s
                if priorpro_s is not None:
                    self.outdata[('aprioriProfile', 'RetrievalAncillaryData')] = priorpro_s

            else:
                def cat(key, arr):
                    self.outdata[key] = np.concatenate((self.outdata[key], arr))

                cat(('latitude',  'MetaData'), lats_s)
                cat(('longitude', 'MetaData'), lons_s)
                cat(('dateTime',  'MetaData'), times_s)

                cat(self.varDict[iodavar]['valKey'], obs_s)
                cat(self.varDict[iodavar]['errKey'], uncert_s)
                cat(self.varDict[iodavar]['qcKey'],  preqc_s)

                cat(('averagingKernel', 'RetrievalAncillaryData'), avgker_s)
                cat((cfg.vert_ioda_name,'RetrievalAncillaryData'), vert_s)

                if priorobs_s is not None:
                    cat(('aprioriTerm',    'RetrievalAncillaryData'), priorobs_s)
                if priorpro_s is not None:
                    cat(('aprioriProfile', 'RetrievalAncillaryData'), priorpro_s)

            first = False

        nlocs = len(self.outdata[('dateTime', 'MetaData')])
        DimDict['Location'] = nlocs
        DimDict['Layer']    = nlay
        DimDict['Vertice']  = nlay + 1

        AttrData['Location'] = np.int32(nlocs)
        AttrData['Layer']    = np.int32(nlay)
        AttrData['Vertice']  = np.int32(nlay + 1)
        AttrData['date_time_string'] = datetime.strftime(
            _EPOCH + timedelta(seconds=int(self.outdata[('dateTime', 'MetaData')][0])),
            "%Y-%m-%dT%H:%M:%SZ")


# ─────────────────────────────────────────────────────────────────────────────
# Entry point
# ─────────────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description=(
            'Reads xtralite CoDAS NetCDF output files and converts to IODA format. '
            'Multiple input files are concatenated before writing.'
        )
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-p', '--product',
        help=f'product key; one of: {sorted(PRODUCT_CONFIG)}',
        type=str, required=True)
    required.add_argument(
        '-i', '--input',
        help='path(s) to xtralite CoDAS NetCDF file(s)',
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help='path of IODA output file',
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-r', '--time_range',
        help='retain only observations within [begindate, enddate); '
             'format: YYYYMMDDHH YYYYMMDDHH',
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('1970010100', '2170010100'))

    args = parser.parse_args()

    converter = CoDASConverter(args.product, args.input, args.time_range)

    # Build VarDims dynamically based on what's in the output
    cfg = PRODUCT_CONFIG[args.product]
    VarDims = {
        cfg.ioda_varname:   ['Location'],
        'averagingKernel':  ['Location', 'Layer'],
        cfg.vert_ioda_name: ['Location', 'Vertice'],
    }
    if cfg.has_priorpro:
        VarDims['aprioriTerm']    = ['Location']
        VarDims['aprioriProfile'] = ['Location', 'Layer']

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(converter.outdata, VarDims, converter.varAttrs, AttrData)


if __name__ == '__main__':
    main()
