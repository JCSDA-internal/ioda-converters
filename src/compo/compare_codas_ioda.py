#!/usr/bin/env python3

#
# compare_codas_ioda.py
#
# Compares variables between an xtralite CoDAS input file and the IODA output
# file produced by xtralite_codas_nc2ioda.py.
#
# Observations are matched by dateTime. The IODA file may have fewer obs than
# the CoDAS file (due to time-window and QC filtering), so only the intersection
# is compared.
#
# Usage:
#   python compare_codas_ioda.py \
#       -p tropomi_ch4 \
#       -c tropomi_ch4_20230115_03z.nc \
#       -i tropomi_ch4_ioda.nc
#

import argparse
import sys
from collections import namedtuple
from datetime import datetime, timedelta

import netCDF4 as nc
import numpy as np

# ─────────────────────────────────────────────────────────────────────────────
# Product config (mirrors xtralite_codas_nc2ioda.py)
# ─────────────────────────────────────────────────────────────────────────────
_ProductConfig = namedtuple('ProductConfig',
    ['ioda_varname', 'vert_coord', 'vert_ioda_name', 'has_priorpro', 'has_isbad'])

PRODUCT_CONFIG = {
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

_EPOCH = datetime(1970, 1, 1, 0, 0, 0)

# ─────────────────────────────────────────────────────────────────────────────
# Helpers
# ─────────────────────────────────────────────────────────────────────────────

def _codas_to_ioda_datetime(date_vals, time_vals):
    """Convert CoDAS date (YYYYMMDD) + time (hhmmss) to int64 seconds since 1970."""
    offsets = np.empty(len(date_vals), dtype=np.int64)
    for i, (d, t) in enumerate(zip(date_vals, time_vals)):
        ds = str(int(d))
        ts = str(int(t)).zfill(6)
        dt = datetime(int(ds[0:4]), int(ds[4:6]), int(ds[6:8]),
                      int(ts[0:2]), int(ts[2:4]), int(ts[4:6]))
        offsets[i] = int((dt - _EPOCH).total_seconds())
    return offsets


def _read_ioda_var(ioda_nc, group, varname):
    """Read a variable from an IODA NetCDF group, return None if absent."""
    if group not in ioda_nc.groups:
        return None
    grp = ioda_nc.groups[group]
    if varname not in grp.variables:
        return None
    return grp.variables[varname][:]


def _match_indices(codas_times, ioda_times):
    """
    Return (codas_idx, ioda_idx) integer arrays such that
    codas_times[codas_idx] == ioda_times[ioda_idx] for each matched pair.
    Uses dateTime (int64 seconds) as the match key.
    Handles duplicate datetimes by matching in encounter order.
    """
    from collections import defaultdict
    ioda_map = defaultdict(list)
    for j, t in enumerate(ioda_times):
        ioda_map[int(t)].append(j)

    codas_idx = []
    ioda_idx  = []
    used = defaultdict(int)   # how many times we've already consumed each key

    for i, t in enumerate(codas_times):
        key = int(t)
        slot = used[key]
        if slot < len(ioda_map[key]):
            codas_idx.append(i)
            ioda_idx.append(ioda_map[key][slot])
            used[key] += 1

    return np.array(codas_idx, dtype=int), np.array(ioda_idx, dtype=int)


# ─────────────────────────────────────────────────────────────────────────────
# Stats
# ─────────────────────────────────────────────────────────────────────────────

def _stats(codas_arr, ioda_arr):
    """Return a dict of comparison statistics between two flat arrays."""
    diff = np.abs(codas_arr.astype('float64') - ioda_arr.astype('float64'))
    denom = np.abs(codas_arr.astype('float64'))
    denom[denom == 0] = np.nan
    rel = diff / denom * 100.0

    return {
        'n':            len(codas_arr),
        'codas_min':    float(np.nanmin(codas_arr)),
        'codas_max':    float(np.nanmax(codas_arr)),
        'ioda_min':     float(np.nanmin(ioda_arr)),
        'ioda_max':     float(np.nanmax(ioda_arr)),
        'max_abs_diff': float(np.nanmax(diff)),
        'mean_abs_diff':float(np.nanmean(diff)),
        'max_rel_diff': float(np.nanmax(rel)),
    }


def _print_stats(label, s):
    print(f"\n  {label}")
    print(f"    N              : {s['n']}")
    print(f"    CoDAS range    : [{s['codas_min']:.6g}, {s['codas_max']:.6g}]")
    print(f"    IODA  range    : [{s['ioda_min']:.6g}, {s['ioda_max']:.6g}]")
    print(f"    Max |diff|     : {s['max_abs_diff']:.3e}")
    print(f"    Mean |diff|    : {s['mean_abs_diff']:.3e}")
    print(f"    Max rel diff % : {s['max_rel_diff']:.3e}")


# ─────────────────────────────────────────────────────────────────────────────
# Main comparison
# ─────────────────────────────────────────────────────────────────────────────

def compare(product, codas_file, ioda_file):
    if product not in PRODUCT_CONFIG:
        sys.exit(f"Unknown product '{product}'. Supported: {sorted(PRODUCT_CONFIG)}")

    cfg = PRODUCT_CONFIG[product]

    # ── Read CoDAS ────────────────────────────────────────────────────────────
    print(f"\nReading CoDAS : {codas_file}")
    cnc = nc.Dataset(codas_file, 'r')

    codas_date = cnc.variables['date'][:]
    codas_time = cnc.variables['time'][:]
    codas_lat  = cnc.variables['lat'][:]
    codas_lon  = cnc.variables['lon'][:]
    codas_obs  = cnc.variables['obs'][:]
    codas_unc  = cnc.variables['uncert'][:]
    codas_ak   = cnc.variables['avgker'][:]
    codas_vert = cnc.variables[cfg.vert_coord][:]

    codas_priorobs = cnc.variables['priorobs'][:] if cfg.has_priorpro else None
    codas_priorpro = cnc.variables['priorpro'][:] if cfg.has_priorpro else None

    cnc.close()

    codas_times = _codas_to_ioda_datetime(codas_date, codas_time)
    n_codas = len(codas_times)

    # ── Read IODA ─────────────────────────────────────────────────────────────
    print(f"Reading IODA  : {ioda_file}")
    inc = nc.Dataset(ioda_file, 'r')

    ioda_times = _read_ioda_var(inc, 'MetaData', 'dateTime')
    ioda_lat   = _read_ioda_var(inc, 'MetaData', 'latitude')
    ioda_lon   = _read_ioda_var(inc, 'MetaData', 'longitude')
    ioda_obs   = _read_ioda_var(inc, 'ObsValue', cfg.ioda_varname)
    ioda_unc   = _read_ioda_var(inc, 'ObsError', cfg.ioda_varname)
    ioda_ak    = _read_ioda_var(inc, 'RetrievalAncillaryData', 'averagingKernel')
    ioda_vert  = _read_ioda_var(inc, 'RetrievalAncillaryData', cfg.vert_ioda_name)

    ioda_priorobs = _read_ioda_var(inc, 'RetrievalAncillaryData', 'aprioriTerm')    if cfg.has_priorpro else None
    ioda_priorpro = _read_ioda_var(inc, 'RetrievalAncillaryData', 'aprioriProfile') if cfg.has_priorpro else None

    inc.close()

    if ioda_times is None:
        sys.exit("ERROR: MetaData/dateTime not found in IODA file.")

    n_ioda = len(ioda_times)

    # ── Match observations by dateTime ────────────────────────────────────────
    cidx, iidx = _match_indices(codas_times, ioda_times)
    n_matched = len(cidx)

    print(f"\nObservation counts")
    print(f"  CoDAS total : {n_codas}")
    print(f"  IODA total  : {n_ioda}")
    print(f"  Matched     : {n_matched}")

    if n_matched == 0:
        print("\nWARNING: No matching observations found. Check time range.")
        return

    unmatched_ioda = n_ioda - n_matched
    if unmatched_ioda > 0:
        print(f"  WARNING: {unmatched_ioda} IODA obs have no CoDAS match "
              f"(unexpected — possible time conversion issue)")

    # ── Compare variables ─────────────────────────────────────────────────────
    print("\n" + "─" * 60)
    print("Variable comparisons (matched observations only)")
    print("─" * 60)

    # dateTime
    _print_stats("dateTime (seconds since 1970-01-01)",
                 _stats(codas_times[cidx], ioda_times[iidx].astype('int64')))

    # lat / lon
    _print_stats("latitude",  _stats(codas_lat[cidx].astype('float64'),
                                      ioda_lat[iidx].astype('float64')))
    _print_stats("longitude", _stats(codas_lon[cidx].astype('float64'),
                                      ioda_lon[iidx].astype('float64')))

    # obs / uncert
    _print_stats(f"ObsValue / {cfg.ioda_varname}",
                 _stats(codas_obs[cidx].astype('float64'),
                        ioda_obs[iidx].astype('float64')))
    _print_stats(f"ObsError / {cfg.ioda_varname}",
                 _stats(codas_unc[cidx].astype('float64'),
                        ioda_unc[iidx].astype('float64')))

    # averaging kernel — compare mean over layer dimension
    if ioda_ak is not None:
        ak_c = codas_ak[cidx].astype('float64')
        ak_i = ioda_ak[iidx].astype('float64')
        _print_stats("averagingKernel (mean over layers)",
                     _stats(ak_c.mean(axis=1), ak_i.mean(axis=1)))

    # vertical coordinate — compare mean over edge dimension
    if ioda_vert is not None:
        vc_c = codas_vert[cidx].astype('float64')
        vc_i = ioda_vert[iidx].astype('float64')
        _print_stats(f"{cfg.vert_ioda_name} (mean over edges)",
                     _stats(vc_c.mean(axis=1), vc_i.mean(axis=1)))

    # a priori
    if cfg.has_priorpro:
        if ioda_priorobs is not None:
            _print_stats("aprioriTerm",
                         _stats(codas_priorobs[cidx].astype('float64'),
                                ioda_priorobs[iidx].astype('float64')))

        if ioda_priorpro is not None:
            pp_c = codas_priorpro[cidx].astype('float64')
            pp_i = ioda_priorpro[iidx].astype('float64')
            _print_stats("aprioriProfile (mean over layers)",
                         _stats(pp_c.mean(axis=1), pp_i.mean(axis=1)))

    print("\n" + "─" * 60)


# ─────────────────────────────────────────────────────────────────────────────
# Entry point
# ─────────────────────────────────────────────────────────────────────────────

def main():
    parser = argparse.ArgumentParser(
        description='Compare xtralite CoDAS input with IODA converter output.'
    )
    parser.add_argument('-p', '--product', required=True,
                        help=f'product key; one of: {sorted(PRODUCT_CONFIG)}')
    parser.add_argument('-c', '--codas',   required=True,
                        help='path to xtralite CoDAS NetCDF input file')
    parser.add_argument('-i', '--ioda',    required=True,
                        help='path to IODA NetCDF output file')
    args = parser.parse_args()

    compare(args.product, args.codas, args.ioda)


if __name__ == '__main__':
    main()
