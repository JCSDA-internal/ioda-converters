#!/usr/bin/env python3

#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

"""
Physical range checks of woudc_ozonesonde2ioda.py output.

Runs the converter on the form-1 and form-2 test files and checks, for every
profile, that:

- the median ozone in the lowest 2 km is between 10 and 150 ppbv;
- the ozone maximum is between 4 and 12 ppmv and lies between 30 and 5 hPa
  (a maximum outside this range points to a unit error);
- pressure is in Pa, no level is above 5 hPa and errors are positive;
- no #PRELAUNCH or #DESELECTED_DATA row was ingested.

Also checks that a window with no launch exits cleanly without an output file.

Usage: woudc_ozonesonde_range_check.py <converter> <testinput dir> <output dir>
"""

import os
import subprocess
import sys

import netCDF4
import numpy as np

INPUTS = [
    'woudc_ozonesonde_stn076_20250101_form1.csv',
    'woudc_ozonesonde_stn316_20251230_form2.csv',
    'woudc_ozonesonde_stn174_20260422_form2.csv',
]

LOW_LAYER_M = 2000.0
LOW_MEDIAN_PPBV = (10.0, 150.0)
PEAK_PPMV = (4.0, 12.0)
PEAK_PRESSURE_HPA = (5.0, 30.0)


def count_profile_rows(filename):
    """Return the number of data rows of the #PROFILE table of an extCSV file."""
    rows, in_profile, header = 0, False, False
    with open(filename) as fh:
        for line in fh:
            line = line.strip()
            if line.startswith('#'):
                in_profile, header = line.upper() == '#PROFILE', False
            elif in_profile and line and not line.startswith('*'):
                if header:
                    rows += 1
                header = True
    return rows


def run(cmd):
    """Run a command, echo it, and return its exit code."""
    print(' '.join(cmd), flush=True)
    return subprocess.run(cmd).returncode


def check_profiles(output, inputs):
    """Check each profile of the IODA file; return a list of error messages."""
    errors = []
    with netCDF4.Dataset(output) as nc:
        ozone = nc['ObsValue/ozoneInsitu'][:].filled(np.nan)
        error = nc['ObsError/ozoneInsitu'][:].filled(np.nan)
        pressure = nc['MetaData/pressure'][:].filled(np.nan)
        height = nc['MetaData/height'][:].filled(np.nan)
        station = np.asarray(nc['MetaData/stationIdentification'][:])
        release = nc['MetaData/releaseTime'][:]

    profiles = sorted(set(zip(station, release)))
    if len(profiles) != len(inputs):
        errors.append(f'{len(profiles)} profiles in output, expected {len(inputs)}')
    if not np.all(np.isfinite(error) & (error > 0)):
        errors.append('non-positive or missing ObsError')
    if np.nanmax(pressure) < 2.0e4 or np.nanmin(pressure) < 500.0:
        errors.append('pressure not in Pa or levels above 5 hPa')

    max_rows = sum(count_profile_rows(f) for f in inputs)
    if ozone.size > max_rows:
        errors.append(f'{ozone.size} locations but only {max_rows} #PROFILE rows in the inputs')

    for stn, rel in profiles:
        sel = (station == stn) & (release == rel)
        x, p, h = ozone[sel], pressure[sel] / 100.0, height[sel]
        low = h - np.nanmin(h) < LOW_LAYER_M
        median_ppbv = np.median(x[low]) * 1e9
        ipeak = np.nanargmax(x)
        peak_ppmv, peak_hpa = x[ipeak] * 1e6, p[ipeak]
        print(f'{stn} {rel}: {sel.sum()} levels, lowest 2 km median {median_ppbv:.1f} ppbv, '
              f'peak {peak_ppmv:.2f} ppmv at {peak_hpa:.1f} hPa')
        if not LOW_MEDIAN_PPBV[0] <= median_ppbv <= LOW_MEDIAN_PPBV[1]:
            errors.append(f'{stn}: lowest 2 km median {median_ppbv:.1f} ppbv out of {LOW_MEDIAN_PPBV}')
        if not PEAK_PPMV[0] <= peak_ppmv <= PEAK_PPMV[1]:
            errors.append(f'{stn}: ozone peak {peak_ppmv:.2f} ppmv out of {PEAK_PPMV} (unit error?)')
        if not PEAK_PRESSURE_HPA[0] <= peak_hpa <= PEAK_PRESSURE_HPA[1]:
            errors.append(f'{stn}: ozone peak at {peak_hpa:.1f} hPa, outside {PEAK_PRESSURE_HPA}')
    return errors


def main():
    converter, indir, outdir = sys.argv[1:4]
    os.makedirs(outdir, exist_ok=True)
    inputs = [os.path.join(indir, f) for f in INPUTS]
    errors = []

    output = os.path.join(outdir, 'woudc_ozonesonde_range_check.nc')
    if run([sys.executable, converter, '-i', *inputs, '-o', output]) != 0:
        errors.append('converter failed')
    else:
        errors += check_profiles(output, inputs)

    # A window without any launch: clean exit and no output file.
    empty = os.path.join(outdir, 'woudc_ozonesonde_empty_window.nc')
    if os.path.exists(empty):
        os.remove(empty)
    code = run([sys.executable, converter, '-i', *inputs, '-o', empty,
                '--window-begin', '2025-06-01T03:00:00Z', '--window-end', '2025-06-01T09:00:00Z'])
    if code != 0 or os.path.exists(empty):
        errors.append(f'empty window: exit code {code}, output written: {os.path.exists(empty)}')

    for e in errors:
        print(f'FAILED: {e}')
    sys.exit(1 if errors else 0)


if __name__ == '__main__':
    main()
