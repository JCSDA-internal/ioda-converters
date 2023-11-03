#!/usr/bin/env python3

import argparse
import time
import sys
import os
import shutil
import h5py


def main(oman, ombg, output_file, output_dir='./'):
    """Convert IODA-converterd NOAA's operational GSI data files into feedback 
    files for comparison/use in SIMOBS skylab_monitor.

    Args:
        oman (.nc4 file) IODA-converted O-A (observation minus analysis) file from NOAA (_anl suffix)
        ombg (.nc4 file) IODA-converted O-B (observation minus background) file from NOAA (_ges suffix)  
        output_file (.nc4 file) representative feedback file of NOAA's operational model to use in skylab_monitor
        output_dir (directory) directory in which to store output_file
    """
    
    # start timer
    tic = record_time()

    print()
    print(f'List of files to process: {oman} {ombg}')
    print()

    # open first guess (background) departure
    f = h5py.File(ombg, 'r')
    # open analysis departure
    g = h5py.File(oman, 'r')
    # duplicate (destructive) the analysis departure
    shutil.copyfile(oman, output_file)
    # open output ioda in read write
    k = h5py.File(output_file, 'r+')

    # start appending data to output_file
    f.copy(f['GsiHofXBc'], k, 'hofx0')
    g.copy(g['GsiHofXBc'], k, 'hofx1')
    g.copy(g['GsiFinalObsError'], k, 'EffectiveError1')
    g.copy(g['GsiEffectiveQC'], k, 'EffectiveQC1')

    # this is specific now to radiances
    g.copy(g['GsiHofXBc'], k, 'oman')
    k['oman']['brightnessTemperature'][:, :] = k['ObsValue']['brightnessTemperature'][:, :] - k['hofx1']['brightnessTemperature'][:, :]

    f.copy(f['GsiHofXBc'], k, 'ombg')
    k['ombg']['brightnessTemperature'][:, :] = k['ObsValue']['brightnessTemperature'][:, :] - k['hofx0']['brightnessTemperature'][:, :]

    # ObsBias from analysis
    g.copy(g['GsiBc'], k, 'ObsBias1')
    k.copy(k['ObsBias1'], k, 'ObsBias0')

    f.close()
    g.close()
    k.close()

    # report time
    toc = record_time(tic=tic)


def record_time(tic=None, print_log=True):
    # ----------------------------------------------------------------------
    # Timing function
    #    call with no arguments to set tic:
    #       tic = record_time()
    #    call again passing tic to report time elapsed:
    #       toc = record_time(tic=tic)
    # ----------------------------------------------------------------------

    if not tic:
        tic = time.perf_counter()
        if print_log:
            print(f"  ... starting timer: {tic:0.3f}")
        return tic
    else:
        toc = time.perf_counter()
        if print_log:
            print(f"  ... elapsed time (sec): {toc - tic:0.3f}")
        return toc


if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description=(
            'Reads the ncDiag IODA files '
            ' convert to skylab like feedack IODA file')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument('--oman', type=str,
                          required=True, action='store',
                          help='ob minus analysis ioda file name')
    required.add_argument('--ombg', type=str,
                          required=True, action='store',
                          help='ob minus background ioda file name')
    required.add_argument('--output', type=str,
                          required=True, action='store',
                          help='output ioda filename')

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--output_dir', type=str,
                          dest='output_dir', default=os.getcwd(),
                          help='directory for output')

    args = parser.parse_args()

    main(args.oman, args.ombg, args.output, output_dir=args.output_dir)
