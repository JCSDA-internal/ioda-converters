#!/usr/bin/python

"""
Python code to ingest observational data and try to convert to IODA
"""

import argparse
from datetime import datetime, timedelta
import glob
import os
import sys

from gnssro_bufr2ioda import main as main_gnssro
from tropics_2ioda import main as main_tropics
from amv_ssec_ascii2ioda import main as main_amv_ssec

def main(args):

    # output_filename = args.output
    # dtg = datetime.strptime(args.date, '%Y%m%d%H')

    input_files = [(i) for i in args.input]
    if 'bfrPrf' in input_files[0] and 'bfrPrf' in input_files[-1]:
        main_gnssro(args)
    elif 'TROPICS' in input_files[0] and 'TROPICS' in input_files[-1]:
        main_tropics(args)
    elif 'AllWindsQIText' in input_files[0] and 'AllWindsQIText' in input_files[-1]:
        main_amv_ssec(args)
    else:
        print(f"  the argument provided is not a known data type")
        print(f"  known types are bfrPrf, TROPICS, AllWindsQIText")
        raise ValueError(f"   ERROR in generic_obs2ioda.py")


if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description=(
            'Reads the observation data '
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument('-i', '--input',
        type=str, nargs='+', required=True,
        help="path of satellite observation input file(s)")
    required.add_argument('-d', '--date',
        metavar="YYYYMMDDHH",
        type=str, required=True,
        help="base date for the center of the window")

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--debug', action='store_true',
        help='enable debug messages')
    optional.add_argument('-v', '--verbose', action='store_true',
        help='enable verbose debug messages')
    optional.add_argument('-j', '--threads',
        type=int, default=1,
        help='multiple threads can be used to load input files in parallel.'
             ' (default: %(default)s)')
    optional.add_argument('-o', '--output',
        type=str, default=os.path.join(os.getcwd(), 'output.nc4'),
        help='path to output ioda file')
    optional.add_argument('-r', '--recordnumber',
        type=int, default=1,
        help=' optional record number to associate with profile ')
    optional.add_argument('-q', '--qualitycontrol',
        default=False, action='store_true', required=False,
        help='turn on quality control georeality checks')

    args = parser.parse_args()

    main(args)
