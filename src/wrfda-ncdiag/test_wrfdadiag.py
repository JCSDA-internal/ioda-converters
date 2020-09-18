#!/usr/bin/env python
# script to run to test if the WRFDA ncdiag converters are still working
import sys
import argparse
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import wrfda_ncdiag as wrfdad

parser = argparse.ArgumentParser(
    description=('Test for WRFDA netCDF diag file to IODA Obs files converters'))
parser.add_argument('-i', '--input',
                    help="name of input file",
                    type=str, required=True)
parser.add_argument('-o', '--output',
                    help="output directory",
                    type=str, required=True)
parser.add_argument('-t', '--type',
                    help="type of input file",
                    type=str, required=True)
args = parser.parse_args()

infile = args.input
outdir = args.output
if (args.type == 'rad'):
    diag = wrfdad.Radiances(infile)
else:
    raise ValueError
diag.read()
diag.toIODAobs(outdir)
