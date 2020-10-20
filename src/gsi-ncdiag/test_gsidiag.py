#!/usr/bin/env python
# script to run to test if the GSI ncdiag converters are still working
import sys
import argparse
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import gsi_ncdiag as gsid

parser = argparse.ArgumentParser(
    description=('Test for GSI netCDF diag file to IODA Obs/GeoVaLs files converters'))
parser.add_argument('-i', '--input',
                    help="name of input file",
                    type=str, required=True)
parser.add_argument('-o', '--output',
                    help="output directory",
                    type=str, required=True)
parser.add_argument('-g', '--geovals',
                    help="geovals output directory",
                    type=str, required=False)
parser.add_argument('-t', '--type',
                    help="type of input file",
                    type=str, required=True)
parser.add_argument('-p', '--platform',
                    help="platform to run, only works on conv",
                    type=str)
parser.add_argument('-b', '--add_obsbias',
                    help="add obsbias groups, only works on rad",
                    type=str)
parser.add_argument('-q', '--add_qcvars',
                    help="add qc variables, only works on rad",
                    type=str)
parser.add_argument('-r', '--add_testrefs',
                    help="add test variables, only works on rad",
                    type=str)
args = parser.parse_args()

infile = args.input
outdir = args.output
obsbias = args.add_obsbias
qcvars = args.add_qcvars
testrefs = args.add_testrefs
if (args.type == 'conv'):
    diag = gsid.Conv(infile)
elif (args.type == 'rad'):
    diag = gsid.Radiances(infile)
elif (args.type == 'aod'):
    diag = gsid.AOD(infile)
elif (args.type == 'oz'):
    diag = gsid.Ozone(infile)
elif (args.type == 'radar'):
    diag = gsid.Radar(infile)
else:
    raise ValueError
diag.read()
if (args.platform and args.type == 'conv'):
    diag.toIODAobs(outdir, platforms=[args.platform])
elif (args.type == 'rad'):
    diag.toIODAobs(outdir, obsbias, qcvars, testrefs)
else:
    diag.toIODAobs(outdir)
if args.geovals:
    diag.toGeovals(outdir)
