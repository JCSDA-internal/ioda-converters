#!/usr/bin/env python3
# script to run to test if the GSI ncdiag converters are still working
import argparse

from pyiodaconv import gsi_ncdiag as gsid

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
parser.add_argument("--add_total_bias", default=False, action='store_true',
                    help="Add bias generated from adjusted minus unadjusted to output observations")
parser.add_argument('-b', '--add_obsbias', default=False, action='store_true',
                    help="add obsbias groups, only works on rad")
parser.add_argument('-q', '--add_qcvars', default=False, action='store_true',
                    help="add qc variables, only works on rad")
parser.add_argument('-r', '--add_testrefs', default=False, action='store_true',
                    help="add test variables, only works on rad")
args = parser.parse_args()

infile = args.input
outdir = args.output
obsbias = args.add_obsbias
qcvars = args.add_qcvars
testrefs = args.add_testrefs
TotalBias = args.add_total_bias
print(f' ... add_total_bias: {TotalBias}')
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
    diag.toIODAobs(outdir, platforms=[args.platform], TotalBias=TotalBias)
elif (args.type == 'rad'):
    diag.toIODAobs(outdir, obsbias, qcvars, testrefs, TotalBias=TotalBias)
else:
    diag.toIODAobs(outdir)
if args.geovals:
    diag.toGeovals(outdir)
