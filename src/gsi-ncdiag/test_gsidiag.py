#!/usr/bin/env python
# script to run to test if the GSI ncdiag converters are still working
import radiances
import conv
import composition
import argparse

parser = argparse.ArgumentParser(
    description=('Test for GSI netCDF diag file to IODA Obs/GeoVaLs files converters'))
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
if (args.type == 'conv'):
    diag = conv.Conv(infile)
elif (args.type == 'rad'):
    diag = radiances.Radiances(infile)
elif (args.type == 'aod'):
    diag = composition.AOD(infile)
elif (args.type == 'oz'):
    diag = composition.Ozone(infile)
else:
    raise ValueError
diag.read()
diag.toIODAobs(outdir)
diag.toGeovals(outdir)
