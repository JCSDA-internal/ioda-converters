#!/usr/bin/env python3
# combine IODA ObsSpaces together into one ObsSpace to write to a file

import sys
import netCDF4 as nc
import numpy as np
import argparse
import ioda_obs_space as ios
from collections import defaultdict, OrderedDict
import datetime as dt
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

def combine_obsspace(FileList, OutFile, GeoDir):
    # get lists of all variables
    AllVarNames = []
    AllVarNames.append('nlocs')
    VarAttrFiles = {}
    for f in FileList:
        obsspace = ios.ObsSpace(f)
        for vname in obsspace.variables:
            if vname not in AllVarNames:
                AllVarNames.append(vname)
                VarAttrFiles[vname] = f
        del obsspace
    AllVarNames.remove('nlocs')
    print(AllVarNames)
    # output variables
    LocKeyList = []
    DimDict = {}
    OutData = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    VarDims = {}
    globalAttrs = {}
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    writer = iconv.IodaWriter(OutFile, LocKeyList, DimDict)
    writer.BuildIoda(OutData, VarDims, varAttrs, globalAttrs)

######################################################
######################################################
if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Combine multiple files in IODA format into one output file',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='list of the input files to combine',
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output', help='name of the output IODA observation file',
        type=str, required=True, default=None)
    parser.add_argument('-g', '--geovals', help='path to where matching geovals should be')

    args = parser.parse_args()

    FileList = args.input
    OutFile = args.output

    GeoDir = False
    if args.geovals:
        GeoDir = args.geovals

    combine_obsspace(FileList, OutFile, GeoDir)
