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

# these are the variables that can be used to match up locations
loc_vars = [
    'MetaData/latitude',
    'MetaData/longitude',
    'MetaData/station_elevation',
    'MetaData/air_pressure',
    'MetaData/station_id',
    'MetaData/height',
    'MetaData/datetime',
]

def combine_obsspace(FileList, OutFile, GeoDir):
    # get lists of all variables
    AllVarNames = []
    LocVarNames = []
    AllVarNames.append('nlocs')
    VarAttrFiles = {}
    for f in FileList:
        obsspace = ios.ObsSpace(f)
        for vname in obsspace.variables:
            if vname in loc_vars:
                if vname not in LocVarNames:
                    LocVarNames.append(vname)
                    VarAttrFiles[vname] = f
            else:
                if vname not in AllVarNames:
                    AllVarNames.append(vname)
                    VarAttrFiles[vname] = f
        del obsspace
    AllVarNames.remove('nlocs')
    # output variables
    LocKeyList = []
    DimDict = {}
    OutData = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    VarDims = {}
    globalAttrs = {}
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    # global attributes will come from the first file only in case of conflicts
    obsspace = ios.ObsSpace(FileList[0])
    for attr in obsspace.attrs:
        if attr not in ['_ioda_layout']:
            globalAttrs[attr] = obsspace.read_attr(attr)
    # add a new global attribute
    globalAttrs['input_files'] = ';'.join(FileList)

    # variable attributes will come from the first file that had said variable
    for vname in LocVarNames + AllVarNames:
        iodafile = VarAttrFiles[vname]
        obsspace = ios.ObsSpace(iodafile)
        _var = obsspace.Variable(vname)
        for attr in _var.attrs:
            if attr not in ['DIMENSION_LIST']:
                varAttrs[vname][attr] = _var.read_attr(attr)
        del _var
        del obsspace

    # figure out which variables should be used to match locations
    LocInAll = {}
    for vname in LocVarNames:
        LocInAll[vname] = True
        for f in FileList:
            obsspace = ios.ObsSpace(f)
            if vname not in obsspace.variables:
                LocInAll[vname] = False
            del obsspace

    # extract location specific variables and generate a numpy array
    MetaVarData = []
    bad_idxs = []
    for vname in LocVarNames:
        tmpvardata = []
        if LocInAll[vname]:
            for f in FileList:
                obsspace = ios.ObsSpace(f)
                _var = obsspace.Variable(vname)
                tmpdata = np.array(_var.read_data())
                if vname == 'MetaData/datetime':
                    tmpdata = tmpdata.astype("<U22")
                tmpvardata.append(tmpdata)
                del _var
                del obsspace
        if len(tmpvardata):
            tmpvardata = np.hstack(tmpvardata)
            MetaVarData.append(tmpvardata)
        else:
            bad_idxs.append(LocVarNames.index(vname))
    MetaVarData = np.vstack(MetaVarData)
    MetaVarUnique, idx, inv, cnt = np.unique(MetaVarData, return_index=True,
                                             return_inverse=True, return_counts=True, axis=1)

    # grab the rest of the variables now

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
