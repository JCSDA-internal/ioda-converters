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
    VarTypes = {}
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
    for fullvname in LocVarNames + AllVarNames:
        iodafile = VarAttrFiles[fullvname]
        obsspace = ios.ObsSpace(iodafile)
        _var = obsspace.Variable(fullvname)
        VarTypes[fullvname] = _var.numpy_dtype()
        for attr in _var.attrs:
            if attr not in ['DIMENSION_LIST']:
                gname, vname = fullvname.split('/')
                varAttrs[(vname, gname)][attr] = _var.read_attr(attr)
        # for now going to assume all are just 'nlocs' dim
        VarDims[fullvname] = ['nlocs']
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
                    tmpdata = tmpdata.astype('<U22')
                    tmpdata = [x.replace(' ', 'T') + 'Z' for x in tmpdata]
                    tmpdata = np.array(tmpdata).astype('<U22')
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
    DimDict['nlocs'] = len(idx)
    for idx2, fullvname in enumerate(LocVarNames):
        gname, vname = fullvname.split('/')
        OutData[(vname, gname)] = MetaVarUnique[idx2, ...].astype(VarTypes[fullvname])

    # grab the rest of the variables now
    DataVarData = []
    for fullvname in AllVarNames:
        gname, vname = fullvname.split('/')
        tmpvardata = []
        for f in FileList:
            obsspace = ios.ObsSpace(f)
            if fullvname in obsspace.variables:
                _var = obsspace.Variable(fullvname)
                tmpdata = np.array(_var.read_data())
            else:
                tmpdata = np.full((obsspace.nlocs), iconv.get_default_fill_val(VarTypes[fullvname]),
                                  dtype=VarTypes[fullvname])
            tmpvardata.append(tmpdata)
        tmpvardata = np.hstack(tmpvardata)
        DataVarData.append(tmpvardata)
    DataVarData = np.vstack(DataVarData)
    DataVarUnique = np.empty((len(DataVarData), len(idx)))
    for idx2, fullvname in enumerate(AllVarNames):
        gname, vname = fullvname.split('/')
        fillval = iconv.get_default_fill_val(VarTypes[fullvname])
        mask = ~(DataVarData[idx2, ...] == fillval)
        DataVarUnique[idx2, ...] = fillval
        DataVarUnique[idx2, inv[mask]] = DataVarData[idx2, mask]
    for idx2, fullvname in enumerate(AllVarNames):
        gname, vname = fullvname.split('/')
        OutData[(vname, gname)] = DataVarUnique[idx2, ...].astype(VarTypes[fullvname])

    writer = iconv.IodaWriter(OutFile, LocKeyList, DimDict)
    writer.BuildIoda(OutData, VarDims, varAttrs, globalAttrs)

    # now write out combined GeoVaLs file
    if GeoDir:
        GeoOutData = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        GeoLocKeyList = ['latitude', 'longitude', 'time']
        GeoFileList = []
        GeoVarNames2 = []
        GeoVarTypes = {}
        GeoVarDims = {}
        GeoVarFiles = {}
        GeoVarNames3 = []
        GeoVarNames31 = []
        for f in FileList:
            inob = f.split('/')[-1]
            ingeo = inob.replace('obs', 'geoval')
            g = GeoDir+'/'+ingeo
            GeoFileList.append(g)
        for f in GeoFileList:
            obsspace = ios.ObsSpace(f)
            nlevs = obsspace.Variable('nlevs').dimsizes[0]
            ninterfaces = obsspace.Variable('ninterfaces').dimsizes[0]
            for vname in obsspace.variables:
                if vname not in ['nlocs', 'nlevs', 'ninterfaces'] and 'maxstrlen' not in vname:
                    _var = obsspace.Variable(vname)
                    if len(_var.dimsizes) == 1 and vname not in GeoVarNames2:
                        GeoVarNames2.append(vname)
                        GeoVarFiles[vname] = f
                        GeoVarDims[vname] = ['nlocs']
                    elif len(_var.dimsizes) == 2:
                        if _var.dimsizes[1] == nlevs and vname not in GeoVarNames3:
                            GeoVarNames3.append(vname)
                            GeoVarFiles[vname] = f
                            GeoVarDims[vname] = ['nlocs', 'nlevs']
                        elif _var.dimsizes[1] == ninterfaces and vname not in GeoVarNames31:
                            GeoVarNames31.append(vname)
                            GeoVarFiles[vname] = f
                            GeoVarDims[vname] = ['nlocs', 'ninterfaces']
                    else:
                        pass
                    del _var
            del obsspace
        # figure out variable type and dims
        for vname in GeoVarNames2 + GeoVarNames3 + GeoVarNames31:
            iodafile = GeoVarFiles[vname]
            obsspace = ios.ObsSpace(iodafile)
            _var = obsspace.Variable(vname)
            GeoVarTypes[vname] = _var.numpy_dtype()
            del _var
            del obsspace

        # start to extract geovals from files
        GeoVarData2 = []
        GeoVarIdx2 = []
        GeoVarData3 = []
        GeoVarIdx3 = []
        GeoVarData31 = []
        GeoVarIdx31 = []

        # 2D fields
        for idx2, v in enumerate(GeoVarNames2):
            tmpgeodata = []
            tmpgeoidx = []
            for f in GeoFileList:
                obsspace = ios.ObsSpace(f)
                if v in obsspace.variables:
                    _var = obsspace.Variable(v)
                    tmpdata = np.array(_var.read_data())
                    del _var
                else:
                    tmpdata = np.full((obsspace.nlocs), iconv.get_default_fill_val(GeoVarTypes[v]),
                                      dtype=GeoVarTypes[v])
                tmpgeodata.append(tmpdata)
                tmpgeoidx.append(np.ones_like(tmpdata).astype(int)*int(idx2))
                del obsspace
            tmpgeodata = np.hstack(tmpgeodata)
            tmpgeoidx = np.hstack(tmpgeoidx)
            GeoVarData2.append(tmpgeodata)
            GeoVarIdx2.append(tmpgeoidx)
        GeoVarData2 = np.vstack(GeoVarData2)
        GeoVarIdx2 = np.vstack(GeoVarIdx2)
        GeoVarData2 = np.transpose(GeoVarData2)
        GeoVarIdx2 = np.transpose(GeoVarIdx2)
        GeoVarUnique2 = np.empty((len(idx), len(GeoVarData2[0])))

        # 3D fields on full levels
        for idx2, v in enumerate(GeoVarNames3):
            tmpgeodata = []
            tmpgeoidx = []
            for f in GeoFileList:
                obsspace = ios.ObsSpace(f)
                if v in obsspace.variables:
                    _var = obsspace.Variable(v)
                    tmpdata = np.array(_var.read_data())
                    del _var
                else:
                    tmpdata = np.full((obsspace.nlocs, nlevs), iconv.get_default_fill_val(GeoVarTypes[v]),
                                      dtype=GeoVarTypes[v])
                tmpgeodata.append(tmpdata)
                tmpgeoidx.append(np.ones_like(tmpdata).astype(int)*int(idx2))
                del obsspace
            tmpgeodata = np.vstack(tmpgeodata)
            tmpgeoidx = np.vstack(tmpgeoidx)
            GeoVarData3.append(tmpgeodata)
            GeoVarIdx3.append(tmpgeoidx)
        GeoVarData3 = np.dstack(GeoVarData3)
        GeoVarIdx3 = np.dstack(GeoVarIdx3)
        GeoVarUnique3 = np.empty((len(idx), len(GeoVarData3[0]), len(GeoVarData3[0, 0, :])))

        # 3D fields on half levels
        for idx2, v in enumerate(GeoVarNames31):
            tmpgeodata = []
            tmpgeoidx = []
            for f in GeoFileList:
                obsspace = ios.ObsSpace(f)
                if v in obsspace.variables:
                    _var = obsspace.Variable(v)
                    tmpdata = np.array(_var.read_data())
                    del _var
                else:
                    tmpdata = np.full((obsspace.nlocs, ninterfaces), iconv.get_default_fill_val(GeoVarTypes[v]),
                                      dtype=GeoVarTypes[v])
                tmpgeodata.append(tmpdata)
                tmpgeoidx.append(np.ones_like(tmpdata).astype(int)*int(idx2))
                del obsspace
            tmpgeodata = np.vstack(tmpgeodata)
            tmpgeoidx = np.vstack(tmpgeoidx)
            GeoVarData31.append(tmpgeodata)
            GeoVarIdx31.append(tmpgeoidx)
        GeoVarData31 = np.dstack(GeoVarData31)
        GeoVarIdx31 = np.dstack(GeoVarIdx31)
        GeoVarUnique31 = np.empty((len(idx), len(GeoVarData31[0]), len(GeoVarData31[0, 0, :])))

        # arrange the output data
        for ii, jj in np.ndindex(GeoVarData2.shape):
            j = GeoVarIdx2[ii, jj]
            i = inv[ii]
            if GeoVarData2[ii, jj] != iconv.get_default_fill_val('float32'):
                GeoVarUnique2[i, j] = GeoVarData2[ii, jj]
        for idx2, v in enumerate(GeoVarNames2):
            GeoOutData[v] = GeoVarUnique2[:, idx2].astype(GeoVarTypes[v])
        for ii, kk, jj in np.ndindex(GeoVarData3.shape):
            j = GeoVarIdx3[ii, kk, jj]
            i = inv[ii]
            if GeoVarData3[ii, kk, jj] != iconv.get_default_fill_val('float32'):
                GeoVarUnique3[i, kk, j] = GeoVarData3[ii, kk, jj]
        for idx2, v in enumerate(GeoVarNames3):
            GeoOutData[v] = GeoVarUnique3[..., idx2].astype(GeoVarTypes[v])
        for ii, kk, jj in np.ndindex(GeoVarData31.shape):
            j = GeoVarIdx31[ii, kk, jj]
            i = inv[ii]
            if GeoVarData31[ii, kk, jj] != iconv.get_default_fill_val('float32'):
                GeoVarUnique31[i, kk, j] = GeoVarData31[ii, kk, jj]
        for idx2, v in enumerate(GeoVarNames31):
            GeoOutData[v] = GeoVarUnique31[..., idx2].astype(GeoVarTypes[v])

        # write out the file
        GeoDimDict = {}
        GeoGlobalAttrs = {}
        GeoDimDict['nlocs'] = len(GeoVarUnique2)
        GeoDimDict['nlevs'] = nlevs
        GeoDimDict['ninterfaces'] = ninterfaces
        OutGeoFile = OutFile.replace('obs', 'geoval')
        GeoGlobalAttrs['input_files'] = ';'.join(GeoFileList)
        gwriter = iconv.IodaWriter(OutGeoFile, GeoLocKeyList, GeoDimDict)
        gwriter.BuildIoda(GeoOutData, GeoVarDims, {}, GeoGlobalAttrs, geovals=True)


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
