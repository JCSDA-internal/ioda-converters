#!/usr/bin/env python3
# combine IODA ObsSpaces together into one ObsSpace to write to a file

import netCDF4 as nc
import numpy as np
import argparse
import ioda_obs_space as ios
from collections import defaultdict, OrderedDict

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

# these are the variables that can be used to match up locations
loc_vars = [
    'MetaData/latitude',
    'MetaData/longitude',
    'MetaData/stationElevation',
    'MetaData/pressure',
    'MetaData/stationIdentification',
    'MetaData/height',
    'MetaData/datetime',
    'MetaData/dateTime',
]


def combine_obsspace(FileList, OutFile, GeoDir):
    # get lists of all variables
    AllVarNames = []
    LocVarNames = []
    AllVarNames.append('Location')
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
    AllVarNames.remove('Location')
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
        if fullvname == 'MetaData/dateTime':
            VarTypes[fullvname] = np.dtype('int64')
        else:
            VarTypes[fullvname] = _var.numpy_dtype()
        for attr in _var.attrs:
            if attr not in ['DIMENSION_LIST', '_FillValue']:
                gname, vname = fullvname.split('/')
                varAttrs[(vname, gname)][attr] = _var.read_attr(attr)
        # for now going to assume all are just 'Location' dim
        VarDims[fullvname] = ['Location']
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
                if vname == 'MetaData/dateTime':
                    tmpdata = [int(x.strftime('%s')) for x in tmpdata]
                    tmpdata = np.array(tmpdata).astype('int64')
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
    DimDict['Location'] = len(idx)
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
    DataVarUnique = np.empty((len(DataVarData), len(idx)), dtype='O')
    for idx2, fullvname in enumerate(AllVarNames):
        gname, vname = fullvname.split('/')
        if fullvname == 'MetaData/dateTime':
            fillval = iconv.get_default_fill_val(VarTypes[fullvname], isDateTime=True)
        else:
            fillval = iconv.get_default_fill_val(VarTypes[fullvname])
        mask = ~(DataVarData[idx2, ...] == fillval)
        DataVarUnique[idx2, ...] = fillval
        DataVarUnique[idx2, inv[mask]] = DataVarData[idx2, mask]
    for idx2, fullvname in enumerate(AllVarNames):
        gname, vname = fullvname.split('/')
        if fullvname == 'MetaData/dateTime':
            OutData[(vname, gname)] = DataVarUnique[idx2, ...].astype(np.dtype('object'))
        else:
            OutData[(vname, gname)] = DataVarUnique[idx2, ...].astype(VarTypes[fullvname])

    writer = iconv.IodaWriter(OutFile, LocKeyList, DimDict)
    writer.BuildIoda(OutData, VarDims, varAttrs, globalAttrs)

    # now write out combined GeoVaLs file
    if GeoDir:
        # get list of geoval files
        GeoFileList = []
        GeoVarNames2 = []
        GeoVarTypes2 = []
        GeoVarNames3 = []
        GeoVarTypes3 = []
        GeoVarNames31 = []
        GeoVarTypes31 = []
        for f in FileList:
            inob = f.split('/')[-1]
            ingeo = inob.replace('obs', 'geoval')
            g = GeoDir+'/'+ingeo
            GeoFileList.append(g)
        # figure out possible variable names
        for f in GeoFileList:
            ncf = nc.Dataset(f, mode='r')
            for key, value in ncf.variables.items():
                if key not in GeoVarNames2 and key not in GeoVarNames3 and key not in GeoVarNames31:
                    if value.ndim == 1:
                        GeoVarNames2.append(key)
                        GeoVarTypes2.append(ncf.variables[key].dtype)
                    else:
                        if 'nlevs' in value.dimensions:
                            GeoVarNames3.append(key)
                            GeoVarTypes3.append(ncf.variables[key].dtype)
                        else:
                            GeoVarNames31.append(key)
                            GeoVarTypes31.append(ncf.variables[key].dtype)

            ncf.close()
        GeoVarData2 = []
        GeoVarIdx2 = []
        GeoVarData3 = []
        GeoVarIdx3 = []
        GeoVarData31 = []
        GeoVarIdx31 = []
        for idx2, v in enumerate(GeoVarNames2):
            tmpgeodata = []
            tmpgeoidx = []
            for f in GeoFileList:
                ncf = nc.Dataset(f, mode='r')
                try:
                    tmpdata = np.array(ncf.variables[v])
                    tmpgeodata.append(tmpdata)
                    tmpgeoidx.append(np.ones_like(tmpdata).astype(int)*int(idx2))
                except KeyError:
                    tmpdata = np.ones_like(np.array(ncf.variables['time'])).astype(GeoVarTypes2[idx2])
                    if GeoVarTypes2[idx2] == np.int32:
                        tmpdata = tmpdata * nc.default_fillvals['i4']
                    else:
                        tmpdata = tmpdata * np.abs(nc.default_fillvals['f4'])
                    tmpgeodata.append(tmpdata)
                    tmpgeoidx.append(np.ones_like(tmpdata).astype(int)*int(idx2))
                ncf.close()
            tmpgeodata = np.hstack(tmpgeodata)
            tmpgeoidx = np.hstack(tmpgeoidx)
            GeoVarData2.append(tmpgeodata)
            GeoVarIdx2.append(tmpgeoidx)
        GeoVarData2 = np.vstack(GeoVarData2)
        GeoVarIdx2 = np.vstack(GeoVarIdx2)
        GeoVarData2 = np.transpose(GeoVarData2)
        GeoVarIdx2 = np.transpose(GeoVarIdx2)
        GeoVarUnique2 = np.ones((len(idx), len(GeoVarData2[0])))*np.abs(nc.default_fillvals['f4'])
        for idx2, v in enumerate(GeoVarNames3):
            tmpgeodata = []
            tmpgeoidx = []
            for f in GeoFileList:
                ncf = nc.Dataset(f, mode='r')
                try:
                    tmpdata = np.array(ncf.variables[v])
                    tmpgeodata.append(tmpdata)
                    tmpgeoidx.append(np.ones_like(tmpdata).astype(int)*int(idx2))
                except KeyError:
                    try:
                        tmpdata = np.ones_like(np.array(ncf.variables['air_pressure'])).astype(GeoVarTypes3[idx2])
                    except KeyError:
                        tmpdata = np.ones_like(np.array(ncf.variables['atmosphere_ln_pressure_coordinate'])).astype(GeoVarTypes3[idx2])
                    if GeoVarTypes3[idx2] == np.int32:
                        tmpdata = tmpdata * nc.default_fillvals['i4']
                    else:
                        tmpdata = tmpdata * np.abs(nc.default_fillvals['f4'])
                    tmpgeodata.append(tmpdata)
                    tmpgeoidx.append(np.ones_like(tmpdata).astype(int)*int(idx2))
            tmpgeodata = np.vstack(tmpgeodata)
            tmpgeoidx = np.vstack(tmpgeoidx)
            GeoVarData3.append(tmpgeodata)
            GeoVarIdx3.append(tmpgeoidx)
        GeoVarData3 = np.dstack(GeoVarData3)
        GeoVarIdx3 = np.dstack(GeoVarIdx3)
        GeoVarUnique3 = np.ones((len(idx), len(GeoVarData3[0]), len(GeoVarData3[0, 0, :])))*np.abs(nc.default_fillvals['f4'])
        for idx2, v in enumerate(GeoVarNames31):
            tmpgeodata = []
            tmpgeoidx = []
            for f in GeoFileList:
                ncf = nc.Dataset(f, mode='r')
                try:
                    tmpdata = np.array(ncf.variables[v])
                    tmpgeodata.append(tmpdata)
                    tmpgeoidx.append(np.ones_like(tmpdata).astype(int)*int(idx2))
                except KeyError:
                    try:
                        tmpdata = np.ones_like(np.array(ncf.variables['air_pressure_levels'])).astype(GeoVarTypes31[idx2])
                    except KeyError:
                        tmpdata = np.ones_like(np.array(ncf.variables['atmosphere_ln_pressure_interface_coordinate'])).astype(GeoVarTypes31[idx2])
                    if GeoVarTypes31[idx2] == np.int32:
                        tmpdata = tmpdata * nc.default_fillvals['i4']
                    else:
                        tmpdata = tmpdata * np.abs(nc.default_fillvals['f4'])
                    tmpgeodata.append(tmpdata)
                    tmpgeoidx.append(np.ones_like(tmpdata).astype(int)*int(idx2))
            tmpgeodata = np.vstack(tmpgeodata)
            tmpgeoidx = np.vstack(tmpgeoidx)
            GeoVarData31.append(tmpgeodata)
            GeoVarIdx31.append(tmpgeoidx)
        GeoVarData31 = np.dstack(GeoVarData31)
        GeoVarIdx31 = np.dstack(GeoVarIdx31)
        GeoVarUnique31 = np.ones((len(idx), len(GeoVarData31[0]), len(GeoVarData31[0, 0, :])))*np.abs(nc.default_fillvals['f4'])
        for ii, jj in np.ndindex(GeoVarData2.shape):
            j = GeoVarIdx2[ii, jj]
            i = inv[ii]
            if GeoVarData2[ii, jj] != nc.default_fillvals['i4'] and GeoVarData2[ii, jj] != np.abs(nc.default_fillvals['f4']):
                GeoVarUnique2[i, j] = GeoVarData2[ii, jj]
        for ii, kk, jj in np.ndindex(GeoVarData3.shape):
            j = GeoVarIdx3[ii, kk, jj]
            i = inv[ii]
            if GeoVarData3[ii, kk, jj] != nc.default_fillvals['i4'] and GeoVarData3[ii, kk, jj] != np.abs(nc.default_fillvals['f4']):
                GeoVarUnique3[i, kk, j] = GeoVarData3[ii, kk, jj]
        for ii, kk, jj in np.ndindex(GeoVarData31.shape):
            j = GeoVarIdx31[ii, kk, jj]
            i = inv[ii]
            if GeoVarData31[ii, kk, jj] != nc.default_fillvals['i4'] and GeoVarData31[ii, kk, jj] != np.abs(nc.default_fillvals['f4']):
                GeoVarUnique31[i, kk, j] = GeoVarData31[ii, kk, jj]
        outgeo = OutFile.split('/')[-1]
        OutGeoFile = outgeo.replace('obs', 'geoval')
        OutGeoFile = GeoDir + '/' + OutGeoFile
        of = nc.Dataset(OutGeoFile, 'w', format='NETCDF4')
        of.setncattr("date_time", ncf.getncattr("date_time"))
        nlocs = len(GeoVarUnique3)
        nlevs = len(GeoVarUnique3[0])
        ninterfaces = len(GeoVarUnique31[0])
        of.createDimension("nlocs", nlocs)
        of.createDimension("nlevs", nlevs)
        of.createDimension("ninterfaces", ninterfaces)
        for ivar, var in enumerate(GeoVarNames2):
            dims = ("nlocs", )
            var_out = of.createVariable(var, GeoVarTypes2[ivar], dims)
            var_out[...] = GeoVarUnique2[..., ivar]
        for ivar, var in enumerate(GeoVarNames3):
            dims = ("nlocs", "nlevs")
            var_out = of.createVariable(var, GeoVarTypes3[ivar], dims)
            var_out[...] = GeoVarUnique3[..., ivar]
        for ivar, var in enumerate(GeoVarNames31):
            dims = ("nlocs", "ninterfaces")
            var_out = of.createVariable(var, GeoVarTypes31[ivar], dims)
            var_out[...] = GeoVarUnique31[..., ivar]


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
