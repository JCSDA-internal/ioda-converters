#!/usr/bin/env python3
# combine_files.py
# combine conventional obs and GOESIR (currently ahi_himawari8) from IODA format into
# one output file with matching corresponding locations
# and missing data where applicable

import sys
import netCDF4 as nc
import numpy as np
import argparse
from collections import defaultdict, OrderedDict
import datetime as dt
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict

vtypedict = {
    'int32': 'integer',
    '|S1': 'string',
    'float32': 'float',
}


def concat_ioda(FileList, OutFile, GeoDir):
    outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    AttrData = {}
    DataVarNames = []
    DataVars = []
    VarUnits = {}
    DataVType = []
    MetaVars = []
    MetaVarNames = []
    MetaVType = []
    VarMetaVars = []
    LocKeyList = []
    MetaInAll = {}
    # get lists of variables
    for f in FileList:
        ncf = nc.Dataset(f, mode='r')
        for key, value in ncf.variables.items():
            vs = key.split('@')
            if vs[1] not in ['MetaData', 'VarMetaData', 'TestReference']:
                if vs[0] not in DataVarNames:
                    DataVarNames.append(vs[0])
                if vs[0] in DataVarNames:
                    if key not in DataVars:
                        DataVars.append(key)
                        vtype = ncf.variables[key].dtype
                        DataVType.append(vtype)
                        try:
                            units = ncf.variables[vs[0]+"@ObsValue"].getncattr("units")
                            VarUnits[vs[0]] = units
                        except AttributeError:
                            pass

            else:
                if vs[1] == 'MetaData' and key not in MetaVars and vs[0] not in ['time', 'ObsIndex']:
                    MetaVars.append(key)
                    MetaVarNames.append(tuple(vs))
                    vtype = ncf.variables[key].dtype
                    MetaVType.append(vtype)
                    try:
                        units = ncf.variables[vs[0]+"@MetaData"].getncattr("units")
                        VarUnits[vs[0]] = units
                    except AttributeError:
                        pass
                    vtypestr = vtypedict[str(vtype)]
                    if vs[1] == 'datetime':
                        vtypestr = 'datetime'
                    LocKeyList.append((vs[0], vtypestr))
                elif vs[1] == 'VarMetaData' and key not in VarMetaVars and vs[0] not in ['variable_names']:
                    VarMetaVars.append(key)
        ncf.close()

    # determine the obstype. If the obstype is a GOESIR type, also get the sensor name and the satellite name
    # an example is: inob="ahi_himawari8_obs_2018041500.nc4", obstype="ahi_himawari8", sensor="ahi"
    # satellite="himawari8"
    inob = f.split('/')[-1]
    obstype = inob[:inob.rfind("obs")-1]
    if obstype == "ahi_himawari8":
        sensor = obstype.split('_')[0]
        satellite = obstype.split('_')[-1]

    # determine which metadata is in all files
    for v in MetaVars:
        MetaInAll[v] = True
        for f in FileList:
            ncf = nc.Dataset(f, mode='r')
            try:
                a = np.array(ncf.variables[v])[0, ...]
            except KeyError:
                MetaInAll[v] = False
            ncf.close()
    # extract metadata and generate a numpy array
    MetaVarData = []
    bad_idxs = []
    for v in MetaVars:
        tmpvardata = []
        if MetaInAll[v]:
            for f in FileList:
                ncf = nc.Dataset(f, mode='r')
                tmpdata = np.array(ncf.variables[v])
                tmpvardata.append(tmpdata)
                ncf.close()
        try:
            ashape = tmpdata.shape[1]
            tmpvardata = np.vstack(tmpvardata)
            tmpvardata = np.array([b''.join(td) for td in tmpvardata])
        except (IndexError, ValueError):
            if len(tmpvardata):
                tmpvardata = np.hstack(tmpvardata)
        if len(tmpvardata):
            MetaVarData.append(tmpvardata)
        else:
            bad_idxs.append(MetaVars.index(v))
    for i in sorted(bad_idxs, reverse=True):
        del MetaVars[i]
        del MetaVarNames[i]
        del MetaVType[i]
    MetaVarData = np.vstack(MetaVarData)
    MetaVarUnique, idx, inv, cnt = np.unique(MetaVarData, return_index=True, return_inverse=True, return_counts=True, axis=1)
    # grab variables to write out
    DataVarData = []
    DataVarIdx = []
    for idx2, v in enumerate(DataVars):
        tmpvardata = []
        for f in FileList:
            ncf = nc.Dataset(f, mode='r')
            try:
                tmpdata = np.array(ncf.variables[v])
                tmpvardata.append(tmpdata)
            except KeyError:
                tmpdata = np.ones_like(np.array(ncf.variables['record_number@MetaData'])).astype(DataVType[idx2])
                if DataVType[idx2] == np.int32:
                    tmpdata = tmpdata * nc.default_fillvals['i4']
                else:
                    tmpdata = tmpdata * np.abs(nc.default_fillvals['f4'])
                tmpvardata.append(tmpdata)
            validtime = dt.datetime.strptime(str(ncf.getncattr('date_time')), "%Y%m%d%H")
            ncf.close()
        tmpvardata = np.hstack(tmpvardata)
        DataVarData.append(tmpvardata)
    DataVarData = np.vstack(DataVarData)
    DataVarUnique = np.ones((len(DataVarData), len(idx))) * nc.default_fillvals['f4']
    for ii in range(DataVarData.shape[0]):
        mask = ~((DataVarData[ii, :] == nc.default_fillvals['i4']) | (DataVarData[ii, :] == np.abs(nc.default_fillvals['f4'])))
        DataVarUnique[ii, inv[mask]] = DataVarData[ii, mask]

    # set up things for the Ncwriter
    if obstype != "ahi_himawari8":
        ridx = np.argwhere(np.array(MetaVars) == 'record_number@MetaData')[0][0]
    nlocs = MetaVarUnique.shape[-1]
    writer = iconv.NcWriter(OutFile, LocKeyList)
    var_mdata['variable_names'] = writer.FillNcVector(DataVarNames, "string")
    # TODO add RecMetaData for Station ID, etc...
    AttrData["date_time_string"] = validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
    writer._nvars = len(DataVarNames)
    writer._nlocs = nlocs

    for idx3, vname in enumerate(MetaVarNames):
        if vname[0] in ['datetime', 'station_id']:
            # TODO Speed up this section creating a character array
            tmp = MetaVarUnique[idx3, :]
            tmp = tmp.astype(str)
            if vname[0] == 'datetime':
                jj = 20
            else:
                jj = 50
            tmp2 = np.empty((len(tmp), jj), dtype=str)
            # loop and replace chars
            for i in range(len(tmp)):
                s = tmp[i]
                tmp2[i, :len(s)] = list(s)
            loc_mdata[vname[0]] = tmp2.astype(MetaVType[idx3])
        else:
            loc_mdata[vname[0]] = MetaVarUnique[idx3, ...].astype(MetaVType[idx3])
    for idx3, vname in enumerate(DataVars):
        tmp = DataVarUnique[idx3, ...]
        tmp = tmp.astype(DataVType[idx3])
        if DataVType[idx3] == 'int32':
            tmp[tmp < -1e5] = nc.default_fillvals['i4']
        outdata[tuple(vname.split('@'))] = tmp
    if obstype == "ahi_himawari8":
        var_mdata['sensor_channel'] = np.asarray(list(range(7, 17)))
        AttrData["satellite"] = satellite
        AttrData["sensor"] = sensor
    writer.BuildNetcdf(outdata, loc_mdata, var_mdata, AttrData, VarUnits)

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
        OutGeoFile = OutFile.replace('obs', 'geoval')
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
        description='Combine conventional obs and GOESIR (currently ahi_himawari8) in IODA format into one output file',
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

    concat_ioda(FileList, OutFile, GeoDir)
