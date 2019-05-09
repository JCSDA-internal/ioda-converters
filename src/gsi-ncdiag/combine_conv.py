#!/usr/bin/env python
# combine_conv.py
# combine conventional obs from IODA format into
# one output file with matching corresponding locations
# and missing data where applicable

import netCDF4 as nc
import ioda_conv_ncio as iconv
import numpy as np
import argparse
from collections import defaultdict, OrderedDict
import datetime as dt
from orddicts import DefaultOrderedDict

vtypedict = {
    'int32': 'integer',
    '|S1': 'string',
    'float32': 'float',
}


def concat_ioda(FileList, OutFile):
    outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    rec_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    AttrData = {}
    DataVarNames = []
    DataVars = []
    DataVType = []
    MetaVars = []
    MetaVarNames = []
    MetaVType = []
    VarMetaVars = []
    RecKeyList = []
    LocKeyList = []
    MetaInAll = {}
    # get lists of variables
    for f in FileList:
        ncf = nc.Dataset(f, mode='r')
        for key, value in ncf.variables.items():
            vs = key.split('@')
            if vs[1] not in ['MetaData', 'VarMetaData']:
                if vs[0] not in DataVarNames:
                    DataVarNames.append(vs[0])
                if vs[0] in DataVarNames:
                    if key not in DataVars:
                        DataVars.append(key)
                        vtype = ncf.variables[key].dtype
                        DataVType.append(vtype)
            else:
                if vs[1] == 'MetaData' and key not in MetaVars and vs[0] not in ['time', 'ObsIndex']:
                    MetaVars.append(key)
                    MetaVarNames.append(tuple(vs))
                    vtype = ncf.variables[key].dtype
                    MetaVType.append(vtype)
                    vtypestr = vtypedict[str(vtype)]
                    if vs[1] == 'datetime':
                        vtypestr = 'datetime'
                    LocKeyList.append((vs[0], vtypestr))
                elif vs[1] == 'VarMetaData' and key not in VarMetaVars and vs[0] not in ['variable_names']:
                    VarMetaVars.append(key)
        ncf.close()
    # determine which metadata is in all files
    for v in MetaVars:
        MetaInAll[v] = True
        for f in FileList:
            ncf = nc.Dataset(f, mode='r')
            try:
                a = ncf.variables[v][0, ...]
            except KeyError:
                MetaInAll[v] = False
            ncf.close()
    # extract metadata and generate a numpy array
    print(MetaVars)
    MetaVarData = []
    MetaVarUnique = []
    for v in MetaVars:
        tmpvardata = []
        if MetaInAll[v]:
            for f in FileList:
                ncf = nc.Dataset(f, mode='r')
                tmpdata = ncf.variables[v][:]
                tmpvardata.append(tmpdata)
                ncf.close()
        try:
            ashape = tmpdata.shape[1]
            tmpvardata = np.vstack(tmpvardata)
            tmpvardata = np.array([b''.join(td) for td in tmpvardata])
        except IndexError:
            tmpvardata = np.hstack(tmpvardata)
        MetaVarData.append(tmpvardata)
    MetaVarData = np.vstack(MetaVarData)
    MetaVarUnique, idx = np.unique(MetaVarData, return_index=True, axis=1)
    # grab variables to write out
    DataVarData = []
    for idx2, v in enumerate(DataVars):
        tmpvardata = []
        for f in FileList:
            ncf = nc.Dataset(f, mode='r')
            try:
                tmpdata = ncf.variables[v][:]
                tmpvardata.append(tmpdata)
            except KeyError:
                tmpdata = np.ones_like(ncf.variables['record_number@MetaData'][:]).astype(DataVType[idx2])
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
    DataVarUnique = DataVarData[..., idx]
    # set up things for the Ncwriter
    ridx = np.argwhere(np.array(MetaVars) == 'record_number@MetaData')[0][0]
    Recs = set(MetaVarUnique[ridx, :].astype('int'))
    nlocs = MetaVarUnique.shape[-1]
    writer = iconv.NcWriter(OutFile, RecKeyList, LocKeyList)
    for rec in Recs:
        rec_mdata['rec_id'] = np.asarray([rec], dtype='i4')
    var_mdata['variable_names'] = writer.FillNcVector(DataVarNames, "string")
    AttrData["date_time_string"] = validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
    writer._nrecs = len(Recs)
    writer._nvars = len(DataVarNames)
    writer._nlocs = nlocs

    for idx, vname in enumerate(MetaVarNames):
        if vname[0] in ['datetime', 'station_id']:
            tmp = MetaVarUnique[idx, :]
            tmp = tmp.astype(str)
            if vname[0] == 'datetime':
                jj = 20
            else:
                jj = 50
            tmp2 = np.empty((len(tmp), jj), dtype=str)
            # loop and replace chars
            for i in range(len(tmp)):
                for j in range(len(tmp[i])):
                    tmp2[i, j] = str(tmp[i][j])
            loc_mdata[vname[0]] = tmp2.astype(MetaVType[idx])
        else:
            loc_mdata[vname[0]] = MetaVarUnique[idx, ...].astype(MetaVType[idx])
    for idx, vname in enumerate(DataVars):
        outdata[tuple(vname.split('@'))] = DataVarUnique[idx, ...].astype(DataVType[idx])
    writer.BuildNetcdf(outdata, rec_mdata, loc_mdata, var_mdata, AttrData)


######################################################
######################################################
if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description='Combine conventional obs in IODA format into one output file',
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='list of the input files to combine',
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output', help='name of the output IODA observation file',
        type=str, required=True, default=None)

    args = parser.parse_args()

    FileList = args.input
    OutFile = args.output

    concat_ioda(FileList, OutFile)
