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

def concat_ioda(FileList, OutFile):
    nvars = 0
    nlocs = 0
    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    outdata_1 = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    outdatas = []
    rec_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    loc_mdata_1 = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    loc_mdatas = []
    var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    VarNames = []
    VarMetaNames = []
    DataVars = []
    DataVarNames = []
    LocVars = []
    RecKeyList = []
    LocKeyList = [] 
    AttrData = {}
    LocKeyList.append(('datetime','string'))
    LocVars.append('datetime@MetaData')
    for f in FileList:
        ncf = nc.Dataset(f, mode='r')
        for key, value in ncf.variables.items():
            if key not in VarNames:
                VarNames.append(key)
                vs = key.split('@')
                if vs[1] == 'MetaData': # assume all 'MetaData' are LocKeys
                    vtype = ncf.variables[key].dtype
                    if vtype == 'float32':
                        dtype = 'float'
                    elif vtype == '|S1':
                        dtype = 'string'
                    elif vtype == 'int32':
                        dtype = 'integer'
                    if vs[0] not in ['datetime','time']:
                        LocKeyList.append((vs[0],dtype))
                        LocVars.append(key)
                elif vs[1] == 'VarMetaData':
                    VarMetaNames.append(key)
                else:
                    if vs[1] == 'ObsValue':
                        nvars = nvars+1
                        DataVarNames.append(vs[0])
                    DataVars.append(key)
        
        nloc = ncf.dimensions['nlocs'].size
        nlocs = max(nlocs,nloc)
        validtime = dt.datetime.strptime(str(ncf.getncattr('date_time')),"%Y%m%d%H") 
    ncf.close()
    writer = iconv.NcWriter(OutFile, RecKeyList, LocKeyList)

    for f in FileList:
        print("Processing:"+f)
        ncf = nc.Dataset(f, mode='r')
        #recKeys = ncf.variables['record_number'][:]
        for idx, lvar in enumerate(LocVars):
            loc_mdata_name = LocKeyList[idx][0]
            #loc_mdata[loc_mdata_name] = np.append(loc_mdata[loc_mdata_name],ncf.variables[lvar][:])
            loc_mdata_1[loc_mdata_name] = ncf.variables[lvar][:] 
        loc_mdatas.append(loc_mdata_1)
        for idx, lvar in enumerate(DataVars):
            data_mdata_name = tuple(lvar.split('@'))
            try:
                outdata_1[data_mdata_name] = ncf.variables[lvar][:]
            except KeyError:
                pass
        outdatas.append(outdata_1)
    for d in loc_mdatas:
        loc_mdata.update(d)
    for d in outdatas:
        outdata.update(d)
    print(loc_mdata['latitude'])
    print(len(loc_mdata['latitude']))

    nrecs = 1
    rec_mdata['rec_id'] = np.asarray([999], dtype='i4')
    var_mdata['variable_names'] = writer.FillNcVector(DataVarNames, "string")
    AttrData["date_time_string"] = validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
    writer._nrecs = nrecs
    writer._nvars = nvars
    writer._nlocs = nlocs
    print(nlocs)
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
    
