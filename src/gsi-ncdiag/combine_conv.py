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
#from orddicts import DefaultOrderedDict

def concat_ioda(FileList, OutFile):
    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: defaultdict(dict)) # fix this to defaultordereddict
    VarNames = []
    DataVars = []
    LocVars = []
    RecKeyList = []
    LocKeyList = [] 
    AttrData = {}
    LocKeyList.append(('datetime','string'))
    for f in FileList:
        ncf = nc.Dataset(f, mode='r')
        for key, value in ncf.variables.items():
            if key not in VarNames:
                VarNames.append(key)
                vs = key.split('@')
                #if vs[1] == 'ObsValue':
                #    varDict[vs[0]]['valKey'] = vs[0], vs[1] 
                #elif vs[1] == 'ObsError':
                #    varDict[vs[0]]['errKey'] = vs[0], vs[1] 
                #elif vs[1] == 'PreQC':
                #    varDict[vs[0]]['qcKey'] = vs[0], vs[1] 
                if vs[1] == 'MetaData': # assume all 'MetaData' are LocKeys
                    vtype = ncf.variables[key].dtype
                    if vtype == 'float32':
                        dtype = 'float'
                    elif vtype == '|S1':
                        dtype = 'string'
                    elif vtype == 'int32':
                        dtype = 'integer'
                    if vs[0] not in ['time','gsi_wind_red_factor']:
                        LocKeyList.append((vs[0],dtype))
                        LocVars.append(key)
                elif vs[1] == 'VarMetaData':
                    pass
                else:
                    DataVars.append(key)
        
        nloc = ncf.dimensions['nlocs'].size
        validtime = dt.datetime.strptime(str(ncf.getncattr('date_time')),"%Y%m%d%H")
        tdiff = ncf.variables['time@MetaData'][:]
        for i in range(nloc):
            try:
                recs = ncf.variables['record_number'][:]
                recKey = recs[i]
            except KeyError:
                recKey = 0
            vals = []
            timestamp = validtime + dt.timedelta(hours=float(tdiff[i]))
            vals.append(timestamp.strftime("%Y-%m-%dT%H:%M:%SZ"))
            for v in LocVars:
                val = ncf.variables[v][:]
                if (np.ma.isMaskedArray(val[i])):
                    vals.append(b''.join(np.ma.getdata(val[i])))
                else:
                    vals.append(val[i])
            LocKey = tuple(vals)
            for v in DataVars:
                try:
                    dout = ncf.variables[v][:]
                    outdata[recKey][LocKey][tuple(v.split('@'))] = dout[i]
                except KeyError:
                    pass
        ncf.close()

    AttrData["date_time_string"] = validtime.strftime("%Y-%m-%dT%H:%M:%SZ")
    writer = iconv.NcWriter(OutFile, RecKeyList, LocKeyList)
    (ObsVars, RecMdata, LocMdata, VarMdata) = writer.ExtractObsData(outdata)
    writer.BuildNetcdf(ObsVars, RecMdata, LocMdata, VarMdata, AttrData)

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
    
