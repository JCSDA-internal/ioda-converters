# This reader can be used to read combined Ku/Ka band reflectivites
# and format them in xarray format
# Isaac Moradi, April 10, 2023

import os
from glob import glob
import sys
import copy

import numpy as np
import scipy as sp  
import scipy.io as sio  
import datetime, time
import pdb
import pandas as pd
import xarray as xr
import h5py
from collections import OrderedDict
from scipy.interpolate import interp1d
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

os.environ["HDF5_USE_FILE_LOCKING"] = "FALSE"

# do not remove this to avoid error in reading HDF
#from pyhdf.HDF import *
#from pyhdf.VS import *

class H5ls:
    def __init__(self):
        # Store an empty list for dataset names
        self.names = []

    def __call__(self, name, h5obj):
        # only h5py datasets have dtype attribute, so we can search on this
        if hasattr(h5obj,'dtype') and not name in self.names:
            self.names += [name]


        # we have no return so that the visit function is recursive

        
# https://hdfeos.org/zoo/OTHER/2010128055614_21420_CS_2B-GEOPROF_GRANULE_P_R04_E03.hdf.py
def read_dpr_hdf_file(fname):
    from scipy import interpolate
    
    print(fname)
    df = h5py.File(fname)
    h5ls = H5ls()
    # this will now visit all objects inside the hdf5 file and store datasets in h5ls.names
    df.visititems(h5ls)
    # get all the dimensions
    dims = {}
    data = {}
    for k in h5ls.names:
        if 'AlgorithmRuntimeInfo' in k: continue
        
        if 'DimensionNames' in  df[k].attrs:
            data[k] = df[k][:]
            dims[k] = df[k].attrs['DimensionNames'].decode('UTF-8').split(',')  
    df.close()
    
    unique_dims = {}        
    for k in data:
        shape = data[k].shape
        for idim, dname in enumerate(dims[k]):
            # xarray does not allow method as dimension for whatever reason
            dname = dname.replace('method', 'method1')
            unique_dims[dname] = np.arange(shape[idim])

    xr_data = xr.Dataset()
    for dname in unique_dims:
        xr_data[dname] = unique_dims[dname]

    for k in data:
        xr_dims = OrderedDict()
        for dname in dims[k]:
            dname = dname.replace('method', 'method1')
            xr_dims[dname] = unique_dims[dname]
        xr_data[k] = xr.DataArray(data[k], dims = xr_dims)


    epoch_time = np.zeros(xr_data.nscan.size)
    for iscan in xr_data.nscan.values:
        time1 = datetime.datetime(xr_data['FS/ScanTime/Year'].values[iscan],
                        xr_data['FS/ScanTime/Month'].values[iscan],
                        xr_data['FS/ScanTime/DayOfMonth'].values[iscan],
                        xr_data['FS/ScanTime/Hour'].values[iscan],
                        xr_data['FS/ScanTime/Minute'].values[iscan],
                        xr_data['FS/ScanTime/Second'].values[iscan],
                        xr_data['FS/ScanTime/MilliSecond'].values[iscan])
        epoch_time[iscan] = round((time1 - epoch).total_seconds())
    
    # build output dictionary
    dprdata = xr.Dataset()
    dprdata['lat'] = xr_data['FS/Latitude']   # "nscan,nray=nfov";
    dprdata['lon'] = xr_data['FS/Longitude']  # "nscan,nray=nfov";
    dprdata['height'] = xr_data['FS/PRE/height'] # "nscan,nray=nfov,nbin=nelev";
    dprdata['epoch_time'] = xr_data['FS/navigation/scLat'].copy()
    dprdata['epoch_time'].values = np.squeeze(epoch_time)
    dprdata['obs_measured'] =  xr_data['FS/PRE/zFactorMeasured'] #"nscan,nray=nfov,nbin=nelev,nfreq=nchan"
    dprdata['obs'] =  xr_data['FS/SLV/zFactorFinal'] #"nscan,nray=nfov,nbin=nelev,nfreq=nchan"
    dprdata['obs'].values[dprdata['obs'].values < -100] = np.nan
    dprdata['precipWater'] = xr_data['FS/SLV/precipWater']
    dprdata['zenith_angle'] = xr_data['FS/PRE/localZenithAngle'] #   nscan,nray=nfov,nfreq=nchan    
    dprdata['azimuth_angle'] = dprdata['zenith_angle'].copy() * 0
    dprdata['centerFreq'] = xr.DataArray(np.array([13.6, 35.5]), dims={"nfreq": 2}) # GHz
    dprdata['centerWN'] = xr.DataArray(np.array([0.453, 1.183]),  dims={"nfreq": 2}) # 1/cm
    
    # rename dimensions
    dprdata = dprdata.rename({'nbin': 'elevation', 'nfreq': 'channel', 'nray': 'fov', 'nscan': 'scanline'})
    dprdata['fov1'] = dprdata.fov.copy()
    
    return dprdata


def read_dpr_gpm(dpr_fnames):

    for f in dpr_fnames:
        dpr_data = read_dpr_hdf_file(f)

        if 'dpr_data1' not in vars():  # not defined yet
            dpr_data1 = dpr_data
        else:
            # common keys
            for k in (dpr_data.keys() & dpr_data1.keys()):
                dpr_data1[k] = np.append(dpr_data1[k], dpr_data[k], axis=0)

    if 'dpr_data1' not in vars():
        return []



    # combine geovar is used to combine DPR with precipitation retrievals from DPR
    dpr_data1 = dpr_data1.stack(obs_id=('scanline', 'fov')).reset_index('obs_id')
    dpr_data1 = dpr_data1.transpose('obs_id', 'channel', 'elevation')

    # convert to jd/lev/lat/lon
    lon = dpr_data1['lon'].values
    lon[lon < 0] = 360 + lon[lon < 0]
    dpr_data1['lon'].values = lon

    dpr_data1["sequenceNumber"] = xr.DataArray(np.arange(dpr_data1.obs_id.size), dpr_data1.obs_id.coords)
        
    return dpr_data1
