#!/usr/bin/env python3

#
# (C) Copyright 2020-2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
#
# contribution based on prototype by 2023, Isaac Moradi
#

import os
from glob import glob
import sys

import numpy as np
from datetime import datetime, timezone
import xarray as xr
import h5py
from collections import OrderedDict
from pyiodaconv.def_jedi_utils import epoch

os.environ["HDF5_USE_FILE_LOCKING"] = "FALSE"


class H5ls:
    def __init__(self):
        # Store an empty list for dataset names
        self.names = []

    def __call__(self, name, h5obj):
        # only h5py datasets have dtype attribute, so we can search on this
        if hasattr(h5obj, 'dtype') and name not in self.names:
            self.names += [name]

        # we have no return so that the visit function is recursive


def read_dpr_hdf_file(fname):

    print(fname)
    df = h5py.File(fname)
    h5ls = H5ls()
    # this will now visit all objects inside the hdf5 file and store datasets in h5ls.names
    df.visititems(h5ls)
    # get all the dimensions
    dims = {}
    data = {}
    for k in h5ls.names:
        if 'AlgorithmRuntimeInfo' in k:
            continue

        if 'DimensionNames' in df[k].attrs:
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
        xr_data[k] = xr.DataArray(data[k], dims=xr_dims)

    epoch_time = np.zeros(xr_data.nscan.size)
    for iscan in xr_data.nscan.values:
        time1 = datetime(xr_data['FS/ScanTime/Year'].values[iscan],
                                  xr_data['FS/ScanTime/Month'].values[iscan],
                                  xr_data['FS/ScanTime/DayOfMonth'].values[iscan],
                                  xr_data['FS/ScanTime/Hour'].values[iscan],
                                  xr_data['FS/ScanTime/Minute'].values[iscan],
                                  xr_data['FS/ScanTime/Second'].values[iscan],
                                  xr_data['FS/ScanTime/MilliSecond'].values[iscan])
        time1 = time1.replace(tzinfo=timezone.utc)
        epoch_time[iscan] = round((time1 - epoch).total_seconds())

    # build output dictionary
    dprdata = xr.Dataset()
    dprdata['lat'] = xr_data['FS/Latitude']       # "nscan,nray=nfov";
    dprdata['lon'] = xr_data['FS/Longitude']      # "nscan,nray=nfov";
    dprdata['height'] = xr_data['FS/PRE/height']  # "nscan,nray=nfov,nbin=nelev";
    dprdata['epoch_time'] = xr_data['FS/navigation/scLat'].copy()
    dprdata['epoch_time'].values = np.squeeze(epoch_time)
    dprdata['obs_measured'] = xr_data['FS/PRE/zFactorMeasured']  # "nscan,nray=nfov,nbin=nelev,nfreq=nchan"
    dprdata['obs'] = xr_data['FS/SLV/zFactorFinal']  # "nscan,nray=nfov,nbin=nelev,nfreq=nchan"
    dprdata['obs'].values[dprdata['obs'].values < -100] = np.nan
    dprdata['zenith_angle'] = xr_data['FS/PRE/localZenithAngle']  # nscan,nray=nfov,nfreq=nchan
    dprdata['azimuth_angle'] = dprdata['zenith_angle'].copy() * 0
    dprdata['centerFreq'] = xr.DataArray(np.array([13.6, 35.5]), dims={"nfreq": 2})  # GHz
    dprdata['centerWN'] = xr.DataArray(np.array([0.453, 1.183]), dims={"nfreq": 2})  # 1/cm

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
            dpr_data1 = xr.concat([dpr_data, dpr_data1], dim='scanline')

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
