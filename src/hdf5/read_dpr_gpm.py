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
import datetime
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


def read_dpr_gpm(dpr_fnames,
                 dpr_dir=[],
                 dt_start=datetime.datetime(2014, 4, 1),
                 dt_end=datetime.datetime(2030, 1, 1)):

    if len(dpr_fnames) == 0:
        # files are partitioned into almost two hours period
        dt_start1 = dt_start - datetime.timedelta(hours=3)
        dt_end1 = dt_end + datetime.timedelta(hours=2)
        dt_range = [dt_start1 + datetime.timedelta(hours=x) for x in range(int((dt_end1 - dt_start1).total_seconds() // 3600) + 1)]
        dpr_fnames = []
        for idt in dt_range:
            # 2A.GPM.DPR.V9-20211125.20170907-S075404-E092639.020033.V07A.HDF5
            file_pattern = '*.%s*.HDF5' % idt.strftime('%Y%m%d-S%H')
            for dir, _, _ in os.walk(dpr_dir):
                dpr_fnames.extend(glob(os.path.join(dir, file_pattern)))

    if not dpr_fnames:
        return None

    # limit data so that dt_start <= date <= dt_end
    dt_start = round((dt_start - epoch).total_seconds())
    dt_end = round((dt_end - epoch).total_seconds())

    for f in dpr_fnames:
        dpr_data = read_dpr_hdf_file(f)
        logid = (dpr_data.epoch_time.values >= dt_start) & (dpr_data.epoch_time.values < dt_end)
        if np.sum(logid) == 0:
            continue
        scanid = np.arange(dpr_data.scanline.size)[logid]
        dpr_data = dpr_data.isel(scanline=scanid)

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
