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
import sys
import copy

import numpy as np
import datetime
import pdb
import xarray as xr
from pyiodaconv.def_jedi_utils import iso8601_string, epoch


def read_cloudsat_hdf_file(fname):
    print(f"Reading {fname}")
    from pyhdf.SD import SD, SDC
    from pyhdf import HDF, VS, V

    hdf = SD(fname, SDC.READ)

    # Read datasets.
    DATAFIELD_NAME = 'Radar_Reflectivity'
    dset = hdf.select(DATAFIELD_NAME)
    data = dset[:, :]

    ht = hdf.select('Height')
    height = ht[:, :].copy()

    # Read attributes.
    '''
    attrs = dset.attributes(full=1)
    lna=attrs["long_name"]
    long_name = lna[0]
    sfa=attrs["factor"]
    scale_factor = sfa[0]
    vra=attrs["valid_range"]
    valid_min = vra[0][0]
    valid_max = vra[0][1]
    ua=attrs["units"]
    units = ua[0]

    attrs_h = ht.attributes(full=1)
    uah=attrs_h["units"]
    units_h = uah[0]
    '''

    h = HDF.HDF(fname)
    vs = h.vstart()

    # Read the scales
    xid = vs.find('Radar_Reflectivity.factor')
    scale_factor = np.array(vs.attach(xid)[:]).flatten()
    xid = vs.find('Radar_Reflectivity.offset')
    offset = np.array(vs.attach(xid)[:]).flatten()
    xid = vs.find('Radar_Reflectivity.valid_range')
    valid_range = np.array(vs.attach(xid)[:]).flatten()
    valid_min = valid_range[0]
    valid_max = valid_range[1]
    # Process valid range.
    invalid = np.logical_or(data < valid_min, data > valid_max)
    dataf = data.astype(float)
    dataf[invalid] = np.nan

    # Apply scale factor according to [1].
    dataf = dataf / scale_factor

    xid = vs.find('start_time')
    start_time = np.array(vs.attach(xid)[:]).flatten()
    start_time = datetime.datetime.strptime(start_time[0], '%Y%m%d%H%M%S')

    xid = vs.find('Latitude')
    latid = vs.attach(xid)
    latid.setfields('Latitude')
    nrecs, _, _, _, _ = latid.inquire()
    latitude = np.array(latid.read(nRec=nrecs))
    latid.detach()

    lonid = vs.attach(vs.find('Longitude'))
    lonid.setfields('Longitude')
    nrecs, _, _, _, _ = lonid.inquire()
    longitude = np.array(lonid.read(nRec=nrecs))
    lonid.detach()

    demid = vs.attach(vs.find('DEM_elevation'))
    demid.setfields('DEM_elevation')
    nrecs, _, _, _, _ = demid.inquire()
    dem = np.array(demid.read(nRec=nrecs))
    demid.detach()

    sfcid = vs.attach(vs.find('SurfaceHeightBin_fraction'))
    sfcid.setfields('SurfaceHeightBin_fraction')
    nrecs, _, _, _, _ = sfcid.inquire()
    SurfaceHeightBin_fraction = np.array(sfcid.read(nRec=nrecs))
    sfcid.detach()

    sfcid = vs.attach(vs.find('SurfaceHeightBin'))
    sfcid.setfields('SurfaceHeightBin')
    nrecs, _, _, _, _ = sfcid.inquire()
    SurfaceHeightBin = np.array(sfcid.read(nRec=nrecs))
    sfcid.detach()

    RangeBinSize = 250
    SurfaceHeightBin_fraction[SurfaceHeightBin_fraction < -5] = 0
    surface_height = np.zeros(height.shape[0])
    for i, shb in enumerate(SurfaceHeightBin.flatten()):
        surface_height[i] = height[i, shb] + RangeBinSize * SurfaceHeightBin_fraction[i]

    timeid = vs.attach(vs.find('Profile_time'))
    timeid.setfields('Profile_time')
    nrecs, _, _, _, _ = timeid.inquire()
    time = timeid.read(nRec=nrecs)
    epoch_time = np.zeros(len(time))
    for i, ss in enumerate(time):
        time1 = start_time + datetime.timedelta(seconds=ss[0])
        epoch_time[i] = round((time1 - epoch).total_seconds())

    '''
    units_t =  timeid.attr('units').get()
    longname_t = timeid.attr('long_name').get()
    '''
    timeid.detach()

    # interpolate to common height
    '''
    dataf1 = np.ma.array(dataf, mask=np.isnan(dataf))
    for n in np.arange(dataf1.shape[0]):
        dataf1[n,:] = np.interp(height[0,:], height[n,:], dataf1[n,:])
    '''

    # build output dictionary
    csdata = {}
    csdata['lat'] = np.squeeze(latitude)
    csdata['lon'] = np.squeeze(longitude)
    csdata['height'] = np.squeeze(height)
    csdata['dem_elevation'] = np.squeeze(dem)
    csdata['surface_height'] = np.squeeze(surface_height)
    csdata['SurfaceHeightBin'] = np.squeeze(SurfaceHeightBin)
    csdata['epoch_time'] = np.squeeze(epoch_time)
    csdata['obs'] = np.squeeze(dataf)
    csdata['zenith_angle'] = csdata['lat'].copy() * 0
    csdata['azimuth_angle'] = csdata['lat'].copy() * 0

    return csdata


def read_cloudsat(cs_fnames):

    for f in cs_fnames:
        cs_data = read_cloudsat_hdf_file(f)

        if 'cs_data1' not in vars():  # not defined yet
            cs_data1 = cs_data
        else:
            # common keys
            for k in (cs_data.keys() & cs_data1.keys()):
                cs_data1[k] = np.append(cs_data1[k], cs_data[k], axis=0)

    if 'cs_data1' not in vars():
        return []

    # create xarray dataset
    nobs = cs_data1['obs'].shape[0]
    nlev = cs_data1['obs'].shape[1]
    nchan = 1
    channel = np.arange(1, nchan+1)   # cloudsat has one channel
    elevation = np.arange(1, nlev+1)  # elevation are different high levels that go from 1 to n_height

    obs_id = np.arange(nobs)
    cs_data = xr.Dataset()
    cs_data['obs_id'] = xr.DataArray(obs_id, dims={'obs_id': obs_id})
    cs_data['channel'] = xr.DataArray(channel, dims={'channel': channel})
    cs_data['elevation'] = xr.DataArray(elevation, dims={'elevation': elevation})

    for k in cs_data1:
        if (k == 'obs') or (k == 'height'):
            dims = {'obs_id': obs_id, 'elevation': elevation}
        else:
            dims = {'obs_id': obs_id}

        cs_data[k] = xr.DataArray(cs_data1[k], dims=dims)
    cs_data['scan_line'] = cs_data['obs_id'].copy()
    cs_data['fov1'] = cs_data['obs_id'].copy() * 0.0

    # expand the dimension for obs (reflectivities)
    expand_dims = {'obs': {'channel': 1}}
    for k in expand_dims:
        cs_data[k] = cs_data[k].expand_dims(dim=expand_dims[k])

    # reverse the coordinates and add obs_id
    cs_data = cs_data.transpose('obs_id', 'channel', 'elevation')

    # convert to jd/lev/lat/lon
    lon = cs_data['lon'].values
    lon[lon < 0] = 360 + lon[lon < 0]
    cs_data['lon'].values = lon

    cs_data["sequenceNumber"] = xr.DataArray(np.arange(cs_data.obs_id.size), cs_data.obs_id.coords)

    return cs_data
