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

import sys
import netCDF4 as nc
import xarray as xr
import numpy as np
import datetime
import time
import read_cloudsat

def main(args):
    # fname_in = ['2009212223327_17338_CS_2B-GEOPROF_GRANULE_P1_R05_E02_F00.hdf']
    fname_in = args.input
    fname_out = args.output

    cpr_obs = read_cloudsat.read_cloudsat(fname_in)
    cpr_obs = cpr_obs.rename_vars({"elevation": "elevation1"})
    cpr_obs = cpr_obs.stack(Location=['obs_id', 'elevation']).reset_index("Location")
    cpr_obs = cpr_obs.transpose("Location", "channel")

    locid = (cpr_obs.obs.values < -100) | (cpr_obs.obs.values > 100) | np.isnan(cpr_obs.obs.values) | np.isinf(cpr_obs.obs.values)
    locid = cpr_obs.Location.values[np.sum(locid,axis=1) == 0]
    cpr_obs = cpr_obs.isel(Location=locid)

    nobs = cpr_obs.Location.size
    nchan = cpr_obs.channel.size
    records = np.unique(cpr_obs.sequenceNumber.values).flatten()

    fill_value = 9.96921e+36
    fill_value_dbz = np.float32(-9999)

    refl_dims = ('Location', 'Channel') 
    tmpMat = np.ones((nobs, nchan,), dtype=np.float32)

    # Create the netCDF4 dataset
    with nc.Dataset(fname_out, 'w', format='NETCDF4', zlib=True) as ds:
        ds.createDimension('nrecs', 1)
        ds.createDimension('Channel', nchan)
        ds.createDimension('Location', nobs)

        ds.createVariable('Channel', np.int32, ('Channel',))
        ds.createVariable('Location', np.float32, ('Location',))
        ds.createVariable('nrecs', np.int32, ('nrecs',))

        ds['nrecs'][:] = np.int32(records.size)
        ds['Location'][:] = cpr_obs.Location.values.astype(np.int32)
        ds['Channel'][:] = cpr_obs.channel.values.astype(np.int32)

        ds['Location'].setncattr("suggested_chunk_dim", 100)
        ds['Channel'].setncattr("suggested_chunk_dim", 100)

        ds.setncattr_string("_ioda_layout", "ObsGroup")
        ds.setncattr("_ioda_layout_version", np.int32(0))
        ds.setncattr("date_time", np.int32(2009113003))
        ds.setncattr("satellite", "cloudsat")
        ds.setncattr("sensor", "cpr")
        ds.setncattr('history', 'Created by Isaac Moradi NASA/GMAO Sept 6, 2023')

        # Groups ['GsiHofX', 'MetaData', 'ObsError', 'ObsValue', 'PreQC', 'RecMetaData']

        g = 'ObsValue'
        ds_g = ds.createGroup(g)
        zvar =  ds_g.createVariable('ReflectivityAttenuated', np.float32, refl_dims, fill_value=fill_value_dbz, zlib=True)
        ds_g.variables['ReflectivityAttenuated'][:] = cpr_obs.obs.values.astype(np.float32)
        zvar.setncattr_string("units", str("dBz"))


        g = 'GsiHofX'
        ds_g = ds.createGroup(g)
        zvar = ds_g.createVariable('ReflectivityAttenuated', np.float32, refl_dims, fill_value=fill_value_dbz, zlib=True)
        '''
        ds_g.variables['ReflectivityAttenuated'][:] = cpr_sim.Reflectivity_Attenuated.values.astype(np.float32)
        zvar.setncattr_string("units", str("dBz"))
        '''

        g = 'ObsError'
        ds_g = ds.createGroup(g)
        zvar = ds_g.createVariable('ReflectivityAttenuated', np.float32, refl_dims, fill_value=fill_value, zlib=True)
        ds_g.variables['ReflectivityAttenuated'][:] = tmpMat.copy()
        zvar.setncattr_string("units", str("dBz"))

        g = 'PreQC'
        ds_g = ds.createGroup(g)
        zvar = ds_g.createVariable('ReflectivityAttenuated', np.float32, refl_dims, fill_value=fill_value, zlib=True)
        ds_g.variables['ReflectivityAttenuated'][:] = tmpMat.copy()
        zvar.setncattr_string("units", str("dBz"))

        g = "GsiBc"
        ds_g = ds.createGroup(g)
        zvar = ds_g.createVariable('ReflectivityAttenuated', np.float32, refl_dims, fill_value=fill_value, zlib=True)
        ds_g.variables['ReflectivityAttenuated'][:] = tmpMat.copy()
        zvar.setncattr_string("units", str("dBz"))

        g = "GsiHofXBc"
        ds_g = ds.createGroup(g)
        zvar = ds_g.createVariable('ReflectivityAttenuated', np.float32, refl_dims, fill_value=fill_value, zlib=True)
        ds_g.variables['ReflectivityAttenuated'][:] = tmpMat.copy()
        zvar.setncattr_string("units", str("dBz"))

        g = "GsiObsBias"
        ds_g = ds.createGroup(g)
        zvar = ds_g.createVariable('ReflectivityAttenuated', np.float32, refl_dims, fill_value=fill_value, zlib=True)
        ds_g.variables['ReflectivityAttenuated'][:] = tmpMat.copy()
        zvar.setncattr_string("units", str("dBz"))

        # we don't need the channel element anymore
        cpr_obs = cpr_obs.isel(channel=0)

        g = 'MetaData'
        ds_g = ds.createGroup(g)

        unixtime = cpr_obs.epoch_time.values

        varid = ds_g.createVariable('dateTime', np.int64, ('Location',))
        ds_g.variables['dateTime'][:] = unixtime.astype(np.int64)
        ds_g.variables['dateTime'].setncattr('units', 'seconds since 1970-01-01T00:00:00Z')

        ds_g.createVariable('sensorCentralFrequency', np.float32, ('Channel',), fill_value=fill_value)
        ds_g.createVariable('sensorCentralWavenumber', np.float32, ('Channel',), fill_value=fill_value)
        ds_g.createVariable('gsi_use_flag', np.int32, ('Channel',), fill_value=-2147483647)
        ds_g.createVariable('sensorPolarizationDirection', np.int32, ('Channel',), fill_value=-2147483647)
        ds_g.createVariable('latitude', np.float32, ('Location',), fill_value=fill_value)
        ds_g.createVariable('longitude', np.float32, ('Location',), fill_value=fill_value)
        ds_g.createVariable('sensorScanPosition', np.int32, ('Location',), fill_value=-2147483647)
        ds_g.createVariable('sensorChannelNumber', np.int32, ('Channel',), fill_value=-2147483647)
        ds_g.createVariable('sensorViewAngle', np.float32, ('Location',), fill_value=fill_value)
        ds_g.createVariable('sensorZenithAngle', np.float32, ('Location',), fill_value=fill_value)
        ds_g.createVariable('sensorAzimuthAngle', np.float32, ('Location',), fill_value=fill_value)
        ds_g.createVariable('solarAzimuthAngle', np.float32, ('Location',), fill_value=fill_value)
        ds_g.createVariable('solarZenithAngle', np.float32, ('Location',), fill_value=fill_value)
        #ds_g.createVariable('time', np.float32, ('Location',), fill_value=fill_value)
        ds_g.createVariable('sequenceNumber', np.int32, ('Location',), fill_value=-2147483647)

        ds_g.variables['sensorCentralFrequency'][:] = np.array([94.05]).astype(np.float32) #cpr_sim.Frequency.values.astype(np.float32)
        ds_g.variables['sensorCentralWavenumber'][:] = np.array([3.1371]).astype(np.float32) #cpr_sim.Wavenumber.values.astype(np.float32)
        ds_g.variables['gsi_use_flag'][:] = np.array([1]).astype(np.int32)
        ds_g.variables['sensorPolarizationDirection'][:] = np.array([9]).astype(np.int32)
        ds_g.variables['latitude'][:] = cpr_obs.lat.values.astype(np.float32)
        ds_g.variables['longitude'][:] = cpr_obs.lon.values.astype(np.float32)
        ds_g.variables['sensorScanPosition'][:] = cpr_obs.fov1.values.astype(np.int32)
        ds_g.variables['sensorChannelNumber'][:] = cpr_obs.channel.values.astype(np.int32)
        ds_g.variables['sensorViewAngle'][:] = cpr_obs.zenith_angle.values.astype(np.float32)
        ds_g.variables['sensorZenithAngle'][:] = cpr_obs.zenith_angle.values.astype(np.float32)
        ds_g.variables['sensorAzimuthAngle'][:] = cpr_obs.azimuth_angle.values.astype(np.float32)
        ds_g.variables['solarAzimuthAngle'][:] = np.zeros(nobs).astype(np.float32)
        ds_g.variables['solarZenithAngle'][:] = np.zeros(nobs).astype(np.float32)
        #ds_g.variables['time'][:] = np.ones(nobs).astype(np.float32)
        ds_g.variables['sequenceNumber'][:] = cpr_obs.sequenceNumber.values.astype(np.int32)

        ds_g.createVariable('height', np.float32, ('Location'), fill_value=fill_value)
        ds_g.createVariable('Layer', np.int32, ('Location',))
        ds_g.variables['height'][:] = cpr_obs.height.values.astype(np.float32)
        ds_g.variables['Layer'][:] = cpr_obs.elevation.values.astype(np.int32)

        g = "RecMetaData"
        ds_g = ds.createGroup(g)
        ds_g.createVariable('sequenceNumber', np.int32, ('Location',))
        ds_g.variables['sequenceNumber'][:] = cpr_obs["sequenceNumber"].values.astype(np.int32)


if __name__ == "__main__":

    import argparse
    import os
    parser = argparse.ArgumentParser(
        description=(
            'Reads the satellite data '
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of satellite observation input file(s)",
        type=str, nargs='+', required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-o', '--output',
        help='path to output ioda file',
        type=str, default=os.path.join(os.getcwd(), 'output.nc4'))
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, default=None)

    args = parser.parse_args()

    main(args)
