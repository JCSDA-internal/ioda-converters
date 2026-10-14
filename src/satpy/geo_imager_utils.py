#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Shared code for the satpy geostationary imager converters (ami_l1b_to_ioda.py,
# fci_l1c_to_ioda.py). It keeps the imager's native fixed grid: every Nth pixel and
# line is written (--subsample), as goes2ioda and the Himawari obs2ioda path do, so each
# location keeps its own geolocation, scan time and viewing geometry. Thinning is left
# to UFO (e.g. Gaussian Thinning).
#

from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import os

import numpy as np
from pyorbital.astronomy import get_alt_az, sun_zenith_angle
from pyorbital.orbital import get_observer_look
from satpy.scene import Scene

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import (
    set_metadata_attributes,
    set_obspace_attributes,
)

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

# mean Earth radius used for the nadir (view) angle at the satellite
earth_radius_km = 6371.0

# gross limits on brightness temperature, as in seviri_l1b_to_ioda.py
bt_min = 20.0
bt_max = 400.0
default_obs_error = 5.0


def get_parser(description):
    """Command line arguments shared by the geostationary imager converters."""
    parser = ArgumentParser(description=description, formatter_class=ArgumentDefaultsHelpFormatter)
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help='full path names of the input files for one full-disk scan',
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help='name of the output netCDF IODA-compliant file',
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-s', '--subsample',
        help='keep every Nth pixel and line of the 2 km IR grid (16 gives about 32 km at the sub-satellite point)',
        type=int, default=16)
    optional.add_argument(
        '--max-zenith',
        help='drop locations whose satellite zenith angle exceeds this (degrees)',
        type=float, default=80.0)
    return parser


def load_scene(reader, files, channels):
    """Read and calibrate the IR channels to brightness temperature."""
    scn = Scene(reader=reader, filenames=files)
    scn.load(channels, calibration='brightness_temperature')
    for name in channels:
        if scn[name].attrs['area'].shape != scn[channels[0]].attrs['area'].shape:
            raise ValueError(f"{name} is not on the same grid as {channels[0]}")
    return scn


def satellite_position(attrs):
    """Sub-satellite longitude, latitude (degrees) and altitude (km) from satpy's orbital parameters."""
    op = attrs.get('orbital_parameters', {})

    def first(*keys, default=None):
        for k in keys:
            if k in op and np.isfinite(op[k]):
                return float(op[k])
        return default

    lon = first('satellite_actual_longitude', 'satellite_nominal_longitude', 'projection_longitude')
    lat = first('satellite_actual_latitude', 'satellite_nominal_latitude', 'projection_latitude', default=0.0)
    alt = first('satellite_actual_altitude', 'satellite_nominal_altitude', 'projection_altitude')
    return lon, lat, alt / 1000.0


def sample_grid(scn, ref, subsample):
    """Rows, columns, latitude and longitude of every Nth pixel and line of the reference channel grid."""
    area = scn[ref].attrs['area']
    nrows, ncols = area.shape
    first = subsample // 2
    data_slice = (slice(first, nrows, subsample), slice(first, ncols, subsample))
    lon, lat = area.get_lonlats(data_slice=data_slice)
    rows, cols = np.meshgrid(np.arange(first, nrows, subsample), np.arange(first, ncols, subsample), indexing='ij')
    return data_slice, rows.ravel(), cols.ravel(), np.asarray(lat, 'f8').ravel(), np.asarray(lon, 'f8').ravel()


def line_times(scn, ref, rows, north_to_south=True):
    """
    Observation time of each row, interpolated linearly between the scan start and end
    (for readers that do not provide a per-pixel time). Row 0 is the north edge of satpy's grid.
    """
    attrs = scn[ref].attrs
    t0 = np.datetime64(attrs['start_time'], 'ns')
    t1 = np.datetime64(attrs['end_time'], 'ns')
    nrows = attrs['area'].shape[0]
    frac = rows / max(nrows - 1, 1)
    if not north_to_south:
        frac = 1.0 - frac
    return t0 + (frac * (t1 - t0).astype('i8')).astype('timedelta64[ns]')


def pixel_times(scn, name, data_slice, epoch):
    """Observation time of each sampled pixel from a reader's time dataset (NaN for no data)."""
    values = np.asarray(scn[name].data[data_slice].compute()).ravel()
    if np.issubdtype(values.dtype, np.datetime64):
        return values.astype('datetime64[ns]')
    # seconds since the given epoch
    times = np.full(values.shape, np.datetime64('NaT'), 'datetime64[ns]')
    ok = np.isfinite(values)
    times[ok] = np.datetime64(epoch, 'ns') + np.round(values[ok] * 1e9).astype('timedelta64[ns]')
    return times


def viewing_geometry(sat_lon, sat_lat, sat_alt_km, times, lat, lon):
    """Satellite zenith, azimuth and view angles and solar zenith and azimuth angles (degrees)."""
    tmid = times.min() + (times.max() - times.min()) / 2
    sensor_azimuth, elevation = get_observer_look(
        np.full(lat.size, sat_lon), np.full(lat.size, sat_lat), np.full(lat.size, sat_alt_km),
        tmid.astype('datetime64[us]').item(), lon, lat, np.zeros(lat.size))
    sensor_zenith = 90.0 - elevation
    # nadir angle at the satellite: sin(view) = R / (R + h) sin(zenith)
    sensor_view = np.rad2deg(np.arcsin(earth_radius_km / (earth_radius_km + sat_alt_km) * np.sin(np.deg2rad(sensor_zenith))))
    solar_zenith = sun_zenith_angle(times, lon, lat)
    _, solar_azimuth = get_alt_az(times, lon, lat)
    solar_azimuth = np.rad2deg(solar_azimuth) % 360.0
    return sensor_zenith, sensor_azimuth % 360.0, sensor_view, solar_zenith, solar_azimuth


def brightness_temperatures(scn, channels, data_slice):
    """(Location, Channel) brightness temperatures on the subsampled grid, with PreQC 2 for gross errors."""
    bt = np.stack([np.asarray(scn[name].data[data_slice].compute(), 'f4').ravel() for name in channels], axis=1)
    bad = ~np.isfinite(bt) | (bt < bt_min) | (bt > bt_max)
    preqc = np.where(bad, 2, 0).astype('i4')
    bt[bad] = float_missing_value
    return bt, preqc


def build_obs(lat, lon, seconds, channel_numbers, bt, preqc, geometry, scan_position, wmo_sat_id):
    sensor_zenith, sensor_azimuth, sensor_view, solar_zenith, solar_azimuth = geometry
    nlocs = lat.size
    obs = {
        ('brightnessTemperature', obsValName): bt,
        ('brightnessTemperature', obsErrName): np.where(preqc == 0, default_obs_error, float_missing_value).astype('f4'),
        ('brightnessTemperature', qcName): preqc,
        ('latitude', metaDataName): lat.astype('f4'),
        ('longitude', metaDataName): lon.astype('f4'),
        ('dateTime', metaDataName): seconds.astype('i8'),
        ('sensorChannelNumber', metaDataName): np.asarray(channel_numbers, 'i4'),
        ('satelliteIdentifier', metaDataName): np.full(nlocs, wmo_sat_id, 'i4'),
        ('sensorZenithAngle', metaDataName): sensor_zenith.astype('f4'),
        ('sensorAzimuthAngle', metaDataName): sensor_azimuth.astype('f4'),
        ('sensorViewAngle', metaDataName): sensor_view.astype('f4'),
        ('solarZenithAngle', metaDataName): solar_zenith.astype('f4'),
        ('solarAzimuthAngle', metaDataName): solar_azimuth.astype('f4'),
        ('sensorScanPosition', metaDataName): scan_position.astype('i4'),
    }
    return obs


def get_obs_properties(nlocs, channel_numbers):
    VarDims = {
        'brightnessTemperature': ['Location', 'Channel'],
        'sensorChannelNumber': ['Channel'],
    }
    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    set_obspace_attributes(VarAttrs)
    set_metadata_attributes(VarAttrs)
    VarAttrs[('latitude', metaDataName)]['units'] = 'degrees_north'
    VarAttrs[('longitude', metaDataName)]['units'] = 'degrees_east'
    VarAttrs[('sensorScanPosition', metaDataName)]['long_name'] = 'column of the full-resolution IR grid (1-based)'
    DimDict = {
        'Location': nlocs,
        'Channel': np.asarray(channel_numbers, 'i4'),
    }
    return VarDims, VarAttrs, DimDict


def convert(args, reader, channels, channel_numbers, wmo_sat_ids, GlobalAttrs, ref,
            times_from=None, times_epoch='1970-01-01T00:00:00', north_to_south=True, processing_level='L1B'):
    """
    Convert one full-disk scan.

    Args:
        args: parsed command line (input, output, subsample, max_zenith)
        reader: satpy reader name
        channels: satpy names of the IR channels, in channel-number order
        channel_numbers: sensor channel numbers matching the CRTM coefficient files
        wmo_sat_ids: platform_name -> WMO satellite identifier (Common Code Table C-5)
        GlobalAttrs: global attributes; platform, sensor and time range are added here
        ref: channel whose grid and attributes define the locations
        times_from: optional satpy dataset with the observation time of each pixel
        times_epoch: epoch of times_from when it holds seconds rather than datetimes
        north_to_south: scan direction, used when times_from is not given
        processing_level: product level written to the processingLevel attribute
    """
    scn = load_scene(reader, args.input, channels)
    attrs = scn[ref].attrs
    platform = attrs['platform_name']
    if platform not in wmo_sat_ids:
        raise ValueError(f"unknown platform {platform}; known: {sorted(wmo_sat_ids)}")

    data_slice, rows, cols, lat, lon = sample_grid(scn, ref, args.subsample)
    if times_from:
        scn.load([times_from])
        times = pixel_times(scn, times_from, data_slice, times_epoch)
    else:
        times = line_times(scn, ref, rows, north_to_south)

    # off-disk pixels have no geolocation
    on_disk = np.isfinite(lat) & np.isfinite(lon) & ~np.isnat(times)
    rows, cols, lat, lon, times = rows[on_disk], cols[on_disk], lat[on_disk], lon[on_disk], times[on_disk]

    sat_lon, sat_lat, sat_alt_km = satellite_position(attrs)
    geometry = viewing_geometry(sat_lon, sat_lat, sat_alt_km, times, lat, lon)
    keep = geometry[0] <= args.max_zenith
    geometry = [g[keep] for g in geometry]
    rows, cols, lat, lon, times = rows[keep], cols[keep], lat[keep], lon[keep], times[keep]

    bt, preqc = brightness_temperatures(scn, channels, data_slice)
    bt, preqc = bt[on_disk][keep], preqc[on_disk][keep]
    seconds = np.round((times - np.datetime64('1970-01-01T00:00:00', 'ns')).astype('i8') / 1e9).astype('i8')

    obs = build_obs(lat, lon, seconds, channel_numbers, bt, preqc, geometry, cols + 1, wmo_sat_ids[platform])
    VarDims, VarAttrs, DimDict = get_obs_properties(lat.size, channel_numbers)

    scan_range = [attrs['start_time'].strftime('%Y-%m-%dT%H:%M:%SZ'), attrs['end_time'].strftime('%Y-%m-%dT%H:%M:%SZ')]
    GlobalAttrs['platformCommonName'] = platform
    GlobalAttrs['sensor'] = attrs['sensor'].upper()
    GlobalAttrs['processingLevel'] = processing_level
    GlobalAttrs['datetimeRange'] = np.array(scan_range, dtype=object)
    GlobalAttrs['converter'] = os.path.basename(GlobalAttrs.get('converter', __file__))

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(obs, VarDims, VarAttrs, GlobalAttrs)

    print(f"{GlobalAttrs['sensor']} {platform}: scan {scan_range[0]} to {scan_range[1]}, "
          f"grid {attrs['area'].shape}, subsample {args.subsample}, {lat.size} locations, "
          f"channels {list(channel_numbers)}, {int(np.sum(preqc == 0))} good values -> {args.output}")
