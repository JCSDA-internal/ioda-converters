#!/usr/bin/env python3

#
# (C) Copyright 2020-2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# author: Benjamin Ruston
# Ingest Spire Hyperspectral Microwave Sounder (HyMS) L1B netCDF4 data
# and output to the JEDI IODA format
#
# HyMS has two hyperspectral bands (~50-58 GHz and ~183-192 GHz) and a
# single imaging channel at 89 GHz.  All three bands share a common
# geolocation in the L1B file, so each pixel is one IODA Location and the
# channels are ordered by frequency: band_54, band_89, band_183
#

import argparse
from datetime import datetime, timedelta, timezone
import os.path

import h5py
import numpy as np

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import (
    float_missing_value,
    int_missing_value,
    metaDataName,
    obsValName,
    set_metadata_attributes,
    set_obspace_attributes,
)

# globals
HyMS_WMO_sat_ID = 999     # unknown -- no WMO satellite ID assigned yet

# bands in order of increasing frequency, defines the channel ordering
BANDS = ['54', '89', '183']
# reference band for geolocation and geometry (identical for all bands in L1B)
GEO_BAND = '54'

DEFAULT_OBS_ERROR = 5.0

GlobalAttrs = {
    "platformCommonName": "HyMS",
    "platformLongDescription": "Spire Hyperspectral Microwave Sounder L1B Brightness Temperature Data",
}
GlobalAttrs['converter'] = os.path.basename(__file__)

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

# per-pixel flags that indicate a degraded calibration or geolocation
PIXEL_FLAG_KEYS = [
    'cold_calibration_inconsistency',
    'hot_calibration_inconsistency',
    'lunar_solar_intrusion',
    'spacecraft_maneuver',
]


def main(args):

    dtg = None
    if args.date:
        dtg = datetime.strptime(args.date, '%Y%m%dT%H%M%SZ' if 'T' in args.date else '%Y%m%d%H')

    # optional time window (at this time symmetrically centered, begin <= dateTime < end)
    window = None
    if args.window_minutes:
        if not dtg:
            raise ValueError("--window_minutes requires -d/--date for the center of the window")
        center = dtg.replace(tzinfo=timezone.utc)
        half = timedelta(minutes=args.window_minutes / 2.)
        window = (int((center - half).timestamp()), int((center + half).timestamp()))
        print(f"keeping data from {center - half:%Y-%m-%dT%H:%M:%SZ} up to {center + half:%Y-%m-%dT%H:%M:%SZ}")

    obs_data = {}
    for afile in args.input:
        file_obs_data = get_hyms_data(afile, skip=args.skip, window=window)
        if not file_obs_data:
            print(f"INFO: no valid data in {afile} skipping")
            continue
        if obs_data:
            concat_hyms_obs(obs_data, file_obs_data)
        else:
            obs_data = file_obs_data

    if not obs_data:
        print('  ...  WARNING: no data found exiting without writing output')
        return

    if dtg:
        GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")

    VarDims, VarAttrs, DimDict = get_obs_properties(obs_data)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    # write everything out
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_hyms_data(afile, skip=1, window=None):

    obs_data = init_obs_loc()
    with h5py.File(afile, 'r') as f:
        GlobalAttrs['sensorFlightModel'] = f.attrs.get('FM', b'unknown').decode('utf-8')

        assign_channels(f, obs_data)
        process_hyms_metadata(f, obs_data)
        assign_brightnessTemperature(f, obs_data)

        # apply gross quality control
        chk_geolocation = apply_gross_qc(obs_data)

        # set PreQC from the pixel level flags and remove unusable locations
        set_flagged_value(f, obs_data, chk_geolocation, skip=skip, window=window)

    if len(obs_data[('latitude', metaDataName)]) == 0:
        return {}

    return obs_data


def assign_channels(f, obs_data):
    """
    channel numbers run 1..nchans across all bands, ordered band_54, band_89, band_183
    """
    freqs = [np.atleast_1d(f[f'freq_band_{b}'][()]) for b in BANDS]
    freq_ghz = np.concatenate(freqs)
    nchans = len(freq_ghz)
    obs_data[('sensorChannelNumber', metaDataName)] = np.arange(1, nchans+1, dtype='int32')
    # IODA convention is Hz
    obs_data[('sensorCentralFrequency', metaDataName)] = np.array(freq_ghz * 1.e9, dtype='float32')


def process_hyms_metadata(f, obs_data):
    # populate metaData from the file, geolocation is the same for every band
    mapping = {
        f'pixel_latitude_band_{GEO_BAND}': 'latitude',
        f'pixel_longitude_band_{GEO_BAND}': 'longitude',
        f'pixel_solar_zenith_band_{GEO_BAND}': 'solarZenithAngle',
        f'pixel_solar_azimuth_band_{GEO_BAND}': 'solarAzimuthAngle',
        f'pixel_incidence_band_{GEO_BAND}': 'sensorZenithAngle',
        f'pixel_azimuth_band_{GEO_BAND}': 'sensorAzimuthAngle',
        f'pixel_look_band_{GEO_BAND}': 'sensorViewAngle',
    }
    for path, ioda_name in mapping.items():
        data = np.array(f[path][:], dtype='float32')
        data[np.isnan(data)] = float_missing_value
        obs_data[(ioda_name, metaDataName)] = data

    lon = obs_data[('longitude', metaDataName)]
    lon[(lon != float_missing_value) & (lon > 180.)] -= 360.
    # azimuth is provided on [-180, 180], IODA convention is [0, 360)
    azi = obs_data[('sensorAzimuthAngle', metaDataName)]
    azi[azi != float_missing_value] %= 360.

    # 'observation time in seconds since 1970-01-01' which is the IODA epoch
    obs_time = f['time'][:]
    date_time = np.full(obs_time.shape, int_missing_value, dtype='int64')
    valid = np.isfinite(obs_time)
    date_time[valid] = np.round(obs_time[valid]).astype('int64')
    obs_data[('dateTime', metaDataName)] = date_time

    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('sensorScanPosition', metaDataName)] = np.array(f['pixel_id'][:] + 1, dtype='int32')
    obs_data[('scanLineNumber', metaDataName)] = np.array(f['scan_id'][:] + 1, dtype='int32')
    obs_data[('satelliteAscendingFlag', metaDataName)] = np.array(f['ascend_descend'][:], dtype='int32')
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), HyMS_WMO_sat_ID, dtype='int32')


def assign_brightnessTemperature(f, obs_data):
    """
    get ObsValue, use the per-pixel Tb uncertainty (calibration + NEDT) for ObsError
    when available, and remove channels the provider marks as bad
    """
    tbs, errs, good_chans = [], [], []
    for b in BANDS:
        tb = np.array(f[f'tb_band_{b}'][:], dtype='float32')
        err = np.array(f[f'std_tb_band_{b}'][:], dtype='float32')
        quality = np.atleast_1d(f[f'channel_quality_band_{b}'][()])
        if tb.ndim == 1:
            tb = tb[:, np.newaxis]
            err = err[:, np.newaxis]
        tbs.append(tb)
        errs.append(err)
        good_chans.append(quality == 1)

    tb = np.concatenate(tbs, axis=1)
    err = np.concatenate(errs, axis=1)
    good_chans = np.concatenate(good_chans)

    err[~np.isfinite(err) | (err <= 0.)] = DEFAULT_OBS_ERROR
    tb_missing = ~np.isfinite(tb)
    tb_missing[:, ~good_chans] = True
    tb[tb_missing] = float_missing_value
    err[tb_missing] = float_missing_value

    k = 'brightnessTemperature'
    obs_data[(k, "ObsValue")] = tb
    obs_data[(k, "ObsError")] = err
    obs_data[(k, "PreQC")] = np.zeros(tb.shape, dtype='int32')


def init_obs_loc():
    obs = {
        ('brightnessTemperature', "ObsValue"): [],
        ('brightnessTemperature', "ObsError"): [],
        ('brightnessTemperature', "PreQC"): [],
        ('satelliteIdentifier', metaDataName): [],
        ('sensorChannelNumber', metaDataName): [],
        ('sensorCentralFrequency', metaDataName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('sensorScanPosition', metaDataName): [],
        ('scanLineNumber', metaDataName): [],
        ('solarZenithAngle', metaDataName): [],
        ('solarAzimuthAngle', metaDataName): [],
        ('sensorZenithAngle', metaDataName): [],
        ('sensorAzimuthAngle', metaDataName): [],
        ('sensorViewAngle', metaDataName): [],
        ('satelliteAscendingFlag', metaDataName): [],
    }

    return obs


def apply_gross_qc(obs_data):
    # check some global satellite geometry will compress all data using this
    lat = obs_data[('latitude', metaDataName)]
    lon = obs_data[('longitude', metaDataName)]
    zen = obs_data[('sensorZenithAngle', metaDataName)]
    chk_geolocation = (lat > 90) | (lat < -90) | (lon > 180) | (lon < -180) | \
        (zen < 0) | (zen > 80) | \
        (obs_data[('dateTime', metaDataName)] == int_missing_value)

    lat[chk_geolocation] = float_missing_value
    lon[chk_geolocation] = float_missing_value
    zen[chk_geolocation] = float_missing_value

    return chk_geolocation


def set_flagged_value(f, obs_data, chk_geolocation, skip=1, window=None):
    """
    PreQC is set to 1 for pixels with ground processing error bits ('err_bits')
    or calibration / attitude flags raised, the data are retained.
    Locations with bad geolocation or no valid Tb in any channel are removed,
    as are locations outside the optional (begin, end) time window.
    """
    tb_key = 'brightnessTemperature'
    obs_key = (tb_key, obsValName)

    pixel_flag = (f['err_bits'][:] != 0).any(axis=1)
    for key in PIXEL_FLAG_KEYS:
        if key in f:
            pixel_flag |= (f[key][:] != 0)
    obs_data[(tb_key, "PreQC")][pixel_flag, :] = 1

    # apply geolocation physical reality check
    obs_data[obs_key][chk_geolocation, :] = float_missing_value
    obs_data[(tb_key, "ObsError")][chk_geolocation, :] = float_missing_value

    good = (obs_data[obs_key] != float_missing_value).any(axis=1)
    if skip > 1:
        mask_skip = (np.arange(len(good)) % skip == 0)
        good = good & mask_skip
    if window:
        date_time = obs_data[('dateTime', metaDataName)]
        good = good & (date_time >= window[0]) & (date_time < window[1])

    obs_data[(tb_key, "PreQC")][obs_data[obs_key] == float_missing_value] = int_missing_value

    for k in obs_data:
        if k in channel_keys():
            continue
        elif metaDataName in k[1]:
            obs_data[k] = obs_data[k][good]
        elif tb_key in k[0]:
            obs_data[k] = obs_data[k][good, :]


def channel_keys():
    return [('sensorChannelNumber', metaDataName), ('sensorCentralFrequency', metaDataName)]


def concat_hyms_obs(obs_data, append_obs_data):
    # Location dimensioned variables are appended, Channel variables must match
    for k in obs_data:
        if k in channel_keys():
            if not np.array_equal(obs_data[k], append_obs_data[k]):
                raise ValueError(f"channel definition differs between input files: {k}")
        else:
            obs_data[k] = np.concatenate((obs_data[k], append_obs_data[k]), axis=0)


def get_obs_properties(obs_data):
    """
    set obs_data Attributes and Dimensions
    """

    # pass parameters to the IODA writer
    VarDims = {
        'brightnessTemperature': ['Location', 'Channel'],
        'sensorChannelNumber': ['Channel'],
        'sensorCentralFrequency': ['Channel'],
    }

    nlocs = len(obs_data[('latitude', metaDataName)])
    DimDict = {
        'Location': nlocs,
        'Channel': obs_data[('sensorChannelNumber', metaDataName)],
    }

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    set_obspace_attributes(VarAttrs)
    set_metadata_attributes(VarAttrs)

    k = 'brightnessTemperature'
    VarAttrs[(k, 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[(k, 'ObsValue')]['units'] = 'K'
    VarAttrs[(k, 'ObsError')]['units'] = 'K'
    VarAttrs[('sensorCentralFrequency', metaDataName)]['units'] = 'Hz'

    return VarDims, VarAttrs, DimDict


if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description=(
            'Reads Spire HyMS L1B netCDF4 files '
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
        metavar="YYYYMMDDHH|YYYYMMDDTHHMMSSZ",
        help="base date for the center of the window",
        type=str, default=None)
    optional.add_argument(
        '--window_minutes',
        help="keep only data within +/- half this many minutes of the date (default: keep all)",
        type=float, default=None)
    optional.add_argument(
        '--skip',
        help="default pixel skip factor to be applied",
        type=int, default=1)

    args = parser.parse_args()

    main(args)
