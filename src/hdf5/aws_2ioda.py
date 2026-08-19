#!/usr/bin/env python3

#
# (C) Copyright 2020-2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# author: Benjamin Ruston
# Use a satpy reader to ingest ESA Arctic Weather Satellite (AWS) data
# and output to the JEDI IODA format
#

from datetime import datetime, timezone
import numpy as np
import h5py
import os
import re

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import (
    compute_scan_angle,
    concat_obs_dict,
    epoch,
    float_missing_value,
    ioda_float_type,
    ioda_int_type,
    int_missing_value,
    long_missing_value,
    metaDataName,
    obsValName,
    set_metadata_attributes,
    set_obspace_attributes,
)

# globals
AWS_PFM_WMO_sat_ID = 80

GlobalAttrs = {
    "platformCommonName": "AWS",
    "platformLongDescription": "ESA Arctic Weather Satellite L1B Brightness Temperature Data",
    "sensorCentralFrequency": [50.3, 52.8, 53.246, 53.596, 54.4,
                               54.94, 55.5, 57.29, 89., 165.5,
                               176.311, 178.811, 180.311, 181.511, 182.311,
                               325.15, 325.15, 325.15, 325.15],
}
GlobalAttrs['converter'] = os.path.basename(__file__)

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]


def get_aws_data(afile, skip=1):

    obs_data = init_obs_loc()
    f = h5py.File(afile, 'r')
    WMO_sat_ID, nscans, nbeam_pos, nchans = get_header_info(f)

    assign_dimension(obs_data, nchans, nscans, nbeam_pos)

    # data is not remapped choose one to approximate all
    iband = 0
    process_aws_metadata(f, obs_data, iband)
    sat_altitude = get_sat_altitude(f, repeat_count=nbeam_pos)
    obs_data[('sensorViewAngle', metaDataName)] = compute_scan_angle(
        sat_altitude,
        obs_data[('sensorZenithAngle', metaDataName)])
    obs_data[('dateTime', metaDataName)] = get_epoch_time(f, repeat_count=nbeam_pos)
    obs_data[('satelliteAscendingFlag', metaDataName)] = get_iasc(f, repeat_count=nbeam_pos)

    # assign orbit WMO ID to all locations
    obs_data = assign_WMO_ID(obs_data, WMO_sat_ID)

    assign_brightnessTemperature(f, obs_data)

    # apply gross quality control
    chk_geolocation = apply_gross_qc(obs_data)

    # quality control using data Flag and final check for valid ObsValues for all bands
    obs_key = ('brightnessTemperature', "ObsValue")
    set_flagged_value(f, obs_key, obs_data, chk_geolocation, skip=skip)

    return obs_data


def assign_brightnessTemperature(f, obs_data):
    # get ObsValue assign an error and PreQC
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])
    raw_data = np.array(f['data/calibration/aws_toa_brightness_temperature'], dtype='float32').reshape(-1, nchans)
    scale = f['data/calibration/aws_toa_brightness_temperature'].attrs.get('scale_factor', 1.0)
    offset = f['data/calibration/aws_toa_brightness_temperature'].attrs.get('add_offset', 0.0)
    fill = f['data/calibration/aws_toa_brightness_temperature'].attrs.get('missing_value', float_missing_value)
    mask = (raw_data == fill)
    raw_data *= scale
    raw_data += offset
    raw_data[mask] = float_missing_value
    k = 'brightnessTemperature'
    obs_data[(k, "ObsValue")] = raw_data
    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[(k, "ObsError")] = np.full((nlocs, nchans), 5.0, dtype='float32')
    obs_data[(k, "PreQC")] = np.full((nlocs, nchans), 0, dtype='int32')


def get_epoch_time(f, repeat_count=145):
    """
    using the specific key for EUMETSAT AWS files and its attribute
    transform the time to IODA epoch
    only one time per scan line repeat for each FOV
    """
    timekey = 'data/navigation/time_startscan_utc_earthview'
    default_epoch = b'seconds since 2020-01-01T00:00:00.00'
    time_attribute = f[timekey].attrs.get('units', default_epoch).decode('utf-8')
    match = re.search(r'since (.*)', time_attribute)
    if not match:
        raise ValueError(f"Could not determine IODA epoch from: {timekey=}")
    date_str = match.group(1)  # Extracted date string
    # Convert the extracted date to a datetime object
    iet_epoch = datetime.fromisoformat(date_str)
    iet_epoch = iet_epoch.replace(tzinfo=timezone.utc)
    offset = (iet_epoch - epoch).total_seconds()  # Offset in seconds
    raw_time = f[timekey][:].astype(np.float64)
    # Convert IET to Unix time
    ioda_dateTime = raw_time + offset
    ioda_dateTime = np.repeat(ioda_dateTime, repeat_count).astype(np.int64)

    return ioda_dateTime


def get_sat_altitude(f, repeat_count=145):
    """
    Extracts, masks, scales, and repeats satelite altitude
    """
    ds_key = 'data/navigation/satellite_altitude'
    dataset = f[ds_key]
    scale = dataset.attrs.get('scale_factor', [1.0])[0]
    offset = dataset.attrs.get('add_offset', [0.0])[0]
    v_min = dataset.attrs.get('valid_min', [None])[0]
    v_max = dataset.attrs.get('valid_max', [None])[0]

    data = dataset[:].astype(np.float32)

    # catch anything outside valid range (missing value incorrect as 0. is used)
    mask = np.zeros(data.shape, dtype=bool)
    if v_min is not None:
        mask |= (data < v_min)
    if v_max is not None:
        mask |= (data > v_max)

    # scale and convert from km to m (IODA convention)
    processed_data = (data * scale * 1000.) + offset
    processed_data[mask] = float_missing_value

    return np.repeat(processed_data, repeat_count)


def init_obs_loc():
    obs = {
        ('brightnessTemperature', "ObsValue"): [],
        ('brightnessTemperature', "ObsError"): [],
        ('brightnessTemperature', "PreQC"): [],
        ('satelliteIdentifier', metaDataName): [],
        ('sensorChannelNumber', metaDataName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('sensorScanPosition', metaDataName): [],
        ('solarZenithAngle', metaDataName): [],
        ('solarAzimuthAngle', metaDataName): [],
        ('sensorZenithAngle', metaDataName): [],
        ('sensorAzimuthAngle', metaDataName): [],
        ('sensorViewAngle', metaDataName): [],
        ('satelliteAscendingFlag', metaDataName): [],
    }

    return obs


def get_header_info(f):

    WMO_sat_ID = AWS_PFM_WMO_sat_ID
    nscans = len(f['data']['n_scans'])
    nbeam_pos = len(f['data']['n_fovs'])
    nchans = len(f['data']['n_channels'])
    return WMO_sat_ID, nscans, nbeam_pos, nchans


def assign_dimension(obs_data, nchans, nscans, nbeam_pos):
    obs_data[('sensorChannelNumber', metaDataName)] = np.array(np.arange(nchans)+1, dtype='int32')
    values = np.arange(nbeam_pos, dtype='int32')+1
    obs_data[('sensorScanPosition', metaDataName)] = np.tile(values, (nscans, 1)).flatten()


def assign_WMO_ID(obs_data, WMO_sat_ID):
    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    return obs_data


def process_aws_metadata(f, obs_data, iband):
    # populate some metaData from the file
    mapping = {
        'data/navigation/aws_lat': 'latitude',
        'data/navigation/aws_lon': 'longitude',
        'data/navigation/aws_solar_zenith_angle': 'solarZenithAngle',
        'data/navigation/aws_solar_azimuth_angle': 'solarAzimuthAngle',
        'data/navigation/aws_satellite_zenith_angle': 'sensorZenithAngle',
        'data/navigation/aws_satellite_azimuth_angle': 'sensorAzimuthAngle'
    }
#       'data/navigation/orbit_angle': 'orbitAngle'
    for path, ioda_name in mapping.items():
        if path in f:
            dset = f[path]
            data = np.array(dset[:, :, iband], dtype='float32').flatten()
            data *= dset.attrs.get('scale_factor', 1.0)
            data += dset.attrs.get('add_offset', 0.0)
            obs_data[(ioda_name, metaDataName)] = data
        else:
            print(f"Warning: {path} not found in file.")


def get_iasc(f, repeat_count=145):
    """
    retrieve orbit_angle and use to define ascending and descending
    """
    dataset = f['data/navigation/orbit_angle']
    angles = dataset[:]

    # Determine flags: 1 for Ascending (0-180), 0 for Descending (>180)
    flags = np.where((angles >= 0) & (angles <= 180), 1, 0)

    return np.repeat(flags, repeat_count).astype('int32')


def apply_gross_qc(obs_data):
    # check some global satellite geometry will compress all data using this
    chk_geolocation = (obs_data[('latitude', metaDataName)] > 90) | (obs_data[('latitude', metaDataName)] < -90) | \
        (obs_data[('longitude', metaDataName)] > 180) | (obs_data[('longitude', metaDataName)] < -180) | \
        (obs_data[('sensorZenithAngle', metaDataName)] < 0) | (obs_data[('sensorZenithAngle', metaDataName)] > 360)

    obs_data[('latitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('longitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('sensorZenithAngle', metaDataName)][chk_geolocation] = float_missing_value

    return chk_geolocation


def set_flagged_value(f, obs_key, obs_data, chk_geolocation, skip=1):
    """
    Use the 'aws_brightnesstemp_flag' [0: invalid, 1: valid]
    however this flag was showing all data as invalid -- needs to be confirmed
    """
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])

    # apply AWS data processing flag
    k_flag = 'data/processing_information/aws_brightnesstemp_flag'
    flags = f['data']['processing_information']['aws_brightnesstemp_flag'][:].reshape(-1, nchans)
    # this was reporting all data as 0: invalid
#   invalid_mask = (flags != 1)
#   obs_data[obs_key][invalid_mask] = float_missing_value
#   print(f"{np.max(obs_data[('brightnessTemperature', 'ObsValue')][:, 2])=}")

    # apply geolocation physical reality check
    for jchan in np.arange(nchans):
        obs_data[obs_key][chk_geolocation, jchan] = float_missing_value

    tb_key = 'brightnessTemperature'
    target_channels = [2, 8, 11]  # check a single V-, W-, G-band channel
#   target_channels = [2, 8, 11, 16]  # check a single V-, W-, G-, and Y-band channel
    good = (obs_data[(tb_key, obsValName)][:, target_channels] != float_missing_value).all(axis=1)
    if skip > 1:
        mask_skip = (np.arange(len(good)) % skip == 0)
        good = good & mask_skip
    for k in obs_data:
        if metaDataName in k[1] and 'sensorChannelNumber' not in k[0]:
            obs_data[k] = obs_data[k][good]
        elif tb_key in k[0]:
            obs_data[k] = obs_data[k][good, :]


def get_obs_properties(obs_data):
    """
    set obs_data Attributes and Dimensions
    """

    # pass parameters to the IODA writer
    VarDims = {
        'brightnessTemperature': ['Location', 'Channel'],
        'sensorChannelNumber': ['Channel'],
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

    return VarDims, VarAttrs, DimDict


def main():

    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
    import os
    desc = 'Convert AWS L1B into IODA convention use a netCDF4 backend'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="full path name of satellite observation input file",
        type=str, required=True, default=None)
    required.add_argument(
        '-o', '--output',
        help='name of the output netCDF IODA-compliant file',
        type=str, required=True, default='output.nc')

    args = parser.parse_args()

    obs = get_aws_data(args.input)
    VarDims, VarAttrs, DimDict = get_obs_properties(obs)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    # write everything out
    writer.BuildIoda(obs, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
