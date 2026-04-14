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

from datetime import datetime, timedelta
import numpy as np
import h5py

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import (
    compute_scan_angle,
    concat_obs_dict,
    epoch,
    ioda_float_type,
    ioda_int_type,
    set_metadata_attributes,
    set_obspace_attributes,
)

# globals
AWS_PFM_WMO_sat_ID = 80

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

GlobalAttrs = {
    "platformCommonName": "AWS",
    "platformLongDescription": "ESA Arctic Weather Satellite L1B Brightness Temperature Data",
    "sensorCentralWavelength": "[50.3, 89, 165.5, 175.31-191.31, 317.15-333.15, 52.61-57.61]",
}

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
    j = 1
    process_aws_metadata(f, obs_data, j)
#   obs_data[('sensorViewAngle', metaDataName)] = np.array(f['sensor_view_angle'][:, :].flatten(), dtype='float32')
#   obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(f), dtype='int64')
#   obs_data[('satelliteAscendingFlag', metaDataName)] = np.array(f['flagAscDesc'][:, :].flatten(), dtype='int32')

    # assign orbit WMO ID to all locations
    obs_data = assign_WMO_ID(obs_data, WMO_sat_ID)

    assign_brightnessTemperature(f, obs_data)

    # apply gross quality control
    apply_gross_qc(obs_data)

    # quality control using data Flag and final check for valid ObsValues for all bands
    obs_key = ('brightnessTemperature', "ObsValue")
    set_flagged_value(nchans, f, obs_key, obs_data, skip=skip)

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


def process_aws_metadata(f, obs_data, j):
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
            data = np.array(dset[:, :, j], dtype='float32').flatten()
            data *= dset.attrs.get('scale_factor', 1.0)
            data += dset.attrs.get('add_offset', 0.0)
            obs_data[(ioda_name, metaDataName)] = data
        else:
            print(f"Warning: {path} not found in file.")


def apply_gross_qc(obs_data):
    # check some global satellite geometry will compress all data using this
    chk_geolocation = (obs_data[('latitude', metaDataName)] > 90) | (obs_data[('latitude', metaDataName)] < -90) | \
        (obs_data[('longitude', metaDataName)] > 180) | (obs_data[('longitude', metaDataName)] < -180) | \
        (obs_data[('sensorZenithAngle', metaDataName)] > 80) | (obs_data[('sensorZenithAngle', metaDataName)] < 0)

    obs_data[('latitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('longitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('sensorZenithAngle', metaDataName)][chk_geolocation] = float_missing_value


def set_flagged_value(nchans, f, obs_key, obs_data, skip=1):
    for jchan in np.arange(nchans):
        chk_ob = ( obs_data[('latitude', metaDataName)][:] == float_missing_value )
        obs_data[obs_key][:, jchan][chk_ob] = float_missing_value

    tb_key = 'brightnessTemperature'
    good = (obs_data[(tb_key, obsValName)][:, 0] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 8] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 11] != float_missing_value)
    for k in obs_data:
        if metaDataName in k[1] and 'sensorChannelNumber' not in k[0]:
            obs_data[k] = obs_data[k][good][::skip]
        elif tb_key in k[0]:
            obs_data[k] = obs_data[k][good, :][::skip]


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
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDTHHMMSSZ",
        help="base dateTime for observation window",
        type=str, required=False, default=None)

    args = parser.parse_args()

    GlobalAttrs['converter'] = os.path.basename(__file__)

#   obs = variables_to_obs(obs_scene, ancillary_data, VarDims)
    obs = get_aws_data(args.input)
#   VarDims, VarAttrs, DimDict = get_obs_properties(obs_scene)

    # setup the IODA writer
#   writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    # write everything out
#   writer.BuildIoda(obs, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
