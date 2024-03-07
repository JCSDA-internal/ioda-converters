#!/usr/bin/env python3

#
# (C) Copyright 2020-2023 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#


"""
Python code to ingest netCDF4 TROPICS data
"""

import argparse
from datetime import datetime
import os.path
import sys

import h5py
import numpy as np

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import set_metadata_attributes, set_obspace_attributes
from pyiodaconv.def_jedi_utils import compute_scan_angle
from pyiodaconv.def_jedi_utils import ioda_int_type, ioda_float_type, epoch
from pyiodaconv.def_jedi_utils import concat_obs_dict

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

# globals
TROPICS01_WMO_sat_ID = 933  # 691  did this ID get updated by WMO
TROPICS02_WMO_sat_ID = 895
TROPICS03_WMO_sat_ID = 896
TROPICS04_WMO_sat_ID = 966
TROPICS05_WMO_sat_ID = 967
TROPICS06_WMO_sat_ID = 968
TROPICS07_WMO_sat_ID = 969
# what to use here
TOMORROWIO_S3_sat_ID = 1333
TOMORROWIO_S1_sat_ID = 1333
TOMORROWIO_GENERIC_sat_ID = 1333

# TROPICS Epoch Time (TET) offset
tet_offset = 946684721.0

GlobalAttrs = {
    "platformCommonName": "TROPICS",
    "platformLongDescription": "TROPICS Brightness Temperature Data",
    "sensorCentralFrequency": "[91.655,  114.50,  115.95,  116.65,  117.25,  117.80,  118.24,  118.58,  184.41,  186.51,  190.31,  204.80]"
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]


def main(args):

    output_filename = args.output
    dtg = None
    if args.date:
        dtg = datetime.strptime(args.date, '%Y%m%d%H')

    input_files = [(i) for i in args.input]
    # initialize
    obs_data = {}
    # read / process files in parallel
#   with ProcessPoolExecutor(max_workers=args.threads) as executor:
#       for file_obs_data in executor.map(get_data_from_files, input_files):
#           if not file_obs_data:
#               print("INFO: non-nominal file skipping")
#               continue
#           if obs_data:
#               concat_obs_dict(obs_data, file_obs_data)
#           else:
#               obs_data = file_obs_data

    for afile in input_files:
        file_obs_data = get_data_from_files(afile)
        if not file_obs_data:
            print("INFO: non-nominal file skipping")
            continue
        if obs_data:
            concat_obs_dict(obs_data, file_obs_data)
        else:
            obs_data = file_obs_data

    nlocs_int = np.array(len(obs_data[('latitude', metaDataName)]), dtype='int64')
    nlocs = nlocs_int.item()
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    if dtg:
        GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    GlobalAttrs['converter'] = os.path.basename(__file__)

    # pass parameters to the IODA writer
    VarDims = {
        'brightnessTemperature': ['Location', 'Channel'],
        'sensorChannelNumber': ['Channel'],
    }

    DimDict = {
        'Location': nlocs,
        'Channel': obs_data[('sensorChannelNumber', metaDataName)],
    }
    writer = iconv.IodaWriter(output_filename, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    set_obspace_attributes(VarAttrs)
    set_metadata_attributes(VarAttrs)

    k = 'brightnessTemperature'
    VarAttrs[(k, 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[(k, 'ObsValue')]['units'] = 'K'
    VarAttrs[(k, 'ObsError')]['units'] = 'K'
    # VarAttrs[(k, 'PreQC')]['units'] = 'unitless'

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_data_from_files(afile):

    # allocate space for output depending on which variables are to be saved
    obs_data = init_obs_loc()

    f = h5py.File(afile, 'r')
    software_version = True
    if 'L1b_SW_Ver' in f.attrs.keys():
        software_version = int(f.attrs['L1b_SW_Ver'].decode("utf-8").split('.')[0]) >= 3
    nc_properties = True
    if '_NCProperties' in f.attrs.keys():
        nc_properties = 'version=2' in f.attrs['_NCProperties'].decode("utf-8")
    if software_version or nc_properties:
        obs_data = get_data(f, obs_data)
    elif int(f.attrs['L1b_SW_Ver'].decode("utf-8").split('.')[0]) == 2:
        obs_data = get_data_deprecated(f, obs_data)
    else:
        print(f'unknown software version: {software_version}')
        obs_data = None
    f.close()

    return obs_data


def get_data(f, obs_data):

    WMO_sat_ID = get_WMO_satellite_ID(f.attrs['ShortName'].decode("utf-8"))

    nscans = len(f['scans'])
    nbeam_pos = len(f['spots'])
    nchans = len(f['channels'])
    nbands = len(f['bands'])
    # Bands_to_Channel = "Band 1 = Ch 1; Band 2 = Ch 2-4; Band 3 = Ch 5-8; Band 4 = Ch 9-11; Band 5 = Ch 12"
    iband = 0   # at this point arbitrarily select a band
    obs_data[('latitude', metaDataName)] = np.array(f['latitude'][iband, :, :].flatten(), dtype='float32')
    obs_data[('longitude', metaDataName)] = np.array(f['longitude'][iband, :, :].flatten(), dtype='float32')
    obs_data[('sensorChannelNumber', metaDataName)] = np.array(np.arange(nchans)+1, dtype='int32')
    obs_data[('sensorScanPosition', metaDataName)] = np.tile(np.arange(nbeam_pos, dtype='int32')+1, (nscans, 1)).flatten()
    obs_data[('solarZenithAngle', metaDataName)] = np.array(f['solar_zenith_angle'][iband, :, :].flatten(), dtype='float32')
    obs_data[('solarAzimuthAngle', metaDataName)] = np.array(f['solar_azimuth_angle'][iband, :, :].flatten(), dtype='float32')
    obs_data[('sensorZenithAngle', metaDataName)] = np.array(f['sensor_zenith_angle'][iband, :, :].flatten(), dtype='float32')
    obs_data[('sensorAzimuthAngle', metaDataName)] = np.array(f['sensor_azimuth_angle'][iband, :, :].flatten(), dtype='float32')
    obs_data[('sensorViewAngle', metaDataName)] = np.array(f['sensor_view_angle'][iband, :, :].flatten(), dtype='float32')

    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    # obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(f), dtype='int64')
    obs_data[('dateTime', metaDataName)] = np.array(f['time'][:, :].flatten() + tet_offset, dtype='int64')

    nlocs = len(obs_data[('latitude', metaDataName)])
    k = 'brightnessTemperature'
    # have to reorder the channel axis to be last then merge ( nscans x nspots = nlocs )
    obs_data[(k, "ObsValue")] = np.array(np.vstack(np.stack(
        np.where(f['brightness_temperature'] == f['brightness_temperature'].fillvalue,
                 float_missing_value, f['brightness_temperature']), axis=2)), dtype='float32')
    obs_data[(k, "ObsError")] = np.full((nlocs, nchans), 5.0, dtype='float32')
    obs_data[(k, "PreQC")] = np.full((nlocs, nchans), 0, dtype='int32')

    # Bit 1: Non-ocean
    # Bit 2: Lunar/solar intrusion
    # Bit 3: Spacecraft Maneuver
    # Bit 4: Cold Cal. Inconsistency
    # Bit 5: Hot Cal. Inconsistency
    # Bit 6: Descending Orbit
    # Bit 7: Night
    # Bit 8: Payload rear orientation'
    quality_word = np.vstack(np.stack(f['calQualityFlag'], axis=2))
    obs_data[('satelliteAscendingFlag', metaDataName)] = np.array(get_normalized_bit(quality_word[:, 0], bit_index=6), dtype='int32')

    # check some global satellite geometry will compress all data using this
    chk_geolocation = (obs_data[('latitude', metaDataName)] > 90) | (obs_data[('latitude', metaDataName)] < -90) | \
        (obs_data[('longitude', metaDataName)] > 180) | (obs_data[('longitude', metaDataName)] < -180) | \
        (obs_data[('sensorZenithAngle', metaDataName)] > 80) | (obs_data[('sensorZenithAngle', metaDataName)] < 0)

    obs_data[('latitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('longitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('sensorZenithAngle', metaDataName)][chk_geolocation] = float_missing_value

    obs_key = (k, "ObsValue")
    obs_data = set_missing_value(nchans, chk_geolocation, quality_word, obs_key, obs_data)

    return obs_data


def get_data_deprecated(f, obs_data):

    WMO_sat_ID = get_WMO_satellite_ID(f.attrs['ShortName'].decode("utf-8"))

    nscans = len(f['scans'])
    nbeam_pos = len(f['spots'])
    nchans = len(f['channels'])
    nbands = len(f['bands'])
    # Bands_to_Channel = "Band 1 = Ch 1; Band 2 = Ch 2-4; Band 3 = Ch 5-8; Band 4 = Ch 9-11; Band 5 = Ch 12"
    iband = 0   # at this point arbitrarily select a band
    obs_data[('latitude', metaDataName)] = np.array(f['losLat_deg'][iband, :, :].flatten(), dtype='float32')
    obs_data[('longitude', metaDataName)] = np.array(f['losLon_deg'][iband, :, :].flatten(), dtype='float32')
    obs_data[('sensorChannelNumber', metaDataName)] = np.array(np.arange(nchans)+1, dtype='int32')
    k = 'sensorScanPosition'
    obs_data[(k, metaDataName)] = np.tile(np.arange(nbeam_pos, dtype='int32')+1, (nscans, 1)).flatten()
    k = 'solarZenithAngle'
    obs_data[(k, metaDataName)] = np.array(f['losSolZen_deg'][iband, :, :].flatten(), dtype='float32')
    k = 'solarAzimuthAngle'
    obs_data[(k, metaDataName)] = np.array(f['losSolAzi_deg'][iband, :, :].flatten(), dtype='float32')
    k = 'sensorZenithAngle'
    obs_data[(k, metaDataName)] = np.array(f['losZen_deg'][iband, :, :].flatten(), dtype='float32')
    k = 'sensorAzimuthAngle'
    obs_data[(k, metaDataName)] = np.array(f['losAzi_deg'][iband, :, :].flatten(), dtype='float32')
    instr_scan_ang = np.array(f['losScan_deg'][iband, :, :].flatten(), dtype='float32')
    # compute view angle
    sat_altitude = np.empty_like(instr_scan_ang)
    sat_altitude[:] = 550.
    obs_data[('sensorViewAngle', metaDataName)] = compute_scan_angle(
        instr_scan_ang,
        sat_altitude,
        instr_scan_ang)

    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    # obs_data[('datetime', metaDataName)] = np.array(get_string_dtg(f), dtype=object)
    obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(f), dtype='int64')

    nlocs = len(obs_data[('latitude', metaDataName)])
    k = 'brightnessTemperature'
    # have to reorder the channel axis to be last then merge ( nscans x nspots = nlocs )
    obs_data[(k, "ObsValue")] = np.array(np.vstack(np.stack(
        np.where(f['tempBrightE_K'] == f['tempBrightE_K'].fillvalue, float_missing_value, f['tempBrightE_K']), axis=2)), dtype='float32')
    obs_data[(k, "ObsError")] = np.full((nlocs, nchans), 5.0, dtype='float32')
    obs_data[(k, "PreQC")] = np.full((nlocs, nchans), 0, dtype='int32')

    # Bit 1: land/undefined
    # Bit 2: Lunar/solar intrusion
    # Bit 3: Active Maneuver
    # Bit 4: Cold Cal. Consistency
    # Bit 5: Hot Cal. Consistency
    # Bit 6: Ascending/Descending
    # Bit 7: Day/Night
    # Bit 8: Payload forward/aft"
    quality_word = np.vstack(np.stack(f['calQualityFlag'], axis=2))
    obs_data[('satelliteAscendingFlag', metaDataName)] = np.array(get_normalized_bit(quality_word[:, 0], bit_index=6), dtype='int32')

    # check some global satellite geometry will compress all data using this
    chk_geolocation = (obs_data[('latitude', metaDataName)] > 90) | (obs_data[('latitude', metaDataName)] < -90) | \
        (obs_data[('longitude', metaDataName)] > 180) | (obs_data[('longitude', metaDataName)] < -180) | \
        (obs_data[('sensorZenithAngle', metaDataName)] > 80) | (obs_data[('sensorZenithAngle', metaDataName)] < 0)

    obs_data[('latitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('longitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('sensorZenithAngle', metaDataName)][chk_geolocation] = float_missing_value

    obs_key = (k, "ObsValue")
    obs_data = set_missing_value(nchans, chk_geolocation, quality_word, obs_key, obs_data)

    return obs_data


def set_missing_value(nchans, chk_geolocation, quality_word, obs_key, obs_data):
    # use quality word to determine where to set for missing values
    for jchan in np.arange(nchans):
        i_land = get_normalized_bit(quality_word[:, jchan], bit_index=1)
        i_intrusion = get_normalized_bit(quality_word[:, jchan], bit_index=2)
        i_maneuver = get_normalized_bit(quality_word[:, jchan], bit_index=3)
        i_cold_cal = get_normalized_bit(quality_word[:, jchan], bit_index=4)
        i_hot_cal = get_normalized_bit(quality_word[:, jchan], bit_index=5)
        i_asc = get_normalized_bit(quality_word[:, jchan], bit_index=6)
        i_day = get_normalized_bit(quality_word[:, jchan], bit_index=7)
        i_forward = get_normalized_bit(quality_word[:, jchan], bit_index=8)
        chk_ob = (i_cold_cal + i_hot_cal + i_intrusion + i_maneuver + chk_geolocation) > 0
        obs_data[obs_key][:, jchan][chk_ob] = float_missing_value

    tb_key = 'brightnessTemperature'
    good = (obs_data[(tb_key, obsValName)][:, 0] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 1] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 8] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 11] != float_missing_value)
    for k in obs_data:
        if metaDataName in k[1] and 'sensorChannelNumber' not in k[0]:
            obs_data[k] = obs_data[k][good]     # [::24] ## add as skip
        elif tb_key in k[0]:
            obs_data[k] = obs_data[k][good, :]  # [::24] ## add as skip

    return obs_data


def get_normalized_bit(value, bit_index):
    return (value >> bit_index) & 1


def assign_values(data):
    if data.dtype == float:
        data[np.abs(data) >= np.abs(float_missing_value)] = float_missing_value
        return np.array(data, dtype=ioda_float_type)
    elif data.dtype == int:
        data[np.abs(data) >= np.abs(int_missing_value)] = int_missing_value
        return np.array(data, dtype=ioda_int_type)


def get_WMO_satellite_ID(attrs_shortname):

    if 'TROPICS01' in attrs_shortname:
        WMO_sat_ID = TROPICS01_WMO_sat_ID
    elif 'TROPICS02' in attrs_shortname:
        WMO_sat_ID = TROPICS02_WMO_sat_ID
    elif 'TROPICS03' in attrs_shortname:
        WMO_sat_ID = TROPICS03_WMO_sat_ID
    elif 'TROPICS04' in attrs_shortname:
        WMO_sat_ID = TROPICS04_WMO_sat_ID
    elif 'TROPICS05' in attrs_shortname:
        WMO_sat_ID = TROPICS05_WMO_sat_ID
    elif 'TROPICS06' in attrs_shortname:
        WMO_sat_ID = TROPICS06_WMO_sat_ID
    elif 'TROPICS07' in attrs_shortname:
        WMO_sat_ID = TROPICS07_WMO_sat_ID
    elif 'TMS_S3_BRTTL1B' in attrs_shortname:
        WMO_sat_ID = TOMORROWIO_S3_sat_ID
    elif 'TMS_S1_BRTTL1B' in attrs_shortname:
        WMO_sat_ID = TOMORROWIO_S1_sat_ID
    elif 'TMS' in attrs_shortname and 'BRTTL1B' in attrs_shortname:
        WMO_sat_ID = TOMORROWIO_GENERIC_sat_ID
    else:
        WMO_sat_ID = -1
        print("could not determine satellite from filename: %s" % attrs_shortname)
        sys.exit()

    return WMO_sat_ID


def get_epoch_time(f):

    nbeam_pos = len(f['spots'])
    # ugh this does not seem generic
    year = f['Year']
    month = f['Month']
    day = f['Day']
    hour = f['Hour']
    minute = f['Minute']
    second = np.zeros_like(minute)

    # following examples here could be written better potentially
    iterables = [year, month, day, hour, minute, second]
    # ensure the year is plausible (65535 appears in some data) if not set to 01Jan1900 (revisit)
    this_datetime = [datetime(adate[0], adate[1], adate[2], adate[3], adate[4], adate[5])
                     if adate[0] < 2200 else datetime(2200, 1, 1, 0, 0, 0)
                     for adate in zip(*iterables)]

    time_offset_short = [round((adatetime - epoch).total_seconds()) for adatetime in this_datetime]
    time_offset = []
    for adate in time_offset_short:
        # need to add replication by nbeam_pos
        for _ in range(nbeam_pos):
            time_offset.append(adate)

    return time_offset


def get_string_dtg(f):

    # for TROPICS data times are per scan line
    # current IODA needs replication by beam position
    nbeam_pos = len(f['spots'])
    year = f['Year']
    month = f['Month']
    day = f['Day']
    hour = f['Hour']
    minute = f['Minute']
    dtg = []
    for i, yyyy in enumerate(year):
        cdtg = ("%4i-%.2i-%.2iT%.2i:%.2i:00Z" % (yyyy, month[i], day[i], hour[i], minute[i]))
        # need to add replication by nbeam_pos
        for _ in range(nbeam_pos):
            dtg.append(cdtg)

    return dtg


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


if __name__ == "__main__":

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
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             ' (default: %(default)s)',
        type=int, default=1)
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
