#!/usr/bin/python

"""
Python code to ingest netCDF4 or HDF5 ATMS data
"""

import argparse
from datetime import datetime, timedelta
import glob
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
import os.path
from os import getcwd
import sys

import h5py
import numpy as np

float_missing_value = 9.96921e+36
int_missing_value = -2147483647

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

# globals
TROPICS01_WMO_sat_ID = 691
TROPICS02_WMO_sat_ID = 895
TROPICS03_WMO_sat_ID = 896
TROPICS04_WMO_sat_ID = 897
TROPICS05_WMO_sat_ID = 898

GlobalAttrs = {
    "platformCommonName": "TROPICS",
    "platformLongDescription": "TROPICS Brightness Temperature Data",
    "sensorCentralFrequency": "[91.655,  114.50,  115.95,  116.65,  117.25,  117.80,  118.24,  118.58,  184.41,  186.51,  190.31,  204.80]"
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]


def main(args):

    output_filename = args.output
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

# V2 nlocs_int32 = np.array(len(obs_data[('latitude', 'MetaData')]), dtype='int32')
    nlocs_int32 = np.array(len(obs_data[('latitude', 'MetaData')]), dtype='float32')  # this is float32 in old convention
    nlocs = nlocs_int32.item()
    nchans = len(obs_data[('channelNumber', 'MetaData')])

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs['date_time_string'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    date_time_int32 = np.array(int(dtg.strftime("%Y%m%d%H")), dtype='int32')
    GlobalAttrs['date_time'] = date_time_int32.item()
    GlobalAttrs['converter'] = os.path.basename(__file__)

    # pass parameters to the IODA writer
# V2     'brightnessTemperature': ['nlocs', 'nchans']
    VarDims = {
        'brightness_temperature': ['nlocs', 'nchans'],
        'channelNumber': ['nchans'],
    }

    DimDict = {
        'nlocs': nlocs,
        'nchans': obs_data[('channelNumber', 'MetaData')],
    }
    writer = iconv.IodaWriter(output_filename, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    k = 'brightnessTemperature'     # V2
    k = 'brightness_temperature'
    VarAttrs[(k, 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[(k, 'ObsValue')]['units'] = 'K'
    VarAttrs[(k, 'ObsError')]['units'] = 'K'
    VarAttrs[(k, 'PreQC')]['units'] = 'unitless'

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_data_from_files(afile):

    # allocate space for output depending on which variables are to be saved
    obs_data = init_obs_loc()

    f = h5py.File(afile, 'r')
    obs_data = get_data(f, obs_data)
    f.close()

    return obs_data


def get_data(f, obs_data):

    WMO_sat_ID = get_WMO_satellite_ID(f.filename)

    nscans = len(f['scans'])
    nbeam_pos = len(f['spots'])
    nchans = len(f['channels'])
    nbands = len(f['bands'])
    # Bands_to_Channel = "Band 1 = Ch 1; Band 2 = Ch 2-4; Band 3 = Ch 5-8; Band 4 = Ch 9-11; Band 5 = Ch 12"
    iband = 0   # at this point arbitrarily select a band
    obs_data[('latitude', 'MetaData')] = np.array(f['losLat_deg'][iband, :, :].flatten(), dtype='float32')
    obs_data[('longitude', 'MetaData')] = np.array(f['losLon_deg'][iband, :, :].flatten(), dtype='float32')
    obs_data[('channelNumber', 'MetaData')] = np.array(np.arange(nchans)+1, dtype='int32')
    k = 'fieldOfViewNumber'     # V2   #### dtype = 'int32'  ####
    k = 'scan_position'
    obs_data[(k, 'MetaData')] = np.tile(np.arange(nbeam_pos, dtype='float32')+1, (nscans, 1)).flatten()
    k = 'solarZenithAngle'      # V2
    k = 'solar_zenith_angle'
    obs_data[(k, 'MetaData')] = np.array(f['losSolZen_deg'][iband, :, :].flatten(), dtype='float32')
    k = 'solarAzimuthAngle'     # V2
    k = 'solar_azimuth_angle'
    obs_data[(k, 'MetaData')] = np.array(f['losSolAzi_deg'][iband, :, :].flatten(), dtype='float32')
    k = 'sensorZenithAngle'     # V2
    k = 'sensor_zenith_angle'
    obs_data[(k, 'MetaData')] = np.array(f['losZen_deg'][iband, :, :].flatten(), dtype='float32')
    k = 'sensorAzimuthAngle'    # V2
    k = 'sensor_azimuth_angle'
    obs_data[(k, 'MetaData')] = np.array(f['losAzi_deg'][iband, :, :].flatten(), dtype='float32')
    k = 'sensor_view_angle'
    obs_data[(k, 'MetaData')] = np.array(f['losScan_deg'][iband, :, :].flatten(), dtype='float32')

    nlocs = len(obs_data[('latitude', 'MetaData')])
    obs_data[('satelliteId', 'MetaData')] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    obs_data[('datetime', 'MetaData')] = np.array(get_string_dtg(f), dtype=object)

    nlocs = len(obs_data[('latitude', 'MetaData')])
    k = 'brightnessTemperature'
    k = 'brightness_temperature'
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
    obs_data[('ascending_flag', 'MetaData')] = np.array(get_normalized_bit(quality_word[:, 0], bit_index=6), dtype='int32')

    # check some global satellite geometry will compress all data using this
    chk_geolocation = (obs_data[('latitude', 'MetaData')] > 90) | (obs_data[('latitude', 'MetaData')] < -90) | \
        (obs_data[('longitude', 'MetaData')] > 180) | (obs_data[('longitude', 'MetaData')] < -180) | \
        (obs_data[('sensor_zenith_angle', 'MetaData')] > 80) | (obs_data[('sensor_zenith_angle', 'MetaData')] < 0)

    obs_data[('latitude', 'MetaData')][chk_geolocation] = float_missing_value
    obs_data[('longitude', 'MetaData')][chk_geolocation] = float_missing_value
    obs_data[('sensor_zenith_angle', 'MetaData')][chk_geolocation] = float_missing_value

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


def concat_obs_dict(obs_data, append_obs_data):
    # For now we are assuming that the obs_data dictionary has the "golden" list
    # of variables. If one is missing from append_obs_data, the obs_data variable
    # will be extended using fill values.
    #
    # Use the first key in the append_obs_data dictionary to determine how
    # long to make the fill value vector.
    append_keys = list(append_obs_data.keys())


def get_WMO_satellite_ID(filename):

    afile = os.path.basename(filename)
    if 'TROPICS01' in afile:
        WMO_sat_ID = TROPICS01_WMO_sat_ID
    elif 'TROPICS02' in afile:
        WMO_sat_ID = TROPICS02_WMO_sat_ID
    elif 'TROPICS03' in afile:
        WMO_sat_ID = TROPICS03_WMO_sat_ID
    elif 'TROPICS04' in afile:
        WMO_sat_ID = TROPICS04_WMO_sat_ID
    elif 'TROPICS05' in afile:
        WMO_sat_ID = TROPICS05_WMO_sat_ID
    else:
        WMO_sat_ID = -1
        print("could not determine satellite from filename: %s" % afile)
        sys.exit()

    return WMO_sat_ID


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
    # V2     ('brightnessTemperature', "ObsValue"): [],
    # V2     ('brightnessTemperature', "ObsError"): [],
    # V2     ('brightnessTemperature', "PreQC"): [],
    # V2     ('fieldOfViewNumber', 'MetaData'): [],
    # V2     ('solarZenithAngle', 'MetaData'): [],
    # V2     ('solarAzimuthAngle', 'MetaData'): [],
    # V2     ('sensorZenithAngle', 'MetaData'): [],
    # V2     ('sensorAzimuthAngle', 'MetaData'): [],
    obs = {
        ('brightness_temperature', "ObsValue"): [],
        ('brightness_temperature', "ObsError"): [],
        ('brightness_temperature', "PreQC"): [],
        ('satelliteId', 'MetaData'): [],
        ('channelNumber', 'MetaData'): [],
        ('latitude', 'MetaData'): [],
        ('longitude', 'MetaData'): [],
        ('datetime', 'MetaData'): [],
        ('scan_position', 'MetaData'): [],
        ('solar_zenith_angle', 'MetaData'): [],
        ('solar_azimuth_angle', 'MetaData'): [],
        ('sensor_zenith_angle', 'MetaData'): [],
        ('sensor_view_angle', 'MetaData'): [],
        ('sensor_azimuth_angle', 'MetaData'): [],
    }

    return obs


def concat_obs_dict(obs_data, append_obs_data):
    # For now we are assuming that the obs_data dictionary has the "golden" list
    # of variables. If one is missing from append_obs_data, a warning will be issued.
    append_keys = list(append_obs_data.keys())
    for gv_key in obs_data.keys():
        if gv_key in append_keys:
            obs_data[gv_key] = np.append(obs_data[gv_key], append_obs_data[gv_key], axis=0)
        else:
            print("WARNING: ", gv_key, " is missing from append_obs_data dictionary")


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
    required.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, required=True)

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

    args = parser.parse_args()

    main(args)
