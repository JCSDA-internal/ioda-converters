#!/usr/bin/env python3

#
# (C) Copyright 2020-2025 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

"""
Python code to ingest netCDF4 or HDF5 Ocean Surface Wind from COWVR
"""

import argparse
from datetime import datetime, timezone
import glob
# from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
import os.path
from os import getcwd
import sys
import time

import h5py
import numpy as np

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

# globals
ISS_COWVR_WMO_sat_ID = 806

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
    ("sensorIdentification", "string"),
    ("height", "float"),
]

variableKeyList = ["windSpeed", "windDirection", "windEastward", "windNorthward"]

iso8601_string = "seconds since 1970-01-01T00:00:00Z"
epoch = datetime.fromisoformat(iso8601_string[14:-1])


def main(args):

    tic = record_time()

    output_filename = args.output
    dtg = None
    if args.date:
        dtg = datetime.strptime(args.date, '%Y%m%d%H')

    input_files = [(i) for i in args.input]
    # read / process files in parallel
    obs_data = {}
    # create a thread pool
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

    if nlocs == 0:
        print(f" no valid or unflagged data found")
        print(f" ... exiting")
        sys.exit()

    GlobalAttrs = get_global_attributes(obs_data[('satelliteIdentifier', metaDataName)])
    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs['datetimeRange'] = np.array([datetime.fromtimestamp(obs_data[('dateTime', metaDataName)][0], timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
                                            datetime.fromtimestamp(obs_data[('dateTime', metaDataName)][-1], timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")],
                                            dtype=object)
    if dtg:
        GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    GlobalAttrs['converter'] = os.path.basename(__file__)

    # pass parameters to the IODA writer
    VarDims = {}
    for k in variableKeyList:
        VarDims[k] = ['Location']

    DimDict = {
        'Location': nlocs,
    }
    writer = iconv.IodaWriter(output_filename, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('dateTime', metaDataName)]['units'] = iso8601_string
    VarAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value

    for k in variableKeyList:
        if 'Direction' not in k:
            VarAttrs[(k, obsValName)]['units'] = 'm s-1'
            VarAttrs[(k, obsErrName)]['units'] = 'm s-1'
        else:
            VarAttrs[(k, obsValName)]['units'] = 'degree'
            VarAttrs[(k, obsErrName)]['units'] = 'degree'
        VarAttrs[(k, obsValName)]['_FillValue'] = float_missing_value
        VarAttrs[(k, obsErrName)]['_FillValue'] = float_missing_value
        VarAttrs[(k, qcName)]['_FillValue'] = int_missing_value

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)

    # report time
    toc = record_time(tic=tic)


def get_data_from_files(zfiles):

    # allocate space for output depending on which variables are to be saved
    obs_data = init_obs_loc()

    # for afile in zfiles:
    afile = zfiles
    f = h5py.File(afile, 'r')
    sensor_name = f['Metadata']['InstrumentShortName'][0].decode("utf-8")
    if 'COWVR' in sensor_name:
        obs_data = get_osw_cowvr_data(f, obs_data)
    else:
        print(f" unrecognized sensor nothing to write for file: {afile}")
    f.close()

    return obs_data


def get_osw_cowvr_data(f, obs_data):

    WMO_sat_ID = get_WMO_satellite_ID(f['Metadata']['InstrumentShortName'][0].decode("utf-8"))

    # obs is on a grid (e.g. [601, 1801])
    windSpeed = f['EnvDataRecords']['wind_speed'][:]

    # Get the shape of the wind speed data
    rows, cols = windSpeed.shape

    obs_data[('latitude', metaDataName)] = np.array(np.repeat(f['GriddedGeolocationAndFlags']['grid_lat'][:], cols), dtype='float32')
    obs_data[('longitude', metaDataName)] = np.array(np.tile(f['GriddedGeolocationAndFlags']['grid_lon'][:], rows), dtype='float32')
    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    mean_sat_alt = get_mean_satellite_altitude(f)
    obs_data[('stationElevation', metaDataName)] = np.full((nlocs), mean_sat_alt, dtype='float32')
    obs_data[('height', metaDataName)] = np.full((nlocs), 17.0, dtype='float32')
    obs_time = combine_fore_aft_time(f)
    obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(obs_time), dtype='int64')

    obs_data[('windSpeed', obsValName)] = np.array(windSpeed.flatten(), dtype='float32')
    obs_data[('windDirection', obsValName)] = np.array(f['EnvDataRecords']['wind_dir'][:].flatten(), dtype='float32')
    obs_data[('windSpeed', obsErrName)] = np.array(0.01*f['EnvDataRecords']['wind_error'][:].flatten(), dtype='float32')
    obs_data[('windDirection', obsErrName)] = np.full((nlocs), 180, dtype='float32')
    obs_data[('windSpeed', qcName)] = np.array(f['EnvDataRecords']['wind_speed_flag'][:].flatten(), dtype='int32')
    obs_data[('windDirection', qcName)] = np.array(f['EnvDataRecords']['wind_dir_flag'][:].flatten(), dtype='int32')
    # get the windEastward and windNorthward
    u, v, u_err, v_err = wind_speed_direction_to_uv(obs_data[('windSpeed', obsValName)],
                                                    obs_data[('windDirection', obsValName)],
                                                    obs_data[('windSpeed', obsErrName)])
    obs_data[('windEastward', obsValName)] = np.array(u, dtype='float32')
    obs_data[('windNorthward', obsValName)] = np.array(v, dtype='float32')
    obs_data[('windEastward', obsErrName)] = np.array(u_err, dtype='float32')
    obs_data[('windNorthward', obsErrName)] = np.array(v_err, dtype='float32')
    # Create flags for eastward and northward wind: 1 if either speed or direction flag is > 0, otherwise 0
    windEastward_flag = np.zeros_like(obs_data[('windSpeed', qcName)], dtype='int32')
    windNorthward_flag = np.zeros_like(obs_data[('windSpeed', qcName)], dtype='int32')
    flag_mask = (obs_data[('windSpeed', qcName)] > 0) | (obs_data[('windDirection', qcName)] > 0)
    windEastward_flag[flag_mask] = 1
    windNorthward_flag[flag_mask] = 1
    obs_data[('windEastward', qcName)] = np.array(windEastward_flag, dtype='int32')
    obs_data[('windNorthward', qcName)] = np.array(windNorthward_flag, dtype='int32')

    obs_data = apply_gross_qc(obs_data)
    return obs_data


def get_WMO_satellite_ID(sensor_name):

    if 'COWVR' in sensor_name:
        WMO_sat_ID = ISS_COWVR_WMO_sat_ID
    else:
        WMO_sat_ID = -1

    return WMO_sat_ID


def get_global_attributes(wmo_satellite_id):
    if wmo_satellite_id[0] == ISS_COWVR_WMO_sat_ID:
        GlobalAttrs = {
            "platformCommonName": "OSW_COWVR",
            "platformLongDescription": "Ocean Surface Wind from COWVR",
        }
    else:
        print(f" could not determine satellite from satelliteIdentifier: {wmo_satellite_id[0]}")
        sys.exit()

    return GlobalAttrs


def combine_fore_aft_time(f):
    # get the tai93 times from both fore and aft and combine into single array
    # use fore value by default and aft value if fore is missing
    # missing value assumed minimum (e.g. -9999.0)
    fore_data = f['GriddedGeolocationAndFlags']['grid_time_tai93_fore'][:]
    aft_data = f['GriddedGeolocationAndFlags']['grid_time_tai93_aft'][:]
    missing_value = np.min(fore_data)
    combined_data = np.copy(fore_data)
    mask_fore_missing_aft_valid = (fore_data == missing_value) & (aft_data != missing_value)
    combined_data[mask_fore_missing_aft_valid] = aft_data[mask_fore_missing_aft_valid]

    return combined_data


def get_mean_satellite_altitude(f):
    # compute the mean satellite altitude
    # this array is not the same dimension as OSW observation
    sat_alt = f['GeolocationAndFlags']['sat_alt'][:]
    missing_value = np.min(sat_alt)
    mean_sat_alt = np.mean(sat_alt[sat_alt != missing_value])

    return mean_sat_alt


def get_epoch_time(obs_time_tai93):
    # use approximate offset of 725846427s between 01Jan1970 and 01Jan1993
    # missing value assumed minimum (e.g. -9999.0)
    time_offset = obs_time_tai93[:].flatten()
    missing_value = np.min(time_offset)
    time_offset[time_offset != missing_value] += 725846427

    return time_offset


def wind_speed_direction_to_uv(windSpeed, windDirection, windSpeedError):
    """
    Converts wind speed and direction (in degrees) to eastward (u) and
    northward (v) wind components.

    Args:
      windSpeed (numpy.ndarray): Array of wind speeds (magnitude).
      windDirection (numpy.ndarray): Array of wind directions

    Returns:
      tuple: A tuple containing two numpy.ndarrays:
             - windEastward (u): Eastward wind component. Positive is wind
                                 blowing towards the East.
             - windNorthward (v): Northward wind component. Positive is wind
                                  blowing towards the North.
    """
    # Meteorological direction is the direction from which the wind is blowing
    # standard angle conventions (0 degrees at East, increasing counter-clockwise)

    # use minimum as missing value
    missing_value = np.min(windSpeed)
    # Create a mask where both windSpeed and windDirection are valid
    valid_mask = (windSpeed != missing_value) & (windDirection != missing_value)

    # Initialize output arrays with the same shape and missing values
    windEastward = np.full_like(windSpeed, missing_value, dtype=np.float64)
    windNorthward = np.full_like(windSpeed, missing_value, dtype=np.float64)
    windEastwardError = np.full_like(windSpeed, missing_value, dtype=np.float64)
    windNorthwardError = np.full_like(windSpeed, missing_value, dtype=np.float64)

    # Convert valid wind directions to radians (meteorological to standard)
    rad = np.deg2rad(windDirection[valid_mask])

    # Calculate eastward and northward components only for valid data
    windEastward[valid_mask] = windSpeed[valid_mask] * np.cos(rad)
    windNorthward[valid_mask] = windSpeed[valid_mask] * np.sin(rad)
    windEastwardError[valid_mask] = windSpeedError[valid_mask] * np.cos(rad)
    windNorthwardError[valid_mask] = windSpeedError[valid_mask] * np.sin(rad)

    return windEastward, windNorthward, windEastwardError, windNorthwardError


def apply_gross_qc(obs_data):
    # remove the missing values from the output
    missing_value_data = np.min(obs_data[('windSpeed', 'ObsValue')])
    missing_value_time = np.min(obs_data[('dateTime', 'MetaData')])
    valid_mask = (obs_data[('windSpeed', 'ObsValue')] != missing_value_data) & \
                 (obs_data[('windDirection', 'ObsValue')] != missing_value_data) & \
                 (obs_data[('dateTime', 'MetaData')] != missing_value_time) & \
                 (obs_data[('windSpeed', 'PreQC')] == 0) & \
                 (obs_data[('windDirection', 'PreQC')] == 0)
    for k in obs_data.keys():
        obs_data[k] = obs_data[k][valid_mask]
        # print(f"{k=}  {np.min(obs_data[k])=}   {np.max(obs_data[k])=}")

    return obs_data


def init_obs_loc():
    obs = {
        ('windSpeed', obsValName): [],
        ('windDirection', obsValName): [],
        ('windEastward', obsValName): [],
        ('windNorthward', obsValName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('height', metaDataName): [],
        ('stationElevation', metaDataName): [],
        ('satelliteIdentifier', metaDataName): [],
    }

    return obs


# ----------------------------------------------------------------------
# Time function
# ----------------------------------------------------------------------
def record_time(tic=None, print_log=True):

    if not tic:
        tic = time.perf_counter()
        if print_log:
            print(f"  ... starting timer: {tic:0.3f}")
        return tic
    else:
        toc = time.perf_counter()
        if print_log:
            print(f"  ... elapsed time (sec): {toc - tic:0.3f}")
        return toc


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
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             ' (default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '-o', '--output',
        help='fullpath and name for ioda output file',
        type=str, default=os.path.join(os.getcwd(), 'test.nc4'))
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, default=None)

    args = parser.parse_args()

    main(args)
