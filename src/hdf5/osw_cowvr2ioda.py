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
    VarDims = {
        'windSpeed': ['Location'],
        'windDirection': ['Location'],
    }
    # num_wind_amb
    # wind_dir
    # wind_dir_amb
    # wind_dir_flag
    # wind_error
    # wind_error_amb
    # wind_speed
    # wind_speed_flag

    DimDict = {
        'Location': nlocs,
    }
    writer = iconv.IodaWriter(output_filename, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('dateTime', metaDataName)]['units'] = iso8601_string
    VarAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value

    VarAttrs[('windSpeed', obsValName)]['units'] = 'm s-1'
    VarAttrs[('windSpeed', obsErrName)]['units'] = 'm s-1'
    VarAttrs[('windDirection', obsValName)]['units'] = 'degree'
    VarAttrs[('windDirection', obsErrName)]['units'] = 'degree'

    VarAttrs[('windSpeed', obsValName)]['_FillValue'] = float_missing_value
    VarAttrs[('windSpeed', obsErrName)]['_FillValue'] = float_missing_value
    VarAttrs[('windSpeed', qcName)]['_FillValue'] = int_missing_value
    VarAttrs[('windDirection', obsValName)]['_FillValue'] = float_missing_value
    VarAttrs[('windDirection', obsErrName)]['_FillValue'] = float_missing_value
    VarAttrs[('windDirection', qcName)]['_FillValue'] = int_missing_value

    VarAttrs[('dateTime', metaDataName)]['units'] = iso8601_string
    VarAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value

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

    # import pdb
    # pdb.set_trace()
    # import sys
    # sys.exit()
    # obs is on a grid (601, 1801)
    windSpeed = f['EnvDataRecords']['wind_speed'][:]

    # Get the shape of the wind speed data
    rows, cols = windSpeed.shape

    obs_data[('latitude', metaDataName)] = np.array(np.repeat(f['GriddedGeolocationAndFlags']['grid_lat'][:], cols), dtype='float32')
    obs_data[('longitude', metaDataName)] = np.array(np.tile(f['GriddedGeolocationAndFlags']['grid_lon'][:], rows), dtype='float32')
    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    obs_data[('height', metaDataName)] = np.full((nlocs), 17.0, dtype='float32')
    # obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(f['GeolocationAndFlags']['time_string']), dtype='int64')
    obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(f['GriddedGeolocationAndFlags']['grid_time_tai93_fore']), dtype='int64')

    obs_data[('windSpeed', obsValName)] = np.array(windSpeed.flatten(), dtype='float32')
    obs_data[('windDirection', obsValName)] = np.array(f['EnvDataRecords']['wind_dir'][:].flatten(), dtype='float32')
    obs_data[('windSpeed', obsErrName)] = np.array(f['EnvDataRecords']['wind_error'][:].flatten(), dtype='float32')
    obs_data[('windDirection', obsErrName)] = np.full((nlocs), 180, dtype='float32')
    obs_data[('windSpeed', qcName)] = np.array(f['EnvDataRecords']['wind_speed_flag'][:].flatten(), dtype='int32')
    obs_data[('windDirection', qcName)] = np.array(f['EnvDataRecords']['wind_dir_flag'][:].flatten(), dtype='int32')

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


def get_epoch_time(obs_time_tai93):

    # use approximate offset of 725846427s between 01Jan1970 and 01Jan1993
    time_offset = obs_time_tai93[:].flatten() 
    time_offset += 725846427

    return time_offset


def init_obs_loc():
    obs = {
        ('windSpeed', obsValName): [],
        ('windDirection', obsValName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('height', metaDataName): [],
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
