#!/usr/bin/env python3

#
# (C) Copyright 2020-2023 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

"""
Python code to ingest ASCII SSMIS Unified Pre-Processor data and put into IODA format
"""

import argparse
from datetime import datetime, timedelta, timezone
import glob
# from concurrent.futures import ProcessPoolExecutor
from pathlib import Path
import os.path
from os import getcwd
import sys
import time

import numpy as np

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.def_jedi_utils import compute_scan_angle
from pyiodaconv.orddicts import DefaultOrderedDict

# globals
DMSPF16_WMO_sat_ID = 249
DMSPF17_WMO_sat_ID = 285
DMSPF18_WMO_sat_ID = 286

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
        'brightnessTemperature': ['Location', 'Channel'],
        'sensorChannelNumber': ['Channel'],
    }

    DimDict = {
        'Location': nlocs,
        'Channel': obs_data[('sensorChannelNumber', metaDataName)],
    }
    writer = iconv.IodaWriter(output_filename, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('sensorZenithAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('sensorViewAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('sensorAzimuthAngle', metaDataName)]['units'] = 'degree'
    if ('solarZenithAngle', metaDataName) in obs_data.keys():
        VarAttrs[('solarZenithAngle', metaDataName)]['units'] = 'degree'
        VarAttrs[('solarAzimuthAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('dateTime', metaDataName)]['units'] = iso8601_string
    VarAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value

    VarAttrs[('brightnessTemperature', obsValName)]['units'] = 'K'
    VarAttrs[('brightnessTemperature', obsErrName)]['units'] = 'K'

    VarAttrs[('brightnessTemperature', obsValName)]['_FillValue'] = float_missing_value
    VarAttrs[('brightnessTemperature', obsErrName)]['_FillValue'] = float_missing_value
    VarAttrs[('brightnessTemperature', qcName)]['_FillValue'] = int_missing_value

    VarAttrs[('dateTime', metaDataName)]['units'] = iso8601_string
    VarAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)

    # report time
    toc = record_time(tic=tic)


def get_data_from_files(afile):

    # allocate space for output depending on which variables are to be saved
    obs_data = get_ssmis_data(afile)

    return obs_data


def get_ssmis_data(afile, add_qc=False):

    local_data = init_obs_loc()
    # these files are specific only for the SSMIS and SSMIS UAS data types
    with open(afile, 'r') as file:
        # we need metaData information from the filename (which satellite)
        WMO_sat_ID, timestamp = get_file_metadata(file.name)

        # Create an iterator from the file object
        file_iterator = iter(file)
        # read the one line header
        line = next(file_iterator)
        ssmis_uas = is_ssmis_uas(line.split()[-1])
        while True:
            try:
                line = next(file_iterator)
                local_data = populate_obsValue(line, local_data, WMO_sat_ID=WMO_sat_ID, ssmis_uas=ssmis_uas, add_qc=add_qc)
            except StopIteration:
                # If StopIteration is raised, break from the loop
                break

    local_data = numpy_obs(local_data)
    return local_data


def populate_obsValue(line, local_data, WMO_sat_ID=int_missing_value, ssmis_uas=False, add_qc=True):

    # this is specifically for SSMIS UPP files
    sensor_altitude = 850000.  # SSMIS satelite altitude approximate
    sensor_zenith = 45.0  # SSMIS zenith

    # read data lines beginning at fourth line
    try:
        if ssmis_uas:
            nchans = 5
            channel_offset = 19
            latitude, longitude, scanline, scanposition, rain_flag, surface_type, \
                tb_ch01, tb_ch02, tb_ch03, tb_ch04, tb_ch05, \
                surface_flag, irej, year, julian_day, \
                month, day, hour, minute, second,  \
                iasc, t_arm, t_ref, dt_t_ref, \
                bdotk, theta_b, bmag, bdk_mean, bdk_sdev, \
                eph_lat_1, eph_lon_1, eph_lat_2, eph_lon_2, orbit_angle = line.split()
        else:
            nchans = 24
            channel_offset = 1
            latitude, longitude, scanline, scanposition, rain_flag, surface_type, \
                tb_ch01, tb_ch02, tb_ch03, tb_ch04, tb_ch05, tb_ch06, tb_ch07, tb_ch08, \
                tb_ch09, tb_ch10, tb_ch11, tb_ch12, tb_ch13, tb_ch14, tb_ch15, tb_ch16, \
                tb_ch17, tb_ch18, tb_ch19, tb_ch20, tb_ch21, tb_ch22, tb_ch23, tb_ch24, \
                surface_flag, irej, year, julian_day, \
                month, day, hour, minute, second,  \
                iasc, t_arm, t_ref, dt_t_ref, \
                eph_lat_1, eph_lon_1, eph_lat_2, eph_lon_2, orbit_angle = line.split()
    except ValueError:
        return local_data

    if len(local_data[('sensorChannelNumber', metaDataName)]) != nchans:
        local_data[('sensorChannelNumber', metaDataName)] = np.array(np.arange(nchans)+channel_offset, dtype="int32")
    # local_data[('sensorAltitude'?  .append(sensor_altitude)
    local_data[('latitude', metaDataName)].append(float(latitude))
    local_data[('longitude', metaDataName)].append(float(longitude))
    local_data[('sensorScanPosition', metaDataName)].append(int(scanposition))
    local_data[('sensorZenithAngle', metaDataName)].append(sensor_zenith)
    # compute from beam position?
    local_data[('sensorAzimuthAngle', metaDataName)].append(0.)
    # confirm view angle computation
    # should call this function and it should provide
    # Scene Altitude                 EIA
    # Sfc                            53.33
    # 11 km                          53.20
    # 60 km                          52.62
    local_data[('sensorViewAngle', metaDataName)].append(sensor_zenith)
#   local_data[('sensorViewAngle', metaDataName)] = compute_scan_angle(
#       sensor_altitude,
#       sensor_zenith,
#       qc_flag=[[int(irej)]])

    nlocs = len(local_data[('latitude', metaDataName)])
    local_data[('satelliteIdentifier', metaDataName)].append(WMO_sat_ID)
    # will use a single time for now... need to find out seconds between scans?
    datetime_obj = datetime(
        year=int(year),
        month=int(month),
        day=int(day),
        hour=int(hour),
        minute=int(minute),
        second=int(second)
    )
    local_data[('dateTime', metaDataName)].append(get_epoch_time(datetime_obj))
    qc_flag = int(irej)

    if ssmis_uas:
        local_data[('brightnessTemperature', obsValName)].append(np.array(
            [tb_ch01, tb_ch02, tb_ch03, tb_ch04, tb_ch05], dtype='float32'))
    else:
        local_data[('brightnessTemperature', obsValName)].append(np.array(
            [tb_ch01, tb_ch02, tb_ch03, tb_ch04, tb_ch05, tb_ch06, tb_ch07, tb_ch08,
             tb_ch09, tb_ch10, tb_ch11, tb_ch12, tb_ch13, tb_ch14, tb_ch15, tb_ch16,
             tb_ch17, tb_ch18, tb_ch19, tb_ch20, tb_ch21, tb_ch22, tb_ch23, tb_ch24], dtype='float32'))
    local_data[('brightnessTemperature', obsErrName)].append(np.full((nchans), 5.0, dtype='float32'))
    local_data[('brightnessTemperature', qcName)].append(np.full((nchans), qc_flag, dtype='int32'))

    if add_qc:
        local_data = ssmis_gross_quality_control(local_data, qc_flag)

    return local_data


def ssmis_gross_quality_control(obs_data, qc_flag):

    tb_key = 'brightnessTemperature'
    good = \
        (obs_data[(tb_key, obsValName)][:, 0] > 10) & (obs_data[(tb_key, obsValName)][:, 0] < 400) & \
        (obs_data[(tb_key, obsValName)][:, 4] > 10) & (obs_data[(tb_key, obsValName)][:, 4] < 400) & \
        (obs_data[(tb_key, obsValName)][:, 8] > 10) & (obs_data[(tb_key, obsValName)][:, 8] < 400) & \
        (obs_data[('latitude', metaDataName)] >= -90) & (obs_data[('latitude', metaDataName)] <= 90) & \
        (qc_flag[:] == 0)

    for k in obs_data:
        if metaDataName in k[1] and 'sensorChannelNumber' not in k[0]:
            obs_data[k] = obs_data[k][good]     # [::33] -- add as skip
        elif tb_key in k[0]:
            obs_data[k] = obs_data[k][good, :]  # [::33] -- add as skip

    return obs_data


def is_ssmis_uas(sensor_name):

    ssmis_uas = False
    if 'UAS' in sensor_name:
        ssmis_uas = True

    return ssmis_uas


def get_global_attributes(wmo_satellite_id):
    if wmo_satellite_id[0] in (DMSPF16_WMO_sat_ID, DMSPF17_WMO_sat_ID, DMSPF18_WMO_sat_ID):
        GlobalAttrs = {
            "platformCommonName": "SSMIS",
            "platformLongDescription": "SSMIS Brightness Temperature Data",
            "sensorCentralFrequency": [50.3,
                                       52.8,
                                       53.596,
                                       54.4,
                                       55.5,
                                       57.29,
                                       59.4,
                                       150.0,
                                       183.31,
                                       183.31,
                                       183.31,
                                       19.35,
                                       19.35,
                                       22.235,
                                       37.0,
                                       37.0,
                                       91.655,
                                       91.655,
                                       63.283248,
                                       60.792668,
                                       60.792668,
                                       60.792668,
                                       60.792668,
                                       60.792668]
        }

    else:
        print(f" could not determine satellite from satelliteIdentifier: {wmo_satellite_id[0]}")
        sys.exit()

    return GlobalAttrs


def get_epoch_time(adatetime):

    time_offset = round((adatetime - epoch).total_seconds())

    return time_offset


def init_obs_loc():
    obs = {
        ('brightnessTemperature', obsValName): [],
        ('brightnessTemperature', obsErrName): [],
        ('brightnessTemperature', qcName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('sensorZenithAngle', metaDataName): [],
        ('sensorAzimuthAngle', metaDataName): [],
        ('sensorViewAngle', metaDataName): [],
        ('sensorScanPosition', metaDataName): [],
        ('satelliteIdentifier', metaDataName): [],
        ('sensorChannelNumber', metaDataName): [],
        ('dateTime', metaDataName): [],
    }
#       ('solarZenithAngle', metaDataName): [],
#       ('solarAzimuthAngle', metaDataName): [],

    return obs


def numpy_obs(obs_data):
    obs_types = {
        ('brightnessTemperature', obsValName): 'float32',
        ('brightnessTemperature', obsErrName): 'float32',
        ('brightnessTemperature', qcName): 'int32',
        ('latitude', metaDataName): 'float32',
        ('longitude', metaDataName): 'float32',
        ('sensorZenithAngle', metaDataName): 'float32',
        ('sensorAzimuthAngle', metaDataName): 'float32',
        ('sensorViewAngle', metaDataName): 'float32',
        ('sensorScanPosition', metaDataName): 'int32',
        ('satelliteIdentifier', metaDataName): 'int32',
        ('sensorChannelNumber', metaDataName): 'int32',
        ('dateTime', metaDataName): 'int64',
    }
    for k in obs_data:
        if k in obs_types.keys():
            obs_data[k] = np.array(obs_data[k], dtype=obs_types[k])
        else:
            print('{k = }  not found in obs_types keys needs a default type')

    return obs_data


def get_file_metadata(filename):

    # create a datetime object with a start and ending time
    # example filename: tdris_f17_d20240702_s231600_e005900_r91143_uas_cfnoc.asc
    _, sat_id, YYYYMMDD, startTime, endTime, rev_number, *rest = os.path.basename(filename).split('_')
    start_datetime_obj = datetime.strptime(YYYYMMDD[1:] + startTime[1:], "%Y%m%d%H%M%S")
    end_datetime_obj = datetime.strptime(YYYYMMDD[1:] + endTime[1:], "%Y%m%d%H%M%S")

    # If the end time is earlier than the start time, it means it belongs to the next day
    if end_datetime_obj < start_datetime_obj:
        end_datetime_obj += timedelta(days=1)

    file_timestamp = [start_datetime_obj, end_datetime_obj]

    if sat_id == 'f16':
        WMO_sat_ID = DMSPF16_WMO_sat_ID
    elif sat_id == 'f17':
        WMO_sat_ID = DMSPF17_WMO_sat_ID
    elif sat_id == 'f18':
        WMO_sat_ID = DMSPF18_WMO_sat_ID
    else:
        print(f" WARNING could not determine satellite WMO ID from: {file.name}")
        WMO_sat_ID = int_missing_value

    return WMO_sat_ID, file_timestamp


def record_time(tic=None, print_log=True):
    # ----------------------------------------------------------------------
    # Time function
    # ----------------------------------------------------------------------

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
