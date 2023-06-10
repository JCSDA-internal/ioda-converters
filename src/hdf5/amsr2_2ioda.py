#!/usr/bin/env python3

#
# (C) Copyright 2020-2023 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#


"""
Python code to ingest hdf5 AMSR/2 data
"""

import argparse
from datetime import datetime
import os.path
import sys

import h5py
import numpy as np

import lib_python.ioda_conv_engines as iconv
from lib_python.orddicts import DefaultOrderedDict
from hdf5.atms_netcdf_hdf5_2ioda import set_metadata_attributes, set_obspace_attributes
from hdf5.cowvr_hdf5_2ioda import compute_scan_angle
from lib_python.def_jedi_utils import concat_obs_dict

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

# globals
GCOMW_WMO_sat_ID = 122
GPM_WMO_sat_ID = 288

GlobalAttrs = {
    "platformCommonName": "AMSR2",
    "platformLongDescription": "AMSR-2 Brightness Temperature Data",
    "sensorCentralFrequency": "[10.65V, 10.65H, 18.7V, 18.7H, 23.8V, 23.8H, 36.5V, 36.5H, 89.0V (A), 89.0H (A)]"
#   "sensorCentralFrequency": "[6.925V, 6.925H, 7.3V, 7.3H, 10.65V, 10.65H, 18.7V, 18.7H, 23.8V, 23.8H, 36.5V, 36.5H, 89.0V (A), 89.0H (A), 89.0V (B), 89.0H (B)]"
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

iso8601_string = "seconds since 1970-01-01T00:00:00Z"
epoch = datetime.fromisoformat(iso8601_string[14:-1])


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

    # k = 'brightnessTemperature'
    # VarAttrs[(k, 'ObsValue')]['_FillValue'] = float_missing_value
    # VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
    # VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
    # VarAttrs[(k, 'ObsValue')]['units'] = 'K'
    # VarAttrs[(k, 'ObsError')]['units'] = 'K'
    # VarAttrs[(k, 'PreQC')]['units'] = 'unitless'

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

    # f.attrs['FileHeader']
    # \nSatelliteName=GCOMW1;
    # WMO_sat_ID = get_WMO_satellite_ID(f.attrs['ShortName'].decode("utf-8"))
    sensor = f.attrs['SensorShortName'].item()
    WMO_sat_ID = get_wmo_id(f.attrs['PlatformShortName'].item())
    nscans = np.shape(f['Latitude of Observation Point for 89A'])[0]
    nbeam_pos = np.shape(f['Latitude of Observation Point for 89A'])[1]
    nchans = 14
    # beam position or sampling across scan 243 for lower frequencies 486 for 89GHz
    obs_data[('latitude', metaDataName)] = np.array(f['Latitude of Observation Point for 89A'], dtype='float32').flatten()
    obs_data[('longitude', metaDataName)] = np.array(f['Longitude of Observation Point for 89A'], dtype='float32').flatten()
    # start at channel 5 as lowest frequencies are not included
    obs_data[('sensorChannelNumber', metaDataName)] = np.array(np.arange(nchans), dtype='int32')
    k = 'sensorScanPosition'
    obs_data[(k, metaDataName)] = np.tile(np.arange(nbeam_pos, dtype='float32')+1, (nscans, 1)).flatten()
    k = 'sensorZenithAngle'   # ~55.2 incidence angle
    obs_data[(k, metaDataName)] = np.array(f['S2']['incidenceAngle'], dtype='float32').flatten()
    instr_scan_ang = obs_data[(k, metaDataName)]
    # compute view angle
    sat_altitude = np.empty_like(instr_scan_ang)
    sat_altitude[:] = f.attrs['SatelliteAltitude'].item()
    orbit_direction = f.attrs['OrbitDirection'].item()
    iasc = get_asc_dsc(f.filename)
    obs_data[('sensorViewAngle', metaDataName)] = compute_scan_angle(
        instr_scan_ang,
        sat_altitude,
        instr_scan_ang)

    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(f), dtype='int64')
    # obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(f.attrs['ObservationStartDateTime'].item()), dtype='int64')


    nlocs = len(obs_data[('latitude', metaDataName)])
    k = 'brightnessTemperature'
    # have to reorder the channel axis to be last then merge ( nscans x nspots = nlocs )
    obs_data[(k, "ObsValue")] = np.transpose(
                                 ( np.array(f['Brightness Temperature (6.9GHz,H)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (6.9GHz,V)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (7.3GHz,H)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (7.3GHz,V)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (10.7GHz,H)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (10.7GHz,V)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (18.7GHz,H)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (18.7GHz,V)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (23.8GHz,H)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (23.8GHz,V)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (36.5GHz,H)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (36.5GHz,V)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (89.0GHz-A,H)'], dtype='float32').flatten(),
                                   np.array(f['Brightness Temperature (89.0GHz-A,V)'], dtype='float32').flatten() ))
    obs_data[(k, "ObsError")] = np.full((nlocs, nchans), 5.0, dtype='float32')
    obs_data[(k, "PreQC")] = np.full((nlocs, nchans), 0, dtype='int32')

    # check some global satellite geometry will compress all data using this
    chk_geolocation = (obs_data[('latitude', metaDataName)] > 90) | (obs_data[('latitude', metaDataName)] < -90) | \
        (obs_data[('longitude', metaDataName)] > 180) | (obs_data[('longitude', metaDataName)] < -180) | \
        (obs_data[('sensorZenithAngle', metaDataName)] > 80) | (obs_data[('sensorZenithAngle', metaDataName)] < 0)

    obs_data[('latitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('longitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('sensorZenithAngle', metaDataName)][chk_geolocation] = float_missing_value

    obs_key = (k, "ObsValue")
    obs_data = set_missing_value(f, obs_key, obs_data)

    return obs_data


def set_missing_value(f, obs_key, obs_data):

    # 0 Good
    # 1 Warning – Possible sun glint, 0 <= sunGlintAngle < 20 degrees.
    # 2 Warning – Possible radio frequency interference.
    # 3 Warning – Degraded geolocation data.
    # 4 Warning – Data corrected for warm load intrusion.
    # -1 Error – Data are missing from file or are unreadable.
    # -2 Error – Invalid Tb or nonphysical brightness temperature (Tb < 40K or Tb > 350K).
    # -3 Error – Error in geolocation data.
    # -4 Error – Data are missing in one channel.
    # -5 Error – Data are missing in multiple channels.
    # -6 Error – Latitude/longitude values are out of range.
    # -7 Error – Non-normal status modes.

    # use quality word to set missing values
    chk_ob = np.array(f['S1']['Quality']).flatten() != 0
    obs_data[obs_key][:, 0][chk_ob] = float_missing_value
    obs_data[obs_key][:, 1][chk_ob] = float_missing_value
    chk_ob = np.array(f['S2']['Quality']).flatten() != 0
    obs_data[obs_key][:, 2][chk_ob] = float_missing_value
    obs_data[obs_key][:, 3][chk_ob] = float_missing_value
    chk_ob = np.array(f['S3']['Quality']).flatten() != 0
    obs_data[obs_key][:, 4][chk_ob] = float_missing_value
    obs_data[obs_key][:, 5][chk_ob] = float_missing_value
    chk_ob = np.array(f['S4']['Quality']).flatten() != 0
    obs_data[obs_key][:, 6][chk_ob] = float_missing_value
    obs_data[obs_key][:, 7][chk_ob] = float_missing_value
    chk_ob = np.array(f['S5']['Quality'][:,1::2]).flatten() != 0
    obs_data[obs_key][:, 8][chk_ob] = float_missing_value
    obs_data[obs_key][:, 9][chk_ob] = float_missing_value

    tb_key = 'brightnessTemperature'
    good = (obs_data[(tb_key, obsValName)][:, 0] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 2] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 4] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 6] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 8] != float_missing_value)
    for k in obs_data:
        if metaDataName in k[1] and 'sensorChannelNumber' not in k[0]:
            obs_data[k] = obs_data[k][good]     # [::24] ## add as skip
        elif tb_key in k[0]:
            obs_data[k] = obs_data[k][good, :]  # [::24] ## add as skip

    return obs_data


def get_asc_dsc(filename)
    # get ascending/descending ugh
    # from JAXA gportal files formatted like so
    # GW1AM2_202202160040_188D_L1SGBTBR_2220220.h5
    try:
        satSensor, dtg, orbit, prod, _ = os.path.split(f.filename)[-1].split('_')
    except:
        print(f' ... WARNING ... can not determine ascending or descending from filename')
        iasc = None

    try:
        if orbit[-1:] == 'A':
            iasc = 1
        elif orbit[-1:] == 'D':
            iasc = 0
        else:
            print(f' ... WARNING ... can not determine ascending or descending from filename')
            iasc = None


def get_wmo_id(platform):
    if platform == 'GCOM-W1':
        WMO_sat_ID = GCOMW_WMO_sat_ID
    else:
        print(' ... could not find PlatformShortName as file attribute')
        return None
    return WMO_sat_ID


def get_epoch_time(f):

    # k: ObservationStartDateTime item: 2022-02-16T00:40:51.967Z
    # isoformat            '2015-02-04T20:55:08.914461+00:00'
    dtgObj = datetime.datetime.fromisoformat(f.attrs['ObservationStartDateTime'].item()[:-1])
    nbeam_pos = np.shape(f['S1']['Latitude'])[1]
    year = dtgObj.year
    month = dtgObj.month
    day = dtgObj.day
    hour = dtgObj.hour
    minute = dtgObj.minute
    second = dtgObj.second

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
#       ('solarZenithAngle', metaDataName): [],
#       ('solarAzimuthAngle', metaDataName): [],
        ('sensorZenithAngle', metaDataName): [],
#       ('sensorAzimuthAngle', metaDataName): [],
        ('sensorViewAngle', metaDataName): [],
#       ('satelliteAscendingFlag', metaDataName): [],
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
