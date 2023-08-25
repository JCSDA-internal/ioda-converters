#!/usr/bin/env python3

#
# (C) Copyright 2020-2023 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#


"""
Python code to ingest hdf5 GMI data
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
from pyiodaconv.def_jedi_utils import concat_obs_dict
from pyiodaconv.def_jedi_utils import iso8601_string
from pyiodaconv.def_jedi_utils import float_missing_value, int_missing_value, long_missing_value

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

# globals
GCOMW_WMO_sat_ID = 122
GPM_WMO_sat_ID = 288

GlobalAttrs = {
    "platformCommonName": "GMI",
    "platformLongDescription": "GMI Brightness Temperature Data",
    "sensorCentralFrequency": "[10.65V, 10.65H, 18.7V, 18.7H, 23.8V, 36.5V, 36.5H, 89.0V, 89.0H, 166.5V, 166.5H, 183.31+/-3V, 183.31+/-7V]"
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

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
    WMO_sat_ID = GPM_WMO_sat_ID

    nscans = np.shape(f['S1']['Latitude'])[0]
    nbeam_pos = np.shape(f['S1']['Latitude'])[1]
    nchans = 13
    # Two-swaths covering 10.65 to 183 GHz
    obs_data[('latitude', metaDataName)] = np.array(f['S1']['Latitude'], dtype='float32').flatten()
    obs_data[('longitude', metaDataName)] = np.array(f['S1']['Longitude'], dtype='float32').flatten()
    # start at channel 5 as lowest frequencies are not included
    obs_data[('sensorChannelNumber', metaDataName)] = np.array(np.arange(nchans)+1, dtype='int32')
    k = 'sensorScanPosition'
    obs_data[(k, metaDataName)] = np.tile(np.arange(nbeam_pos, dtype='int32')+1, (nscans, 1)).flatten()
    k = 'sensorZenithAngle'   # ~52.85 low freq incidence angle (~49.16 higher freq)
    obs_data[(k, metaDataName)] = np.array(f['S1']['incidenceAngle'], dtype='float32').flatten()
    instr_scan_ang = obs_data[(k, metaDataName)]
    # compute view angle
    sat_altitude = np.empty_like(instr_scan_ang)
    sat_altitude[:] = 407.0
    obs_data[('sensorViewAngle', metaDataName)] = compute_scan_angle(
        instr_scan_ang,
        sat_altitude,
        instr_scan_ang)

    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(f), dtype='int64')

    nlocs = len(obs_data[('latitude', metaDataName)])
    k = 'brightnessTemperature'
    # have to reorder the channel axis to be last then merge ( nscans x nspots = nlocs )
    obs_data[(k, "ObsValue")] = np.transpose(
        (np.array(f['S1']['Tc'][:, :, 0], dtype='float32').flatten(),
         np.array(f['S1']['Tc'][:, :, 1], dtype='float32').flatten(),
         np.array(f['S1']['Tc'][:, :, 2], dtype='float32').flatten(),
         np.array(f['S1']['Tc'][:, :, 3], dtype='float32').flatten(),
         np.array(f['S1']['Tc'][:, :, 4], dtype='float32').flatten(),
         np.array(f['S1']['Tc'][:, :, 5], dtype='float32').flatten(),
         np.array(f['S1']['Tc'][:, :, 6], dtype='float32').flatten(),
         np.array(f['S1']['Tc'][:, :, 7], dtype='float32').flatten(),
         np.array(f['S1']['Tc'][:, :, 8], dtype='float32').flatten(),
         np.array(f['S2']['Tc'][:, :, 0], dtype='float32').flatten(),
         np.array(f['S2']['Tc'][:, :, 1], dtype='float32').flatten(),
         np.array(f['S2']['Tc'][:, :, 2], dtype='float32').flatten(),
         np.array(f['S2']['Tc'][:, :, 3], dtype='float32').flatten()))

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
    # low frequency
    chk_ob = np.array(f['S1']['Quality']).flatten() != 0
    for ich in range(9):
        obs_data[obs_key][:, ich][chk_ob] = float_missing_value
    # high frequency
    chk_ob = np.array(f['S2']['Quality']).flatten() != 0
    for ich in range(4):
        obs_data[obs_key][:, 9+ich][chk_ob] = float_missing_value

    tb_key = 'brightnessTemperature'
    good = (obs_data[(tb_key, obsValName)][:, 0] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 3] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 4] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 5] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 8] != float_missing_value)
#       (obs_data[(tb_key, obsValName)][:, 9] != float_missing_value) & \  # upper frequencies are missing more frequently
#       (obs_data[(tb_key, obsValName)][:, 12] != float_missing_value)
    for k in obs_data:
        if metaDataName in k[1] and 'sensorChannelNumber' not in k[0]:
            obs_data[k] = obs_data[k][good]      # [::36] ## add as skip
        elif tb_key in k[0]:
            obs_data[k] = obs_data[k][good, :]   # [::36] ## add as skip

    return obs_data


def get_epoch_time(f):

    nbeam_pos = np.shape(f['S1']['Latitude'])[1]
    year = f['S1']['ScanTime']['Year']
    month = f['S1']['ScanTime']['Month']
    day = f['S1']['ScanTime']['DayOfMonth']
    hour = f['S1']['ScanTime']['Hour']
    minute = f['S1']['ScanTime']['Minute']
    second = f['S1']['ScanTime']['Second']

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
        ('sensorZenithAngle', metaDataName): [],
        ('sensorViewAngle', metaDataName): [],
    }
# ('solarZenithAngle', metaDataName): [],
# ('solarAzimuthAngle', metaDataName): [],
# ('sensorAzimuthAngle', metaDataName): [],
# ('satelliteAscendingFlag', metaDataName): [],

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
