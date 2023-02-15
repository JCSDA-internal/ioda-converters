#!/usr/bin/python

"""
Python code to ingest netCDF4 or HDF5 COWVR data
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

import pdb

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

# globals
ISS_COWVR_WMO_sat_ID = 806
ISS_TEMPEST_WMO_sat_ID = 922

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

GlobalAttrs = {
    "platformCommonName": "COWVR",
    "platformLongDescription": "COWVR Brightness Temperature Data",
    "sensorCentralFrequency": [18.7,
                               23.8,
                               33.9,],
}

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
    # report time
    toc = record_time(tic=tic)

    nlocs_int = np.array(len(obs_data[('latitude', metaDataName)]), dtype='int64')
    nlocs = nlocs_int.item()

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs['datetimeRange'] = np.array([datetime.fromtimestamp(obs_data[('dateTime', metaDataName)][0], timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
                                            datetime.fromtimestamp(obs_data[('dateTime', metaDataName)][-1], timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")],
                                            dtype=object)
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
    VarAttrs[('solarZenithAngle', metaDataName)]['units'] = 'degree'
    VarAttrs[('sensorAzimuthAngle', metaDataName)]['units'] = 'degree'
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


def get_data_from_files(zfiles):

    # allocate space for output depending on which variables are to be saved
    obs_data = init_obs_loc()

    # for afile in zfiles:
    afile = zfiles
    obs_data = get_cowvr_data(afile, obs_data)

    return obs_data


def get_cowvr_data(afile, obs_data, add_qc=True):

    f = h5py.File(afile, 'r')

    WMO_sat_ID = get_WMO_satellite_ID(f.filename)

    # "Geolocation and flags"
    # fore_aft = np.array(f['GeolocationAndFlags']['fore_aft_flag'], dtype='float32')
    sensor_altitude = np.array(f['GeolocationAndFlags']['sat_alt'], dtype='float32')
    sat_alt_flag = np.array(f['GeolocationAndFlags']['sc_att_flag'], dtype='int32')
    obs_data[('latitude', metaDataName)] = np.array(f['GeolocationAndFlags']['obs_lat'], dtype='float32')
    obs_data[('longitude', metaDataName)] = np.array(f['GeolocationAndFlags']['obs_lon'], dtype='float32')
    obs_data[('channelNumber', metaDataName)] = np.array(np.arange(12)+1, dtype='int32')

    obs_data[('solar_zenith_angle', metaDataName)] = np.array(f['GeolocationAndFlags']['sat_solar_zen'], dtype='float32')
    obs_data[('solar_azimuth_angle', metaDataName)] = np.array(f['GeolocationAndFlags']['sat_solar_az'], dtype='float32')
    obs_data[('sensor_zenith_angle', metaDataName)] = np.array(f['GeolocationAndFlags']['earth_inc_ang'], dtype='float32')
    obs_data[('sensor_azimuth_angle', metaDataName)] = np.array(f['GeolocationAndFlags']['earth_az_ang'], dtype='float32')
    obs_data[('sensor_view_angle', metaDataName)] = compute_scan_angle(
        np.array(f['GeolocationAndFlags']['instr_scan_ang'], dtype='float32'),
        sensor_altitude, sat_alt_flag,
        np.array(f['GeolocationAndFlags']['earth_inc_ang'], dtype='float32'))
    obs_data[('scan_position', metaDataName)] = np.array(np.round(f['GeolocationAndFlags']['instr_scan_ang']), dtype='float32')

    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteId', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    obs_data[('dateTime', metaDataName)] = np.array(get_epoch_time(f['GeolocationAndFlags']['time_string']), dtype='int64')
    qc_flag = f['CalibratedSceneTemperatures']['obs_qual_flag']
    solar_array_flag = f['CalibratedSceneTemperatures']['solar_array_flag']
    support_arm_flag = f['CalibratedSceneTemperatures']['support_arm_flag']

    nchans = len(obs_data[('channelNumber', metaDataName)])
    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('brightnessTemperature', obsValName)] = np.array(
        np.column_stack((f['CalibratedSceneTemperatures']['tb18_cfov'],
            f['CalibratedSceneTemperatures']['tb23_cfov'],
            f['CalibratedSceneTemperatures']['tb34_cfov'])), dtype='float32')
    obs_data[('brightnessTemperature', obsErrName)] = np.full((nlocs, nchans), 5.0, dtype='float32')
    obs_data[('brightnessTemperature', qcName)] = np.full((nlocs, nchans), 0, dtype='int32')

    if add_qc:
        obs_data = cowvr_gross_quality_control(obs_data, qc_flag, solar_array_flag, support_arm_flag)

    f.close()

    return obs_data


def cowvr_gross_quality_control(obs_data, qc_flag, solar_array_flag, support_arm_flag):

    tb_key = 'brightnessTemperature'
    good = \
        (obs_data[(tb_key,obsValName)][:,0] > 10) & (obs_data[(tb_key,obsValName)][:,0] < 400) & \
        (obs_data[(tb_key,obsValName)][:,4] > 10) & (obs_data[(tb_key,obsValName)][:,4] < 400) & \
        (obs_data[(tb_key,obsValName)][:,8] > 10) & (obs_data[(tb_key,obsValName)][:,8] < 400) & \
        (obs_data[('latitude',metaDataName)] >= -90) & (obs_data[('latitude',metaDataName)] <= 90) & \
        (qc_flag[:] == 0) & (solar_array_flag[:] == 0) & (support_arm_flag[:] == 0)

    for k in obs_data:
        if "MetaData" in k[1] and 'channelNumber' not in k[0]:
            obs_data[k] = obs_data[k][good][::33]
        elif tb_key in k[0]:
            obs_data[k] = obs_data[k][good,:][::33]

    return obs_data


def compute_scan_angle(instr_scan_ang, sensor_altitude, sat_alt_flag, sensor_zenith):

    earth_mean_radius_km = 6378.1370  # WGS84
    iss_altitude_km = 408.
    d2r = np.pi/180.
    r2d = 180./np.pi
    # compute default scan angle
    ratio = np.empty_like(sensor_altitude)
    ratio[:] = earth_mean_radius_km/(earth_mean_radius_km + iss_altitude_km)      # used a fixed value if not present in data
    # compute scan angle
    good = sat_alt_flag[:] == 0
    if sum(good) > 0:
        ratio[good] = earth_mean_radius_km/(earth_mean_radius_km + sensor_altitude[good]/1000.)

    # γ = arcsin(R / (R + h) * sin(theta)),h: sat alt; theta: sat zenith angle
    scanang = np.arcsin(ratio*np.sin(abs(sensor_zenith)*d2r))*r2d

    return scanang


def get_WMO_satellite_ID(filename):

    afile = os.path.basename(filename)
    WMO_sat_ID = ISS_COWVR_WMO_sat_ID

    return WMO_sat_ID


def get_epoch_time(obs_time_iso):

    this_datetime = [datetime.fromisoformat(adate.decode("utf-8")[:-5]) for adate in obs_time_iso]
    time_offset = [round((adatetime - epoch).total_seconds()) for adatetime in this_datetime]

    return time_offset


def get_string_dtg(obs_time_utc):

    dtg = []
    for adate in obs_time_utc:
        cdtg = adate[:-5].decode("utf-8") + 'Z'
        if "655" in cdtg:
            cdtg = ("%4i-%.2i-%.2iT%.2i:%.2i:00Z" % (2200, 1, 1, 0, 0))
        dtg.append(cdtg)

    return dtg


def init_obs_loc():
    obs = {
        ('brightnessTemperature', obsValName): [],
        ('brightnessTemperature', obsErrName): [],
        ('brightnessTemperature', qcName): [],
        ('satelliteId', metaDataName): [],
        ('channelNumber', metaDataName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('scan_position', metaDataName): [],
        ('solar_zenith_angle', metaDataName): [],
        ('solar_azimuth_angle', metaDataName): [],
        ('sensor_zenith_angle', metaDataName): [],
        ('sensor_view_angle', metaDataName): [],
        ('sensor_azimuth_angle', metaDataName): [],
    }

    return obs


#----------------------------------------------------------------------
# Time function
#----------------------------------------------------------------------
def record_time(tic=None, print_log=True):

    if not tic:
        tic = time.perf_counter()
        if print_log: print(f"  ... starting timer: {tic:0.3f}")
        return tic
    else:
        toc = time.perf_counter()
        if print_log: print(f"  ... elapsed time (sec): {toc - tic:0.3f}")
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
        type=str, default=os.getcwd())

    args = parser.parse_args()

    main(args)
