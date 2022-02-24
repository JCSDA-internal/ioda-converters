#!/usr/bin/python

from datetime import datetime, timedelta
import dateutil.parser
import os
from pathlib import Path
import sys
import time

import numpy as np
import netCDF4 as nc

# set path to ioda_conv_engines module
IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

# These modules need the path to lib-python modules
import ioda_conv_engines as iconv
import meteo_utils
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "integer")
]

ioda_float_type = 'float32'
ioda_int_type = 'int32'
ioda_long_type = 'int64'
float_missing_value = -1.0e+38
int_missing_value = -2147483647
long_missing_value = -9223372036854775806

ioda_datetime_epoch = datetime(1970, 1, 1, 0, 0, 0)  # Jan 1, 1970 00Z


def main(file_names, output_file):

    obs_data = {}
    for fname in file_names:
        print("INFO: Reading file: ", fname)
        file_obs_data, count, start_pos = read_file(fname, count, start_pos)
        if obs_data:
            concat_obs_dict(obs_data, file_obs_data)
        else:
            obs_data = file_obs_data

    if not obs_data:
        print("WARNING: no message data has been captured, stopping execution.")
        sys.exit()

    attr_data = {}
    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
#   dtg = datetime.strptime(cdtg, '%Y%m%d%H')
#   attr_data['date_time_string'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    attr_data['converter'] = os.path.basename(__file__)

    GlobalAttrs = {
        "platformCommonName":  "EUMETSAT_AMV",
        "platformLongDescription":  "EUMETSAT AMV from IR cloudy regions",
    }
    VarDims = {
        'windEastward': ['nlocs'],
        'windNorthward': ['nlocs'],
    }

#   MetaData_keys = {
#       'satelliteId': ['nlocs'],
#       'sensorZenithAngle': ['nlocs'],
#       'pressureAir': ['nlocs'],
#       'windPercentConfidence': ['nlocs'],
#       'sensorCentralFrequency': ['nlocs'],
#   }

    # write them out
    print("INFO: Writing file: ", output_file)
    nlocs = obs_data[('windNorthward', 'ObsValue')].shape[0]
    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('windEastward', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('windNorthward', "ObsValue")]['_FillValue'] = float_missing_value

    VarAttrs[('latitude', "MetaData")]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', "MetaData")]['_FillValue'] = float_missing_value
    VarAttrs[('pressureAir', "MetaData")]['_FillValue'] = float_missing_value

    epoch_units = "seconds since {0}Z".format(ioda_datetime_epoch.isoformat('T'))
    VarAttrs[('dateTime', "MetaData")]['units'] = epoch_units

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)

    print("--- %s total seconds ---" % (time.time() - start_time))


def concat_obs_dict(obs_data, append_obs_data):
    # For now we are assuming that the obs_data dictionary has the "golden" list
    # of variables. If one is missing from append_obs_data, the obs_data variable
    # will be extended using fill values.
    #
    # Use the first key in the append_obs_data dictionary to determine how
    # long to make the fill value vector.
    append_keys = list(append_obs_data.keys())
    append_length = len(append_obs_data[append_keys[0]])
    for gv_key in obs_data.keys():
        if gv_key in append_keys:
            obs_data[gv_key] = np.append(obs_data[gv_key], append_obs_data[gv_key])
        else:
            if obs_data[gv_key].dtype == float:
                fill_data = np.repeat(float_missing_value, append_length, dtype=ioda_float_type)
            elif obs_data[gv_key].dtype == int:
                fill_data = np.repeat(int_missing_value, append_length, dtype=ioda_int_type)
            elif obs_data[gv_key].dtype == object:
                # string type, extend with empty strings
                fill_data = np.repeat("", append_length, dtype=object)
            obs_data[gv_key] = np.append(obs_data[gv_key], fill_data)


def read_file(file_name):

    obs_data = {}
    f = open(file_name, 'rb')

 #type   sat      day     hms     lat     lon     pre   spd   dir   rff    qi  int
#WVCA   GOES17  20210801  0000   -8.00   175.49   287    6.7  227  68.29  0.00  30
#WVCA   GOES17  20210801  0000   -8.10   173.58   287    9.1  239  69.44  0.00  30
#WVCA   GOES17  20210801  0000   -8.00   172.53   312   11.7  235  53.40  0.00  30

    obs_data = {}

    # get the message reference values
    lat = codes_get(bufr, 'latitude', ktype=float)
    lon = codes_get(bufr, 'longitude', ktype=float)
    year = codes_get(bufr, 'year')
    month = codes_get(bufr, 'month')
    day = codes_get(bufr, 'day')
    hour = codes_get(bufr, 'hour')
    minute = codes_get(bufr, 'minute')
    second = codes_get(bufr, 'second')  # non-integer value
    msg_datetime_ref = datetime(year, month, day, hour, minute, second)

    wind_direction = codes_get_array(bufr, 'windDirection', ktype=float)
    wind_speed = codes_get_array(bufr, 'windSpeed', ktype=float)
    temp_air = codes_get_array(bufr, 'airTemperature', ktype=float)
    temp_dewpoint = codes_get_array(bufr, 'dewpointTemperature', ktype=float)
    pressure = codes_get_array(bufr, 'pressure', ktype=float)
    geop_height = codes_get_array(bufr, 'nonCoordinateGeopotentialHeight', ktype=float)

    # what to do for "ObsError" set to 1? and "PreQC" all zero?
    obs_data[('surface_pressure', "ObsValue")] = np.full(num_levels, surface_pressure, dtype='float32')
    obs_data[('geopotential_height', "ObsValue")] = assign_values(geop_height)
    obs_data[('air_temperature', "ObsValue")] = assign_values(temp_air)
    obs_data[('eastward_wind', "ObsValue")] = assign_values(eastward_wind)
    obs_data[('northward_wind', "ObsValue")] = assign_values(northward_wind)
    obs_data[('specific_humidity', "ObsValue")] = assign_values(specific_humidity)

    obs_data[('latitude', "MetaData")] = np.full(num_levels, lat, dtype='float32') + assign_values(lat_displacement)
    obs_data[('longitude', "MetaData")] = np.full(num_levels, lon, dtype='float32') + assign_values(lon_displacement)
    obs_data[('air_pressure', "MetaData")] = assign_values(pressure)

    # get the datetime variables in the new epoch style representation
    obs_dtime, obs_launch_time = get_dtime_offsets(ioda_datetime_epoch, msg_datetime_ref, time_displacement)
    # second argument to assign_values (which is False by default) tells
    # assign_values if it's okay to keep the incoming integer datatype
    # at 64 bits.
    obs_data[('dateTime', "MetaData")] = assign_values(obs_dtime, True)

    # put all the other MetaData repeating nlocs times
    for k, v in profile_meta_data.items():
        vals = np.repeat(v, num_levels)
        obs_data[(k, "MetaData")] = assign_values(vals)

    return obs_data


def get_meta_data(bufr):

    # get per raob profile global attributes
    meta_data_keys = def_meta_data()

    # these are the MetaData we are interested in
    profile_meta_data = {}
    for k, v in meta_data_keys.items():
        profile_meta_data[k] = codes_get(bufr, v)

    profile_meta_data = get_station_elevation(bufr, profile_meta_data)
    profile_meta_data = get_station_id(profile_meta_data)

    return profile_meta_data


def get_station_id(profile_meta_data):

    k = 'station_id'
    wmo_block_id = profile_meta_data["stationIdWMOblock"]
    wmo_station_id = profile_meta_data["stationIdWMOstation"]
    profile_meta_data[k] = '{0:02}'.format(wmo_block_id) + '{0:03}'.format(wmo_station_id)

    return profile_meta_data

def def_meta_data():

    meta_data_keys = {
        "stationIdWMOblock": 'blockNumber',
        "stationIdWMOstation": 'stationNumber',
        "stationLongName": 'shipOrMobileLandStationIdentifier',
        "instrumentType": 'radiosondeType',
        "instrumentSerialNum": 'radiosondeSerialNumber',
        "instrumentSoftwareVersion": 'softwareVersionNumber',
        "instrumentHumidityCorrectionInfo": 'correctionAlgorithmsForHumidityMeasurements',
        "instrumentRadiationCorrectionInfo": 'solarAndInfraredRadiationCorrection',
    }

    return meta_data_keys

def get_dtime_offsets(epoch, msg_ref, time_offset):
    nlocs = len(time_offset)
    ref_offset = (msg_ref - epoch).total_seconds()
    obs_dtime = np.full(nlocs, ref_offset, dtype='int64') + time_offset.astype('int64')
    obs_launch_time = np.repeat(ref_offset, nlocs).astype('int64')
    return obs_dtime, obs_launch_time


def assign_values(data, allow_long=False):
    if data.dtype == float:
        # test above matches any precision float
        # force to ioda_float_type (which is 32-bit for now)
        data[np.abs(data) >= np.abs(float_missing_value)] = float_missing_value
        return np.array(data, dtype=ioda_float_type)
    elif data.dtype == int:
        # test above matches any precision integer
        if (data.dtype == 'int64') and (allow_long):
            data[np.abs(data) >= np.abs(long_missing_value)] = long_missing_value
            return np.array(data, dtype=ioda_long_type)
        else:
            data[np.abs(data) >= np.abs(int_missing_value)] = int_missing_value
            return np.array(data, dtype=ioda_int_type)
    elif data.dtype.kind in {'U', 'S'}:
        return np.array(data, dtype=object)



if __name__ == "__main__":

    from argparse import ArgumentParser

    parser = ArgumentParser(
        description=(
            'Read a raob BUFR file and convert into IODA output file')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument('-i', '--input-files', nargs='+', dest='file_names',
                          action='store', default=None, required=True,
                          help='input files')
    required.add_argument('-o', '--output-file', dest='output_file',
                          action='store', default=None, required=True,
                          help='output file')
    args = parser.parse_args()

    for file_name in args.file_names:
        if not os.path.isfile(file_name):
            parser.error('Input (-i option) file: ', file_name, ' does not exist')

    main(args.file_names, args.output_file)
