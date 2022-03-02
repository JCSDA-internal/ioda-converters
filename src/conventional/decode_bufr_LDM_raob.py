#!/usr/bin/python

from datetime import datetime, timedelta
import dateutil.parser
import os
from pathlib import Path
import sys
import time

import numpy as np
import netCDF4 as nc
from eccodes import *
from multiprocessing import Pool

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
    ("datetime", "string")
]

ioda_float_type = 'float32'
ioda_int_type = 'int32'
ioda_long_type = 'int64'
float_missing_value = -1.0e+38
int_missing_value = -2147483647
long_missing_value = -9223372036854775806

ioda_datetime_epoch = datetime(1970, 1, 1, 0, 0, 0)  # Jan 1, 1970 00Z


def main(file_names, output_file):

    # initialize
    count = [0, 0]
    start_pos = None
    start_time = time.time()

    # read / process files in parallel
    # pool = Pool(1)
    # pool_inputs = [(i, count, start_pos) for i in filenames]

    # obs = pool.map(read_file, pool_inputs)

    # obs_data, count, start_pos = obs[0]
    obs_data = {}
    for fname in file_names:
        print("INFO: Reading file: ", fname)
        file_obs_data, count, start_pos = read_file(fname, count, start_pos)
        if file_obs_data:
            if obs_data:
                concat_obs_dict(obs_data, file_obs_data)
            else:
                obs_data = file_obs_data
        else:
            print("WARNING: no message data has been captured: ", fname)

    if not obs_data:
        print("WARNING: no message data has been captured, stopping execution.")
        sys.exit()

    print("--- %s BUFR read seconds ---" % (time.time() - start_time))
    # print ( "number of valid mssg: ", count[0] )
    # print ( "number of invalid mssg: ", count[1] )

    attr_data = {}
    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
#   dtg = datetime.strptime(cdtg, '%Y%m%d%H')
#   attr_data['date_time_string'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    attr_data['converter'] = os.path.basename(__file__)

    GlobalAttrs = {}
# V2 'windDirection': ['nlocs'],
# V2 'windSpeed': ['nlocs'],
# V2 'temperatureAir': ['nlocs'],
# V2 'temperatureDewpoint': ['nlocs'],
    VarDims = {
        'geopotential_height': ['nlocs'],
        'air_temperature': ['nlocs'],
        'eastward_wind': ['nlocs'],
        'northward_wind': ['nlocs'],
        'specific_humidity': ['nlocs'],
        'surface_pressure': ['nlocs'],
    }

    # write them out
    print("INFO: Writing file: ", output_file)
    nlocs = obs_data[('geopotential_height', 'ObsValue')].shape[0]
    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('geopotential_height', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('air_temperature', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('eastward_wind', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('northward_wind', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('specific_humidity', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('surface_pressure', "ObsValue")]['_FillValue'] = float_missing_value

    VarAttrs[('latitude', "MetaData")]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', "MetaData")]['_FillValue'] = float_missing_value
    VarAttrs[('air_pressure', "MetaData")]['_FillValue'] = float_missing_value

    epoch_units = "seconds since {0}Z".format(ioda_datetime_epoch.isoformat('T'))
    VarAttrs[('dateTime', "MetaData")]['units'] = epoch_units
    VarAttrs[('LaunchTime', "MetaData")]['units'] = epoch_units

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


def read_file(file_name, count, start_pos):

    obs_data = {}
    f = open(file_name, 'rb')

    while True:
        # here is where to send to ecCodes
        msg_data, count, start_pos = read_bufr_message(f, count, start_pos)

        # only record msg_data if it is not empty
        if (msg_data):
            # If obs_data is empty, simply copy msg_data
            # Otherwise, append the elements in msg_data to the
            #    corrsponding elements in obs_data
            if (obs_data):
                # Append
                concat_obs_dict(obs_data, msg_data)
            else:
                # Copy
                obs_data = msg_data

        if start_pos is None:
            # print("start_pos: ", start_pos)
            break

    return obs_data, count, start_pos


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


def get_station_elevation(bufr, profile_meta_data):

    # try to progressively get more detailed height of instrument
    #   prefer: heightOfStationGroundAboveMeanSeaLevel
    #   then:   heightOfBarometerAboveMeanSeaLevel
    #   then:   height
    k = 'station_elevation'

    station_height = codes_get(bufr, 'heightOfStationGroundAboveMeanSeaLevel', ktype=float)
    if abs(station_height) <= abs(float_missing_value):
        profile_meta_data[k] = station_height
    else:
        station_height = codes_get(bufr, 'heightOfBarometerAboveMeanSeaLevel', ktype=float)
        if abs(station_height) <= abs(float_missing_value):
            profile_meta_data[k] = station_height
        else:
            profile_meta_data[k] = codes_get(bufr, 'height', ktype=float)

    return profile_meta_data


def def_data_keys():

    data_keys = {
        'nonCoordinateGeopotentialHeight': 'geopotential_height',
        'windDirection': 'wind_direction',
        'windSpeed': 'wind_speed',
        'airTemperature': 'air_temperature',
        'dewpointTemperature': 'dew_point_temperature',
    }

# these are all dervied fields
#   ('air_pressure', "MetaData")
#   ('surface_pressure', "ObsValue")
#   ('eastward_wind', "ObsValue")
#   ('northward_wind', "ObsValue")
#   ('specific_humidity', "ObsValue")

#   ('latitude', "MetaData")
#   ('longitude', "MetaData")

    return data_keys


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


def def_significance_table():

    significance_table = {
        "pressure level originally indicated by height as the veritcal coordinate": 1,
        "freezing level": 2,
        "level determined by regional decision": 3,
        "top of wind sounding": 4,
        "end of missing wind data": 5,
        "beginning of missing wind data": 6,
        "end of missing humidity data": 7,
        "beginning of missing humidity data": 8,
        "end of missing temperature data": 9,
        "beginning of missing temperature data": 10,
        "significant wind level": 11,
        "significant humidity level": 12,
        "significant temperature level": 13,
        "maximum wind level": 14,
        "tropopause level": 15,
        "standard level": 16,
        "surface": 17,
    }

    return significance_table


def get_dtime_offsets(epoch, msg_ref, time_offset):
    nlocs = len(time_offset)
    ref_offset = (msg_ref - epoch).total_seconds()
    obs_dtime = np.full(nlocs, ref_offset, dtype='int64') + time_offset.astype('int64')
    obs_launch_time = np.repeat(ref_offset, nlocs).astype('int64')
    return obs_dtime, obs_launch_time


def get_normalized_bit(value, bit_index):
    return (value >> bit_index) & 1


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


def read_bufr_message(f, count, start_pos):

    obs_data = {}
    call_fail = False
    if start_pos == f.tell():
        return obs_data, count, None
    start_pos = f.tell()
    # print ( "starting pos: ", f.tell() )

    met_utils = meteo_utils.meteo_utils()

    try:
        bufr = codes_bufr_new_from_file(f)

        codes_set(bufr, 'unpack', 1)

        profile_meta_data = get_meta_data(bufr)

        # replication factors for the datasets but these do not have number of levels?
        krepfac = codes_get_array(bufr, 'extendedDelayedDescriptorReplicationFactor')
        drepfac = codes_get_array(bufr, 'delayedDescriptorReplicationFactor')

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

        # get the message displacements
        lat_displacement = codes_get_array(bufr, 'latitudeDisplacement', ktype=float)
        lon_displacement = codes_get_array(bufr, 'longitudeDisplacement', ktype=float)
        time_displacement = codes_get_array(bufr, 'timePeriod', ktype=int)

        wind_direction = codes_get_array(bufr, 'windDirection', ktype=float)
        wind_speed = codes_get_array(bufr, 'windSpeed', ktype=float)
        temp_air = codes_get_array(bufr, 'airTemperature', ktype=float)
        temp_dewpoint = codes_get_array(bufr, 'dewpointTemperature', ktype=float)
        pressure = codes_get_array(bufr, 'pressure', ktype=float)
        geop_height = codes_get_array(bufr, 'nonCoordinateGeopotentialHeight', ktype=float)
        sounding_significance = codes_get_array(bufr, 'extendedVerticalSoundingSignificance', ktype=int)

        significance_table = def_significance_table()

        # set a default surface pressure as the first level
        surface_pressure = pressure[0]

        #  is there a labled surface pressure via the sounding significance table
        if np.any(sounding_significance & 2**significance_table["surface"]):

            if np.sum(np.any(sounding_significance & 2**significance_table["surface"])) > 1:
                print(" ERROR too many surface levels in single BUFR mssg")
                call_fail = True
                print("starting position: %i" % start_pos)
                print("length pressure: %i" % len(pressure))
                print("length temperature: %i" % len(temp_air))
                print("length dewpoint: %i" % len(temp_dewpoint))
                print("length geop_height: %i" % len(geop_height))
                print("length sound_sig: %i" % len(sounding_significance))
                print("length wind_dir: %i" % len(wind_direction))
                print("length wind_speed: %i" % len(wind_speed))
                print("length lat_displacement: %i" % len(lat_displacement))
                print("length lon_displacement: %i" % len(lon_displacement))
                print("length time_displacement: %i" % len(time_displacement))
                print("========")

            # index for surface pressure record
            surface_level = np.where(sounding_significance & 2**significance_table["surface"])[0][-1]
            surface_pressure = pressure[surface_level]

            # if these lengths all match no need to do anything
            if len(pressure) == len(temp_air) == len(lat_displacement):
                pass

            # typical case first record is surface pressure
            elif len(pressure) > len(temp_air):
                if (len(pressure) != len(lat_displacement)):
                    print(' ERROR handling geolocation displacement does not match P ', len(lat_displacement), len(pressure))
                    call_fail = True
                    raise BaseException
                elif surface_level != 0:
                    # surface level not first record
                    surface_not_first = True
                    first_level = surface_level
                    if (len(temp_air) == len(pressure)):
                        temp_air = temp_air[first_level:]
                        temp_dewpoint = temp_dewpoint[first_level:]
                        geop_height = geop_height[first_level:]
                        wind_direction = wind_direction[first_level:]
                        wind_speed = wind_speed[first_level:]
                    pressure = pressure[first_level:]
                    lat_displacement = lat_displacement[first_level:]
                    lon_displacement = lon_displacement[first_level:]
                    time_displacement = time_displacement[first_level:]
                    sounding_significance = sounding_significance[first_level:]
                elif surface_level == 0:
                    # surface level is first record
                    pressure = pressure[1:]
                    lat_displacement = lat_displacement[1:]
                    lon_displacement = lon_displacement[1:]
                    time_displacement = time_displacement[1:]
                    sounding_significance = sounding_significance[1:]

            if len(pressure) != len(temp_air):
                var_length_check = True
                var_length_check = var_length_check and (len(temp_air) == len(temp_dewpoint) == len(geop_height) == len(wind_direction) == len(wind_speed))
                var_length_check = var_length_check and \
                    (len(pressure) == len(sounding_significance) == len(lat_displacement) == len(lon_displacement) == len(time_displacement))
                if (var_length_check):
                    new_length = min([len(pressure), len(temp_air)])
                    pressure = pressure[:new_length]
                    lat_displacement = lat_displacement[:new_length]
                    lon_displacement = lon_displacement[:new_length]
                    time_displacement = time_displacement[:new_length]
                    sounding_significance = sounding_significance[:new_length]
                    # add these too?
                    temp_air = temp_air[:new_length]
                    temp_dewpoint = temp_dewpoint[:new_length]
                    geop_height = geop_height[:new_length]
                    wind_direction = wind_direction[:new_length]
                    wind_speed = wind_speed[:new_length]
                else:
                    print(' ERROR variables have too random a distribution of lengths ')
                    print("starting position: %i" % start_pos)
                    print("length pressure: %i" % len(pressure))
                    print("length temperature: %i" % len(temp_air))
                    print("length dewpoint: %i" % len(temp_dewpoint))
                    print("length geop_height: %i" % len(geop_height))
                    print("length sound_sig: %i" % len(sounding_significance))
                    print("length wind_dir: %i" % len(wind_direction))
                    print("length wind_speed: %i" % len(wind_speed))
                    print("length lat_displacement: %i" % len(lat_displacement))
                    print("length lon_displacement: %i" % len(lon_displacement))
                    print("length time_displacement: %i" % len(time_displacement))
                    print("========")
#                   call_fail = True
                    raise BaseException

#  ===========================================================================================
#  ===========================================================================================
#  ============    at this point assume arrays have been aligned and all have nlocs    =======
#  ===========================================================================================
#  ===========================================================================================

        if len(lat_displacement) != len(temp_air) != len(pressure):
            print(" need more error handling mis-matched pressure to air temperature")
            call_fail = True
            print("starting position: %i" % start_pos)
            print("length pressure: %i" % len(pressure))
            print("length temperature: %i" % len(temp_air))
            print("length dewpoint: %i" % len(temp_dewpoint))
            print("length geop_height: %i" % len(geop_height))
            print("length sound_sig: %i" % len(sounding_significance))
            print("length wind_dir: %i" % len(wind_direction))
            print("length wind_speed: %i" % len(wind_speed))
            print("length lat_displacement: %i" % len(lat_displacement))
            print("length lon_displacement: %i" % len(lon_displacement))
            print("length time_displacement: %i" % len(time_displacement))
            print("========")

        # skip records with bad sounding significance values
        if np.any(sounding_significance == np.abs(int_missing_value)):     # 2147483647
            valid_level = np.where(sounding_significance != np.abs(int_missing_value))
            lat_displacement = lat_displacement[valid_level]
            lon_displacement = lon_displacement[valid_level]
            time_displacement = time_displacement[valid_level]
            wind_direction = wind_direction[valid_level]
            wind_speed = wind_speed[valid_level]
            temp_air = temp_air[valid_level]
            temp_dewpoint = temp_dewpoint[valid_level]
            pressure = pressure[valid_level]
            geop_height = geop_height[valid_level]
            sounding_significance = sounding_significance[valid_level]

        codes_release(bufr)

        num_levels = len(lat_displacement)
        if (len(lat_displacement) != len(time_displacement)):
            print("this should never happen lat and time have different displacements lengths")
            call_fail = True

        # Check to make sure all variables have the proper length. For now, skip
        # this message if this check fails.
        var_length_check = True
        var_length_check = var_length_check and (len(lat_displacement) == num_levels)
        var_length_check = var_length_check and (len(lon_displacement) == num_levels)
        var_length_check = var_length_check and (len(time_displacement) == num_levels)
        var_length_check = var_length_check and (len(temp_air) == num_levels)
        var_length_check = var_length_check and (len(temp_dewpoint) == num_levels)
        var_length_check = var_length_check and (len(pressure) == num_levels)
        var_length_check = var_length_check and (len(geop_height) == num_levels)
        var_length_check = var_length_check and (len(sounding_significance) == num_levels)
        var_length_check = var_length_check and (len(wind_direction) == num_levels)
        var_length_check = var_length_check and (len(wind_speed) == num_levels)
        if (not var_length_check):
            print(' ERROR: BUFR message variable lengths do not match')
            raise BaseException

        #  compute derived variables
        #   ... specific humidity
        specific_humidity = np.fromiter(map(met_utils.specific_humidity, temp_dewpoint, pressure), dtype='float64')

        #   ... and zonal and meridional wind
        wind = np.array(list(map(met_utils.dir_speed_2_uv, wind_direction, wind_speed)))
        eastward_wind = wind[:, 0]
        northward_wind = wind[:, 1]

        # what to do for "ObsError" set to 1? and "PreQC" all zero?
        obs_data[('surface_pressure', "ObsValue")] = np.full(num_levels, surface_pressure, dtype='float32')
        obs_data[('geopotential_height', "ObsValue")] = assign_values(geop_height)
        obs_data[('air_temperature', "ObsValue")] = assign_values(temp_air)
        obs_data[('eastward_wind', "ObsValue")] = assign_values(eastward_wind)
        obs_data[('northward_wind', "ObsValue")] = assign_values(northward_wind)
        obs_data[('specific_humidity', "ObsValue")] = assign_values(specific_humidity)

# V2    obs_data[('wind_direction', "ObsValue")] = assign_values(wind_direction)
# V2    obs_data[('wind_speed', "ObsValue")] = assign_values(wind_speed)
# V2    obs_data[('dew_point_temperature', "ObsValue")] = assign_values(temp_dewpoint)

        obs_data[('latitude', "MetaData")] = np.full(num_levels, lat, dtype='float32') + assign_values(lat_displacement)
        obs_data[('longitude', "MetaData")] = np.full(num_levels, lon, dtype='float32') + assign_values(lon_displacement)
        obs_data[('air_pressure', "MetaData")] = assign_values(pressure)

        # get the datetime variables in the new epoch style representation
        obs_dtime, obs_launch_time = get_dtime_offsets(ioda_datetime_epoch, msg_datetime_ref, time_displacement)
        # second argument to assign_values (which is False by default) tells
        # assign_values if it's okay to keep the incoming integer datatype
        # at 64 bits.
        obs_data[('dateTime', "MetaData")] = assign_values(obs_dtime, True)
        obs_data[('LaunchTime', "MetaData")] = assign_values(obs_launch_time, True)

        # put all the other MetaData repeating nlocs times
        for k, v in profile_meta_data.items():
            vals = np.repeat(v, num_levels)
            obs_data[(k, "MetaData")] = assign_values(vals)

        # print ( " decoded raob num_lev: ", len(obs_data[('latitude',"MetaData")]) )
        # print ( "ending pos: ", f.tell() )
        count[0] += 1

        return obs_data, count, start_pos

    except BaseException:
        # print ( "invalid bufr message" )
        if call_fail:
            # sys.exit()
            pass
        count[1] += 1
        # print ( "number of valid mssg: ", count[0] )
        # print ( "number of invalid mssg: ", count[1] )
        return obs_data, count, start_pos


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

    args.output_file = os.path.abspath(args.output_file)
    apath, afile = os.path.split(args.output_file)
    # create output directory path if necessary
    if not os.path.exists(apath):
        print("creating output directory: ", apath)
        os.makedirs(apath)

    main(args.file_names, args.output_file)
