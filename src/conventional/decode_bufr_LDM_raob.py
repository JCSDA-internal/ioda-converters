#!/usr/bin/python

from datetime import datetime, timedelta
import dateutil.parser
import os
from pathlib import Path
import sys

import numpy as np
import netCDF4 as nc
from eccodes import *
from multiprocessing import Pool

#from IPython import embed as shell

# set path to ioda_conv_engines module
IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

# These modules need the path to lib-python modules
import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

ioda_float_type = 'float32'
ioda_int_type = 'int32'
float_missing_value = -1.0e+38
int_missing_value = -2147483647

def main(file_name, output_file):

    filenames = [ file_name ]
    # initialize
    count = [0, 0]
    start_pos = None

    # read / process files in parallel
    # pool = Pool(1)
    # pool_inputs = [(i, count, start_pos) for i in filenames]

    # obs = pool.map(read_file, pool_inputs)

    # obs_data, count, start_pos = obs[0]
    obs_data, count, start_pos = read_file(file_name, count, start_pos)

    # print ( "number of valid mssg: ", count[0] )
    # print ( "number of invalid mssg: ", count[1] )

    attr_data = {}
    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
#   dtg = datetime.strptime(cdtg, '%Y%m%d%H')
#   attr_data['date_time_string'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    attr_data['converter'] = os.path.basename(__file__)

    GlobalAttrs = {}
    VarDims = {
        'altitude'              :  ['nlocs'],
        'windDirection'         :  ['nlocs'],
        'windSpeed'             :  ['nlocs'],
        'temperatureAir'        :  ['nlocs'],
        'temperatureDewpoint'   :  ['nlocs'],
}

    # write them out
    nlocs = obs_data[('altitude', 'ObsValue')].shape[0]
    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('altitude', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('wind_direction', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('wind_speed', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('air_temperature', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('dew_point_temperature', "ObsValue")]['_FillValue'] = float_missing_value

    VarAttrs[('latitude', "MetaData")]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', "MetaData")]['_FillValue'] = float_missing_value
    VarAttrs[('air_pressure', "MetaData")]['_FillValue'] = float_missing_value

    # VarAttrs[('altitude', 'ObsValue')]['units'] = 'Radians'
    # VarAttrs[('altitude', 'ObsError')]['units'] = 'Radians'
    # VarAttrs[('altitude', 'PreQC')]['units']    = 'unitless'

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)

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
        msg_data, count, start_pos = read_bufr_message( f, count, start_pos )

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

        if start_pos == None:
            # print ( "start_pos: ", start_pos )
            break

    return obs_data, count, start_pos


def get_meta_data(bufr):

    # get per raob profile global attributes
    meta_data_keys = def_meta_data()

    # these are the MetaData we are interested in
    profile_meta_data = {}
    for k, v in meta_data_keys.items():
        profile_meta_data[k] = codes_get(bufr, v)

    # do the hokey time structure to time structure
    year  = codes_get(bufr, 'year')
    month = codes_get(bufr, 'month')
    day   = codes_get(bufr, 'day')
    hour  = codes_get(bufr, 'hour')
    minute = codes_get(bufr, 'minute')
    second = codes_get(bufr, 'second')  #  non-integer value

    # should really add seconds
    dtg = ( "%4i-%.2i-%.2iT%.2i:%.2i:00Z" % (year, month, day, hour, minute) )
    profile_meta_data['datetime'] = dtg
    # do not see a different launch time in the BUFR file
    profile_meta_data['sondeLaunchTime'] = dtg

    return profile_meta_data

def def_meta_data():

    meta_data_keys = {
 "stationIdWMOblock"                     : 'blockNumber',
 "stationIdWMOstation"                   : 'stationNumber',
 "stationLongName"                       : 'shipOrMobileLandStationIdentifier',
 "instrumentType"                        : 'radiosondeType',
 "stationElevation"                      : 'heightOfBarometerAboveMeanSeaLevel',
 "instrumentSerialNum"                   : 'radiosondeSerialNumber',
 "instrumentSoftwareVersion"             : 'softwareVersionNumber',
 "instrumentHumidityCorrectionInfo"      : 'correctionAlgorithmsForHumidityMeasurements',
 "instrumentRadiationCorrectionInfo"     : 'solarAndInfraredRadiationCorrection',
}

    return meta_data_keys


def assign_values(data):
    if data.dtype == float:
        data[np.abs(data) >= np.abs(float_missing_value)] = float_missing_value
        return np.array(data, dtype=ioda_float_type)
    elif data.dtype == int:
        data[np.abs(data) >= np.abs(int_missing_value)] = int_missing_value
        return np.array(data, dtype=ioda_int_type)
    elif data.dtype.kind in { 'U', 'S' }:
        return np.array(data, dtype=object)


def read_bufr_message( f, count, start_pos ):

    obs_data = {}
    if start_pos == f.tell():
        return obs_data, count, None
    start_pos = f.tell()
    # print ( "starting pos: ", f.tell() )

    try:
        bufr = codes_bufr_new_from_file( f )

        codes_set(bufr, 'unpack', 1)

        profile_meta_data = get_meta_data(bufr)

        # replication factors for the datasets but these do not have number of levels?
        krepfac = codes_get_array(bufr, 'extendedDelayedDescriptorReplicationFactor')
        drepfac = codes_get_array(bufr, 'delayedDescriptorReplicationFactor')

        lat = codes_get(bufr, 'latitude', ktype=float)
        lon = codes_get(bufr, 'longitude', ktype=float)
        lat_displacement = codes_get_array(bufr, 'latitudeDisplacement', ktype=float)
        lon_displacement = codes_get_array(bufr, 'longitudeDisplacement', ktype=float)
        time_displacement = codes_get_array(bufr, 'timePeriod', ktype=float)
        wind_direction = codes_get_array(bufr, 'windDirection', ktype=float)
        wind_speed = codes_get_array(bufr, 'windSpeed', ktype=float)
        temp_air = codes_get_array(bufr, 'airTemperature', ktype=float)
        temp_dewpoint = codes_get_array(bufr, 'dewpointTemperature', ktype=float)
        pressure = codes_get_array(bufr, 'pressure', ktype=float)
        geop_height = codes_get_array(bufr, 'nonCoordinateGeopotentialHeight', ktype=float)

        codes_release( bufr )

        num_levels        = len( lat_displacement )

        # what to do for "ObsError" set to 1? and "PreQC" all zero?
        obs_data[('altitude', "ObsValue")] = assign_values(geop_height)
        obs_data[('wind_direction', "ObsValue")] = assign_values(wind_direction)
        obs_data[('wind_speed', "ObsValue")] = assign_values(wind_speed)
        obs_data[('air_temperature', "ObsValue")] = assign_values(temp_air)
        obs_data[('dew_point_temperature', "ObsValue")] = assign_values(temp_dewpoint)

        obs_data[('latitude', "MetaData")] = np.full(num_levels, lat, dtype='float32') + assign_values(lat_displacement)
        obs_data[('longitude', "MetaData")] = np.full(num_levels, lon, dtype='float32') + assign_values(lon_displacement)
        obs_data[('air_pressure', "MetaData")] = assign_values(pressure)
        for k, v in profile_meta_data.items():
            vals = np.repeat(v, krepfac[0])
            obs_data[(k, "MetaData")] = assign_values(vals)

        # print ( " decoded raob num_lev: ", len(obs_data[('latitude',"MetaData")]) )
        # print ( "ending pos: ", f.tell() )
        count[0] += 1

        return obs_data, count, start_pos

    except:
        # print ( "invalid bufr message" )
        count[1] += 1
        # print ( "number of valid mssg: ", count[0] )
        # print ( "number of invalid mssg: ", count[1] )
        return obs_data, count, start_pos
        pass

if __name__ == "__main__":

    from optparse import OptionParser

    usage = 'usage: %prog -i input-file -o output-file'
    parser = OptionParser(usage)
    parser.add_option('-i', '--input-file', dest='file_name',
                      action='store', default=None,
                      help='input file')
    parser.add_option('-o', '--output-file', dest='output_file',
                      action='store', default=None,
                      help='input file')
    (options, args) = parser.parse_args()

    if not os.path.isfile(options.file_name):
        parser.error('File does not exist, please enter valid input file with the -i option.')

    main(options.file_name, options.output_file)
