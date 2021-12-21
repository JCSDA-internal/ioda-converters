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

def main(file_names, output_file):

    # initialize
    count = [0, 0]
    start_pos = None

    # read / process files in parallel
    # pool = Pool(1)
    # pool_inputs = [(i, count, start_pos) for i in filenames]

    # obs = pool.map(read_file, pool_inputs)

    # obs_data, count, start_pos = obs[0]
    obs_data = {}
    for fname in file_names:
        print("INFO: Reading file: ", fname)
        file_obs_data, count, start_pos = read_file(fname, count, start_pos)
        if obs_data:
            concat_obs_dict(obs_data, file_obs_data)
        else:
            obs_data = file_obs_data

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
        'geopotential_height'              :  ['nlocs'],
        'windDirection'         :  ['nlocs'],
        'windSpeed'             :  ['nlocs'],
        'temperatureAir'        :  ['nlocs'],
        'temperatureDewpoint'   :  ['nlocs'],
}

    # write them out
    nlocs = obs_data[('geopotential_height', 'ObsValue')].shape[0]
    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('geopotential_height', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('wind_direction', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('wind_speed', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('air_temperature', "ObsValue")]['_FillValue'] = float_missing_value
    VarAttrs[('dew_point_temperature', "ObsValue")]['_FillValue'] = float_missing_value

    VarAttrs[('latitude', "MetaData")]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', "MetaData")]['_FillValue'] = float_missing_value
    VarAttrs[('air_pressure', "MetaData")]['_FillValue'] = float_missing_value

    # VarAttrs[('geopotential_height', 'ObsValue')]['units'] = 'Radians'
    # VarAttrs[('geopotential_height', 'ObsError')]['units'] = 'Radians'
    # VarAttrs[('geopotential_height', 'PreQC')]['units']    = 'unitless'

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

    profile_meta_data = get_station_elevation(bufr, profile_meta_data)
    profile_meta_data = get_station_id(profile_meta_data)


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
    profile_meta_data['LaunchTime'] = dtg

    return profile_meta_data

def get_station_id(profile_meta_data):

    k = 'station_id'
    wmo_block_id = profile_meta_data["stationIdWMOblock"]
    wmo_station_id = profile_meta_data["stationIdWMOstation"]
    profile_meta_data[k] = '{0:02}'.format(wmo_block_id) + '{0:03}'.format(wmo_station_id)

    return profile_meta_data

def get_station_elevation(bufr, profile_meta_data):

    # try to progressively get more detailed height of instrument
    k = 'station_elevation'

    station_height = codes_get(bufr, 'heightOfStationGroundAboveMeanSeaLevel')
    if station_height != abs(int_missing_value):
        profile_meta_data[k] = station_height
    else:
        station_height = codes_get(bufr, 'heightOfBarometerAboveMeanSeaLevel')
        if station_height != abs(int_missing_value):
            profile_meta_data[k] = station_height

    return profile_meta_data

def def_meta_data():

    meta_data_keys = {
 "stationIdWMOblock"                     : 'blockNumber',
 "stationIdWMOstation"                   : 'stationNumber',
 "stationLongName"                       : 'shipOrMobileLandStationIdentifier',
 "instrumentType"                        : 'radiosondeType',
 "station_elevation"                     : 'height',
 "instrumentSerialNum"                   : 'radiosondeSerialNumber',
 "instrumentSoftwareVersion"             : 'softwareVersionNumber',
 "instrumentHumidityCorrectionInfo"      : 'correctionAlgorithmsForHumidityMeasurements',
 "instrumentRadiationCorrectionInfo"     : 'solarAndInfraredRadiationCorrection',
}

    return meta_data_keys


def def_significance_table():

    significance_table = {
     "pressure level originally indicated by height as the veritcal coordinate" : 1,
     "freezing level" : 2,
     "level determined by regional decision" : 3,
     "top of wind sounding" : 4,
     "end of missing wind data" : 5,
     "beginning of missing wind data" : 6,
     "end of missing humidity data" : 7,
     "beginning of missing humidity data" : 8,
     "end of missing temperature data" : 9,
     "beginning of missing temperature data" : 10,
     "significant wind level" : 11,
     "significant humidity level" : 12,
     "significant temperature level" : 13,
     "maximum wind level" : 14,
     "tropopause level" : 15,
     "standard level" : 16,
     "surface" : 17,
}

    return significance_table

def get_normalized_bit(value, bit_index):
    return (value >> bit_index) & 1

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
    call_fail = False
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
        sounding_significance = codes_get_array(bufr, 'extendedVerticalSoundingSignificance', ktype=int)

        significance_table = def_significance_table()

        # set a default if first record labeled surface set surface pressure
        # could set this to None and skip if never gets set
        surface_pressure = pressure[0]
        if np.any(sounding_significance & 2**significance_table["surface"]):

            if np.sum(np.any(sounding_significance & 2**significance_table["surface"])) > 1:
                print ( " need more error handling too many surface levels" )
                call_fail = True
                print ( "starting position: %i" % start_pos )
                print ( "length pressure: %i" % len(pressure) )
                print ( "length temperature: %i" % len(temp_air) )
                print ( "length dewpoint: %i" % len(temp_dewpoint) )
                print ( "length geop_height: %i" % len(geop_height) )
                print ( "length sound_sig: %i" % len(sounding_significance) )
                print ( "length wind_dir: %i" % len(wind_direction) )
                print ( "length wind_speed: %i" % len(wind_speed) )
                print ( "length lat_displacement: %i" % len(lat_displacement) )
                print ( "length lon_displacement: %i" % len(lon_displacement) )
                print ( "length time_displacement: %i" % len(time_displacement) )
                print ( "========" )
            surface_level = np.where(sounding_significance & 2**significance_table["surface"])[0][0]
            surface_pressure = pressure[ surface_level ]
            if len(pressure) == len(temp_air):
                pass
                # if surface_level > 0 need to throw away data ????
            elif len(pressure) == len(temp_air)+1:
                if surface_level != 0:
                    # surface level not first record
                    pressure = pressure[surface_level:]
                    sounding_significance = sounding_significance[surface_level:]
                elif surface_level == 0:
                    # surface level first record
                    pressure = pressure[1:]
                    sounding_significance = sounding_significance[1:]
            elif len(pressure) > len(temp_air) + 1:
                if len(pressure) == len(lat_dispalcement):
                    # the only case we know
                    pressure = pressure[surface_level:len(pressure)+surface_level-1]
                    sounding_significance = sounding_significance[surface_level:len(pressure)+surface_level-1]
                    lat_displacement = lat_displacement[surface_level:len(pressure)+surface_level-1]
                    lon_displacement = lon_displacement[surface_level:len(pressure)+surface_level-1]


        if len( pressure ) != len( temp_air ):
            print ( " need more error handling mis-matched pressure to air temperature" )
            call_fail = True
            print ( "starting position: %i" % start_pos )
            print ( "length pressure: %i" % len(pressure) )
            print ( "length temperature: %i" % len(temp_air) )
            print ( "length dewpoint: %i" % len(temp_dewpoint) )
            print ( "length geop_height: %i" % len(geop_height) )
            print ( "length sound_sig: %i" % len(sounding_significance) )
            print ( "length wind_dir: %i" % len(wind_direction) )
            print ( "length wind_speed: %i" % len(wind_speed) )
            print ( "length lat_displacement: %i" % len(lat_displacement) )
            print ( "length lon_displacement: %i" % len(lon_displacement) )
            print ( "length time_displacement: %i" % len(time_displacement) )
            print ( "========" )
            #shell()
            #sys.exit()

        # skip records with bad sounding significance values
        if np.any( sounding_significance == np.abs( int_missing_value ) ):     #2147483647
            valid_level = np.where( sounding_significance != np.abs( int_missing_value ) )
            lat_displacement = lat_displacement[ valid_level ]
            lon_displacement = lon_displacement[ valid_level ]
            time_displacement = time_displacement[ valid_level ]
            wind_direction = wind_direction[ valid_level ]
            wind_speed = wind_speed[ valid_level ]
            temp_air = temp_air[ valid_level ]
            temp_dewpoint = temp_dewpoint[ valid_level ]
            pressure = pressure[ valid_level ]
            geop_height = geop_height[ valid_level ]
            sounding_significance = sounding_significance[ valid_level ]

        codes_release( bufr )

        num_levels        = len( lat_displacement )
        if ( len( lat_displacement ) != len( time_displacement ) ):
            print ( "this should never happen lat and time have different displacements lengths")
            call_fail = True
            sys.exit()

        # what to do for "ObsError" set to 1? and "PreQC" all zero?
        obs_data[('surface_pressure', "ObsValue")] = np.full(num_levels, surface_pressure, dtype='float32')
        obs_data[('geopotential_height', "ObsValue")] = assign_values(geop_height)
        obs_data[('wind_direction', "ObsValue")] = assign_values(wind_direction)
        obs_data[('wind_speed', "ObsValue")] = assign_values(wind_speed)
        obs_data[('air_temperature', "ObsValue")] = assign_values(temp_air)
        obs_data[('dew_point_temperature', "ObsValue")] = assign_values(temp_dewpoint)

        obs_data[('latitude', "MetaData")] = np.full(num_levels, lat, dtype='float32') + assign_values(lat_displacement)
        obs_data[('longitude', "MetaData")] = np.full(num_levels, lon, dtype='float32') + assign_values(lon_displacement)
        obs_data[('air_pressure', "MetaData")] = assign_values(pressure)

        # fake surface pressure for now from the first entry of air_pressure
#       profile_meta_data['surface_pressure'] = pressure[0]  # should go to obs value
        for k, v in profile_meta_data.items():
            vals = np.repeat(v, krepfac[0])
            obs_data[(k, "MetaData")] = assign_values(vals)

        # print ( " decoded raob num_lev: ", len(obs_data[('latitude',"MetaData")]) )
        # print ( "ending pos: ", f.tell() )
        count[0] += 1

        return obs_data, count, start_pos

    except:
        # print ( "invalid bufr message" )
        if call_fail:
            sys.exit()
        count[1] += 1
        # print ( "number of valid mssg: ", count[0] )
        # print ( "number of invalid mssg: ", count[1] )
        return obs_data, count, start_pos
        pass

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
