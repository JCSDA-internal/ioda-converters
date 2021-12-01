#!/usr/bin/python

from __future__ import print_function
import struct
import os
import sys
import numpy as np
from eccodes import *

from IPython import embed as shell

def main(file_name, cdtg):

    # initialize
    count = [0, 0]
    start_pos = None

    f = open(file_name, 'rb')

    while True:
        # here is where to send to ecCodes
        obs_data, loc_data, count, start_pos = read_bufr_message( f, count, start_pos )

        if start_pos == None:
            #print ( "start_pos: ", start_pos )
            break

    #print ( "number of valid mssg: ", count[0] )
    #print ( "number of invalid mssg: ", count[1] )

    sys.exit()

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
#   profile_meta_data['sondeLaunchTime'] = datetime.strptime( dtg, "%Y-%m-%dT%H:%M:%SZ")

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

def read_bufr_message( f, count, start_pos ):

    obs_data = {}
    loc_data = {}
    if start_pos == f.tell():
        return obs_data, loc_data, count, None
    start_pos = f.tell()
    print ( "staring pos: ", f.tell() )

    try:
        bufr = codes_bufr_new_from_file( f )

        codes_set(bufr, 'unpack', 1)

        profile_meta_data = get_meta_data(bufr)

        # replication factors for the datasets but these do not have number of levels?
        krepfac = codes_get_array(bufr, 'extendedDelayedDescriptorReplicationFactor')
        drepfac = codes_get_array(bufr, 'delayedDescriptorReplicationFactor')

        lat = codes_get(bufr, 'latitude')
        lon = codes_get(bufr, 'longitude')
        lat_displacement  = codes_get_array(bufr, 'latitudeDisplacement')
        lon_displacement  = codes_get_array(bufr, 'longitudeDisplacement')
        time_displacement = codes_get_array(bufr, 'timePeriod')
        wind_direction    = codes_get_array(bufr, 'windDirection')
        wind_speed        = codes_get_array(bufr, 'windSpeed')
        temp_air          = codes_get_array(bufr, 'airTemperature')
        temp_dewpoint     = codes_get_array(bufr, 'dewpointTemperature')
        pressure          = codes_get_array(bufr, 'pressure')
        geop_height       = codes_get_array(bufr, 'nonCoordinateGeopotentialHeight')

        codes_release( bufr )

        num_levels        = len( lat_displacement )

        # what to do for "ObsError" set to 1? and "PreQC" all zero?
        obs_data[('altitude', "ObsValue")]             = geop_height
        obs_data[('windDirection', "ObsValue")]        = wind_direction
        obs_data[('windSpeed', "ObsValue")]            = wind_speed
        obs_data[('temperatureAir', "ObsValue")]       = temp_air
        obs_data[('temperatureDewpoint', "ObsValue")]  = temp_dewpoint

        loc_data['latitude']   =  np.full(num_levels, lat, dtype='float32') + lat_displacement
        loc_data['longitude']  =  np.full(num_levels, lon, dtype='float32') + lon_displacement
        loc_data['pressure']   =  pressure
        for k, v in profile_meta_data.items():
            loc_data[k]        = np.repeat(v, krepfac[0])

        print ( " decoded raob num_lev: ", len(loc_data['latitude']) )
        print ( "ending pos: ", f.tell() )
        count[0] += 1

        return obs_data, loc_data, count, start_pos

    except:
        #print ( "invalid bufr message" )
        count[1] += 1
        #print ( "number of valid mssg: ", count[0] )
        #print ( "number of invalid mssg: ", count[1] )
        return obs_data, loc_data, count, start_pos
        pass

if __name__ == "__main__":

    from optparse import OptionParser

    usage = 'usage: %prog -d date-time -i increment-file'
    parser = OptionParser(usage)
    parser.add_option('-d', '--date-time', dest='cdtg',
                      action='store', default=None,
                      help='10-digit date time group')
    parser.add_option('-i', '--input-file', dest='file_name',
                      action='store', default=None,
                      help='input file')
    (options, args) = parser.parse_args()

    # chk date
    if (options.cdtg):
        if len(options.cdtg) != 10:
            parser.error("expecting date in 10 character (yyyymmddhh) format \n \
                        received date: %s" % options.cdtg)
    else:
        parser.error("expecting date in 10 character (yyyymmddhh) format \n \
                      received date: %s" % options.cdtg)

    if not os.path.isfile(options.file_name):
        parser.error('File does not exist, please enter valid increment file in -i option.')

    main(options.file_name, options.cdtg)
