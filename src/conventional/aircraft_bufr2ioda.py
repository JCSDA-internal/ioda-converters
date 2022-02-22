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
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict
import ioda_conv_engines as iconv
import meteo_utils

os.environ["TZ"] = "UTC"

locationKeyList = [
    ("aircraft_id", "string", ""),
    ("aircraft_flightNum", "string", ""),
    ("aircraft_tailNum", "string", ""),
    ("obs_sequenceNum", "integer", ""),
    ("originationAirport", "string", ""),
    ("destinationAirport", "string", ""),
    ("flight_phase", "integer", ""),
    ("roll_angle", "float", "degrees"),
    ("roll_angle_quality", "integer", ""),
    ("aircraft_speed", "float", "m s-1"),
    ("aircraft_heading", "float", "degrees"),
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("height", "float", "m"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z"),
]
meta_keys = [m_item[0] for m_item in locationKeyList]

metaDataKeyList = {
    'aircraft_id': ['aircraftRegistrationNumberOrOtherIdentification'],
    'aircraft_flightNum': ['aircraftFlightNumber'],
    'aircraft_tailNum': ['aircraftTailNumber'],
    'obs_sequenceNum': ['observationSequenceNumber'],
    'originationAirport': ['originationAirport'],
    'destinationAirport': ['destinationAirport'],
    'flight_phase': ['detailedPhaseOfFlight'],
    'roll_angle': ['aircraftRollAngle'],
    'roll_angle_quality': ['aircraftRollAngleQuality'],
    'aircraft_speed': ['aircraftTrueAirspeed'],
    'aircraft_heading': ['aircraftTrueHeading'],
    'latitude': ['latitude'],
    'longitude': ['longitude'],
    'height': ['Constructed', 'globalNavigationSatelliteSystemAltitude', 'height', 'flightLevel'],
    'dateTime': ['Constructed'],
}

# True incoming BUFR observed variables.
raw_obsvars = ['airTemperature', 'mixingRatio', 'windDirection', 'windSpeed']

# The outgoing IODA variables and their units.
obsvars = ['air_temperature', 'specific_humidity', 'eastward_wind', 'northward_wind']
obsvars_units = ['K', 'kg kg-1', 'm s-1', 'm s-1']

VarDims = {
    'air_temperature': ['nlocs'],
    'specific_humidity': ['nlocs'],
    'eastward_wind': ['nlocs'],
    'northward_wind': ['nlocs'],
}

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

AttrData = {
    'converter': os.path.basename(__file__),
    'ioda_version': 2,
    'description': 'Aircraft observations converted from BUFR',
    'source': 'LDM at NCAR-RAL',
    'source_files': ''
}

DimDict = {
}

ioda_float_type = 'float32'
ioda_int_type = 'int32'
ioda_long_type = 'int64'
float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
double_missing_value = nc.default_fillvals['f8']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'
bufr_missing_value =  int_missing_value   # -2147483647

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])


def main(file_names, output_file, debug, verbose):

    # initialize
    count = [0, 0]
    start_pos = None
    start_time = time.time()

    obs_data = {}          # The final outputs.
    data = {}              # Before assigning the output types into the above.
    data['eastward_wind'] = []
    data['northward_wind'] = []
    data['specific_humidity'] = []
    data['air_temperature'] = []
    for key in meta_keys:
        data[key] = []

    varDict = defaultdict(lambda: DefaultOrderedDict(dict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    for fname in file_names:
        if debug: print("INFO: Reading file: ", fname)
        AttrData['source_files'] += ", " + fname

        data, count, start_pos = read_file(fname, count, start_pos, data)

    AttrData['source_files'] = AttrData['source_files'][2:]
    if verbose: print("INFO: All source files: ", AttrData['source_files'])

    if not data:
        print("WARNING: no message data was captured, stopping execution.")
        sys.exit()

    if debug: print("--- %s BUFR read seconds ---" % (time.time() - start_time))

    nlocs = len(data['dateTime'])
    DimDict = {'nlocs': nlocs}
    AttrData['nlocs'] = np.int32(DimDict['nlocs'])

    # Set coordinates and units of the ObsValues.
    n = 0
    for iodavar in obsvars:
        varDict[iodavar]['valKey'] = iodavar, obsValName
        varDict[iodavar]['errKey'] = iodavar, obsErrName
        varDict[iodavar]['qcKey'] = iodavar, qcName
        varAttrs[iodavar, obsValName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsErrName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, qcName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsValName]['units'] = obsvars_units[n]
        varAttrs[iodavar, obsErrName]['units'] = obsvars_units[n]
        varAttrs[iodavar, qcName]['units'] = 'unitless'
        n += 1

    # Set units of the MetaData variables and all _FillValues.
    for key in meta_keys:
        varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
        if (locationKeyList[meta_keys.index(key)][1] == "string"):
            varAttrs[(key, metaDataName)]['_FillValue'] = string_missing_value
        elif (locationKeyList[meta_keys.index(key)][1] == "integer"):
            varAttrs[(key, metaDataName)]['_FillValue'] = int_missing_value
        elif (locationKeyList[meta_keys.index(key)][1] == "long"):
            varAttrs[(key, metaDataName)]['_FillValue'] = long_missing_value
        elif (locationKeyList[meta_keys.index(key)][1] == "float"):
            varAttrs[(key, metaDataName)]['_FillValue'] = float_missing_value

    # Transfer from the 1-D data vectors and ensure output data (obs_data) types using numpy.
    iodavar = 'air_temperature'
    obs_data[(iodavar, obsValName)] = np.array(data[iodavar], dtype=np.float32)
    obs_data[(iodavar, obsErrName)] = np.full((nlocs), 1.2, dtype=np.float32)
    obs_data[(iodavar, qcName)] = np.full((nlocs), 2, dtype=np.int32)
    iodavar = 'specific_humidity'
    obs_data[(iodavar, obsValName)] = np.array(data[iodavar], dtype=np.float32)
    obs_data[(iodavar, obsErrName)] = np.full((nlocs), 0.75E-3, dtype=np.float32)
    obs_data[(iodavar, qcName)] = np.full((nlocs), 2, dtype=np.int32)
    iodavar = 'eastward_wind'
    obs_data[(iodavar, obsValName)] = np.array(data[iodavar], dtype=np.float32)
    obs_data[(iodavar, obsErrName)] = np.full((nlocs), 1.7, dtype=np.float32)
    obs_data[(iodavar, qcName)] = np.full((nlocs), 2, dtype=np.int32)
    iodavar = 'northward_wind'
    obs_data[(iodavar, obsValName)] = np.array(data[iodavar], dtype=np.float32)
    obs_data[(iodavar, obsErrName)] = np.full((nlocs), 1.7, dtype=np.float32)
    obs_data[(iodavar, qcName)] = np.full((nlocs), 2, dtype=np.int32)

    for key in meta_keys:
        varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
        if (locationKeyList[meta_keys.index(key)][1] == "string"):
            obs_data[(key, metaDataName)] = np.array(data[key], dtype=object)
        elif (locationKeyList[meta_keys.index(key)][1] == "integer"):
            obs_data[(key, metaDataName)] = np.array(data[key], dtype=np.int32)
        elif (locationKeyList[meta_keys.index(key)][1] == "long"):
            obs_data[(key, metaDataName)] = np.array(data[key], dtype=np.int64)
        elif (locationKeyList[meta_keys.index(key)][1] == "float"):
            obs_data[(key, metaDataName)] = np.array(data[key], dtype=np.float32)

    if debug: print("INFO: Writing file: ", output_file)

    # setup the IODA writer
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(obs_data, VarDims, varAttrs, AttrData)

    if debug: print("--- %s total seconds ---" % (time.time() - start_time))


# Replace BUFR missing value indicators with IODA missings.
def assign_value(data, key):

    if type(data) == str:
        return string_missing_value if (data.find('?') >= 0 or data == "") else data
    elif type(data) == float:
        return float_missing_value if (np.abs(data) >= np.abs(bufr_missing_value)) else data
    elif type(data) == int:
        return int_missing_value if (np.abs(data) >= np.abs(bufr_missing_value)) else data

    return float_missing_value


# Assign the correct IODA missing value to MetaData element.
def assign_missing_meta(key):

    if (locationKeyList[meta_keys.index(key)][1] == "string"):
        return string_missing_value
    elif (locationKeyList[meta_keys.index(key)][1] == "integer"):
        return int_missing_value
    elif (locationKeyList[meta_keys.index(key)][1] == "long"):
        return long_missing_value
    elif (locationKeyList[meta_keys.index(key)][1] == "float"):
        return float_missing_value

    return float_missing_value


# Determine if the BUFR data of MetaData element is missing or not.
def is_missing(data, key):

    if (locationKeyList[meta_keys.index(key)][1] == "string" and
        (data.find('?') >= 0 or data == "")): return True
    elif (locationKeyList[meta_keys.index(key)][1] == "integer" and
        data == bufr_missing_value): return True
    elif (locationKeyList[meta_keys.index(key)][1] == "long" and
        data == bufr_missing_value): return True
    elif (locationKeyList[meta_keys.index(key)][1] == "float" and
        data == bufr_missing_value): return True

    return False


def read_file(file_name, count, start_pos, data):

    f = open(file_name, 'rb')

    while True:
        # Use eccodes to decode each bufr message in the file
        data, count, start_pos = read_bufr_message(f, count, start_pos, data)

        if start_pos is None:
            break

    return data, count, start_pos


def read_bufr_message(f, count, start_pos, data):

    meta_data = {}
    vals = {}
    call_fail = False
    if start_pos == f.tell():
        return data, count, None
    start_pos = f.tell()

    met_utils = meteo_utils.meteo_utils()

    count[0] += 1

    try:
        bufr = codes_bufr_new_from_file(f)
    except:
        print ("ABORT, failue when attempting to call:  codes_bufr_new_from_file")
        sys.exit()

    try:
        codes_set(bufr, 'unpack', 1)
    except:
        if debug: print ("INFO: finished unpacking BUFR file")
        start_pos = None
        return data, count, start_pos

    # First, get the MetaData we are interested in (list is in metaDataKeyList)
    for k, v in metaDataKeyList.items():
        if (len(v) > 1):
            for var in v:
                if (var != 'Constructed'):
                    try:
                        meta_data[k] = assign_value(codes_get(bufr, var), k)
                    except KeyValueNotFoundError:
                        if verbose: print ("Caution: unable to find requested BUFR key: " + var)
                        meta_data[k] = assign_missing_meta(k)
                        continue
                    if (is_missing(meta_data[k], k)):
                        if verbose: print ("INFO: " + var + " is present but missing data")
                        meta_data[k] = assign_missing_meta(k)
                    else:
                        break
        else:
            if (v[0] != 'Constructed'):
                try:
                    meta_data[k] = assign_value(codes_get(bufr, v[0]), k)
                except KeyValueNotFoundError:
                    if verbose: print ("Caution, unable to find requested BUFR key: " + v[0])
                    meta_data[k] = assign_missing_meta(k)
                    continue
                if (is_missing(meta_data[k], k)):
                    if verbose: print ("INFO: " + v[0] + " is present but missing data")
                    meta_data[k] = assign_missing_meta(k)

    # Plus, to construct a dateTime, we always need its components.
    try:
        year = codes_get(bufr, 'year')
    except KeyValueNotFoundError:
        if verbose: print ("Caution, no data for year")
        year = -1

    try:
        month = codes_get(bufr, 'month')
    except KeyValueNotFoundError:
        if verbose: print ("Caution, no data for month")
        month = -1

    try:
        day = codes_get(bufr, 'day')
    except KeyValueNotFoundError:
        if verbose: print ("Caution, no data for day")
        day = -1


    try:
        hour = codes_get(bufr, 'hour')
    except KeyValueNotFoundError:
        if verbose: print ("Caution, no data for hour")
        hour = -1

    try:
        minute = codes_get(bufr, 'minute')
    except KeyValueNotFoundError:
        if verbose: print ("Caution, no data for minute")
        minute = -1

    try:
        second = codes_get(bufr, 'second')    # non-integer value, optional
        if (second >= 0 and second <= 59):
            second = round(second)
        else:
            second = 0
    except KeyValueNotFoundError:
        second = 0

    meta_data['dateTime'] = int_missing_value
    if (year>1900 and year<2199 and month>=1 and month<=12 and day>=1 and day<=31
        and hour>=0 and hour<=23 and minute>=0 and minute<=59 and second>=0 and second<=59):
        this_datetime = datetime(year, month, day, hour, minute, second)
        time_offset = (this_datetime - epoch).total_seconds()
        meta_data['dateTime'] = round(time_offset)
    else:
        if verbose: print("Warning, useless date info, {:4d}".format(year) + "-{:02d}".format(month)
            + "-{:02d}".format(day) + "T{:02d}".format(hour) + ":{:02d}".format(minute)
            + ":{:02d}".format(second))
        count[1] += 1

    # Always need lat, lon or else the observation is useless, so let us double-check.
    if (meta_data['latitude'] >= -90 and meta_data['latitude'] <= 90
        and meta_data['longitude'] >= -180 and meta_data['longitude'] <= 360):
        if (meta_data['longitude'] > 180):
            meta_data['longitude'] = 360.0 - meta_data['longitude']
    else:
        if verbose: print ("Warning, either or both of lat/lon are mising in this BUFR msg."
            + " Lat, Lon = {:.3}".format(meta_data['latitude']) + ", {:.3}".format(meta_data['longitude']))
        count[1] += 1

    # If the height/altitude is unreasonable, then it is useless.
    if (meta_data['height'] < -425 or meta_data['height'] > 90000):
        if verbose: print ("Warning, height information really bad: {:.1}".format(meta_data['height']))
        count[1] += 1

    # Next, get the raw observed weather variables we want.
    # TO-DO: currently all missing are set to float type, probably need different assignment for integers.
    for variable in raw_obsvars:           #  ['airTemperature', 'mixingRatio', 'windDirection', 'windSpeed']
        try:
            vals[variable] = assign_value(codes_get(bufr, variable), variable)
        except KeyValueNotFoundError:
            if verbose: print ("Caution, unable to find requested BUFR variable: " + variable)
            vals[variable] = float_missing_value

    # Be done with this BUFR message.
    codes_release(bufr)

    # Need to transform some variables to others (wind speed/direction to components for example).
    if (vals['windDirection'] != float_missing_value and vals['windSpeed'] != float_missing_value):
        uwnd, vwnd = met_utils.dir_speed_2_uv(vals['windDirection'], vals['windSpeed'])
    else:
        uwnd = float_missing_value
        vwnd = float_missing_value

    if (vals['mixingRatio'] != float_missing_value):
        spfh = vals['mixingRatio'] / (1.0 + vals['mixingRatio'])
    else:
        spfh = float_missing_value

    # Move everything into the final data dictionary, including metadata.
    data['eastward_wind'].append(uwnd)
    data['northward_wind'].append(vwnd)
    data['specific_humidity'].append(spfh)
    data['air_temperature'].append(vals['airTemperature'])
    for key in meta_keys:
        data[key].append(meta_data[key])

    if debug: print ( "number of total msgs: ", count[0] )
    if debug: print ( "number of invalid or useless msgs: ", count[1] )
    return data, count, start_pos


if __name__ == "__main__":

    import argparse

    parser = argparse.ArgumentParser(
        description=(
            'Read aircraft (AMDAR/TAMDAR/ACAR/etc.) BUFR file and convert into IODA output file')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument('-i', '--input-files', nargs='+', dest='file_names',
                          action='store', default=None, required=True,
                          help='input files')
    required.add_argument('-o', '--output-file', dest='output_file',
                          action='store', default=None, required=True,
                          help='output file')

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--debug', action=argparse.BooleanOptionalAction,
                          help='enable debug messages')
    optional.add_argument('--verbose', action=argparse.BooleanOptionalAction,
                          help='enable verbose debug messages')

    args = parser.parse_args()

    debug = True if args.debug else False
    verbose = True if args.verbose else False
    if verbose: debug = True

    for file_name in args.file_names:
        if not os.path.isfile(file_name):
            parser.error('Input (-i option) file: ', file_name, ' does not exist')

    main(args.file_names, args.output_file, debug, verbose)
