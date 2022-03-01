#!/usr/bin/python

from datetime import datetime, timedelta
import dateutil.parser
import os
from pathlib import Path
import sys
import time
import logging

import numpy as np
import netCDF4 as nc
from eccodes import CODES_MISSING_LONG, codes_bufr_new_from_file, codes_set, codes_get_array, KeyValueNotFoundError, codes_release
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
    ("aircraft_heading", "integer", "degrees"),
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

var_mimic_length = "latitude"

# True incoming BUFR observed variables.
raw_obsvars = ['airTemperature', 'mixingRatio', 'windDirection', 'windSpeed']

# The outgoing IODA variables (ObsValues), their units, and assigned constant ObsError.
obsvars = ['air_temperature', 'specific_humidity', 'eastward_wind', 'northward_wind']
obsvars_units = ['K', 'kg kg-1', 'm s-1', 'm s-1']
obserrlist = [1.2, 0.75E-3, 1.7, 1.7]

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

float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
double_missing_value = nc.default_fillvals['f8']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'
bufr_missing_value = CODES_MISSING_LONG

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])


def main(file_names, output_file):

    # initialize
    count = [0, 0, 0]
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
        logging.debug("Reading file: " + fname)
        AttrData['source_files'] += ", " + fname

        data, count, start_pos = read_file(fname, count, start_pos, data)

    AttrData['source_files'] = AttrData['source_files'][2:]
    logging.debug("All source files: " + AttrData['source_files'])

    if not data:
        logging.critical("ABORT: no message data was captured, stopping execution.")

    logging.info("--- {:9.4f} BUFR read seconds ---".format(time.time() - start_time))

    nlocs = len(data['dateTime'])
    DimDict = {'nlocs': nlocs}
    AttrData['nlocs'] = np.int32(DimDict['nlocs'])

    # Set coordinates and units of the ObsValues.
    for n, iodavar in enumerate(obsvars):
        varDict[iodavar]['valKey'] = iodavar, obsValName
        varDict[iodavar]['errKey'] = iodavar, obsErrName
        varDict[iodavar]['qcKey'] = iodavar, qcName
        varAttrs[iodavar, obsValName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsErrName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, qcName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsValName]['units'] = obsvars_units[n]
        varAttrs[iodavar, obsErrName]['units'] = obsvars_units[n]
        varAttrs[iodavar, qcName]['units'] = 'unitless'

    # Set units of the MetaData variables and all _FillValues.
    missing_vals = {'string' : string_missing_value,
                    'integer' : int_missing_value,
                    'long' : long_missing_value,
                    'float' : float_missing_value,
                    'double' : double_missing_value}
    dtypes = {'string' : object,
              'integer' : np.int32,
              'long' : np.int64,
              'float' : np.float32,
              'double' : np.float64}

    for key in meta_keys:
        dtypestr = locationKeyList[meta_keys.index(key)][1]
        varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]
        obs_data[(key, metaDataName)] = np.array(data[key], dtype=dtypes[dtypestr])

    # Transfer from the 1-D data vectors and ensure output data (obs_data) types using numpy.
    for n, iodavar in enumerate(obsvars):
        obs_data[(iodavar, obsValName)] = np.array(data[iodavar], dtype=np.float32)
        obs_data[(iodavar, obsErrName)] = np.full(nlocs, obserrlist[n], dtype=np.float32)
        obs_data[(iodavar, qcName)] = np.full(nlocs, 2, dtype=np.int32)

    logging.debug("Writing file: " + output_file)

    # setup the IODA writer
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(obs_data, VarDims, varAttrs, AttrData)

    logging.info("--- {:9.4f} total seconds ---".format(time.time() - start_time))


# Replace BUFR missing value indicators with IODA missings.
def assign_values(data, key):

    if isinstance(data[0], str):
        return np.array(data, dtype=object)

    if (data.dtype == np.float32 or data.dtype == np.float64):
        if not (key in meta_keys):
            data[np.abs(data) >= np.abs(bufr_missing_value)] = float_missing_value
            return np.array(data, dtype=np.float32)
        elif (locationKeyList[meta_keys.index(key)][1] == "float"):
            data[np.abs(data) >= np.abs(bufr_missing_value)] = float_missing_value
            return np.array(data, dtype=np.float32)
        elif (locationKeyList[meta_keys.index(key)][1] == "double"):
            data[np.abs(data) >= np.abs(bufr_missing_value)] = double_missing_value
            return np.array(data, dtype=np.float64)
        elif (locationKeyList[meta_keys.index(key)][1] == "integer"):
            data[np.abs(data) >= np.abs(bufr_missing_value)] = int_missing_value
            return np.array(data, dtype=np.int32)
        elif (locationKeyList[meta_keys.index(key)][1] == "long"):
            data[np.abs(data) >= np.abs(bufr_missing_value)] = long_missing_value
            return np.array(data, dtype=np.int64)
    elif (data.dtype == np.int32 or data.dtype == np.int64):
        if not (key in meta_keys):
            data[np.abs(data) >= np.abs(bufr_missing_value)] = int_missing_value
            return np.array(data, dtype=np.int32)
        if (locationKeyList[meta_keys.index(key)][1] == "integer"):
            data[np.abs(data) >= np.abs(bufr_missing_value)] = int_missing_value
            return np.array(data, dtype=np.int32)
        elif (locationKeyList[meta_keys.index(key)][1] == "long"):
            data[np.abs(data) >= np.abs(bufr_missing_value)] = long_missing_value
            return np.array(data, dtype=np.int64)
        elif (locationKeyList[meta_keys.index(key)][1] == "float"):
            data = data.astype(np.float32)
            data[np.abs(data) >= np.abs(bufr_missing_value)] = float_missing_value
            return np.array(data, dtype=np.float32)
    elif data.dtype.kind in {'U', 'S'}:
        #data[data == ""] = string_missing_value
        return np.array(data, dtype=object)

    logging.critical("ABORT, no matching datatype found for key: " + key)


# Assign the correct IODA missing value to MetaData element.
def assign_missing_meta(key, num):

    if (locationKeyList[meta_keys.index(key)][1] == "string"):
        return np.full(num, string_missing_value, dtype=object)
    elif (locationKeyList[meta_keys.index(key)][1] == "integer"):
        return np.full(num, int_missing_value, dtype=np.int32)
    elif (locationKeyList[meta_keys.index(key)][1] == "long"):
        return np.full(num, long_missing_value, dtype=np.int64)
    elif (locationKeyList[meta_keys.index(key)][1] == "float"):
        return np.full(num, float_missing_value, dtype=np.float32)
    elif (locationKeyList[meta_keys.index(key)][1] == "double"):
        return np.full(num, double_missing_value, dtype=np.float64)
    else:
        logging.critical("ABORT, no matching datatype found for key: " + key)


# Return True if all elements in the array are missing, otherwise False.
def is_all_missing(data):
    if data.dtype == np.float32:
        return all(x==float_missing_value for x in data)
    if data.dtype == np.float64:
        return all(x==double_missing_value for x in data)
    elif data.dtype == np.int32:
        return all(x==int_missing_value for x in data)
    elif data.dtype == np.int64:
        return all(x==long_missing_value for x in data)
    elif data.dtype.kind in {'U', 'S'}:
        return all(x==string_missing_value for x in data)

    logging.warning("data type not determined, returning False")
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

    meta_data = {}         # All the various MetaData go in here.
    vals = {}              # Floating-point ObsValues variables go in here.
    avals = []             # Temporarily hold array of values.
    call_fail = False
    if start_pos == f.tell():
        return data, count, None
    start_pos = f.tell()
    count[0] += 1

    met_utils = meteo_utils.meteo_utils()

    try:
        bufr = codes_bufr_new_from_file(f)
    except:
        logging.critical("ABORT, failue when attempting to call:  codes_bufr_new_from_file")

    try:
        codes_set(bufr, 'skipExtraKeyAttributes', 1)   # Supposedly this is ~25 percent faster
        codes_set(bufr, 'unpack', 1)
    except:
        logging.info("finished unpacking BUFR file")
        start_pos = None
        return data, count, start_pos

    # First, get the MetaData we are interested in (list is in metaDataKeyList)
    for k, v in metaDataKeyList.items():
        meta_data[k] = []
        if (len(v) > 1):
            for var in v:
                if (var != 'Constructed'):
                    try:
                        avals = codes_get_array(bufr, var)
                        meta_data[k] = assign_values(avals, k)
                        if not is_all_missing(meta_data[k]): break
                    except KeyValueNotFoundError:
                        logging.warning("Caution: unable to find requested BUFR key: " + var)
        else:
            if (v[0] != 'Constructed'):
                try:
                    avals = codes_get_array(bufr, v[0])
                    meta_data[k] = assign_values(avals, k)
                except KeyValueNotFoundError:
                    logging.warning("Caution, unable to find requested BUFR key: " + v[0])

    # Determine the target number of observation points from a critical variable (i.e., latitude).
    target_number = len(meta_data[var_mimic_length])

    # Plus, to construct a dateTime, we always need its components.
    try:
        year = codes_get_array(bufr, 'year')
        if (len(year) < target_number): year = np.full(target_number, year[0])
        year[year < 1900] = 1900
        year[year > 2399] = 1900
    except KeyValueNotFoundError:
        logging.warning("Caution, no data for year")
        year = np.full(target_number, 1900)

    try:
        month = codes_get_array(bufr, 'month')
        if (len(month) < target_number): month = np.full(target_number, month[0])
        year[np.logical_or(month<1, month>12)] = 1900
        month[np.logical_or(month<1, month>12)] = 1
    except KeyValueNotFoundError:
        logging.warning("Caution, no data for month")
        year = np.full(target_number, 1900)
        month = np.full(target_number, 1)

    try:
        day = codes_get_array(bufr, 'day')
        if (len(day) < target_number): day = np.full(target_number, day[0])
        year[np.logical_or(day<1, day>31)] = 1900
        day[np.logical_or(day<1, day>31)] = 1
    except KeyValueNotFoundError:
        logging.warning("Caution, no data for day")
        year = np.full(target_number, 1900)
        day = np.full(target_number, 1)

    try:
        hour = codes_get_array(bufr, 'hour')
        if (len(hour) < target_number): hour = np.full(target_number, hour[0])
        year[np.logical_or(hour<0, hour>23)] = 1900
        hour[np.logical_or(hour<0, hour>23)] = 0
    except KeyValueNotFoundError:
        logging.warning("Caution, no data for hour")
        year = np.full(target_number, 1900)
        hour = np.full(target_number, 0)

    try:
        minute = codes_get_array(bufr, 'minute')
        if (len(minute) < target_number): minute = np.full(target_number, minute[0])
        year[np.logical_or(minute<0, minute>59)] = 1900
        minute[np.logical_or(minute<0, minute>59)] = 0
    except KeyValueNotFoundError:
        logging.warning("Caution, no data for minute")
        year = np.full(target_number, 1900)
        minute = np.full(target_number, 0)

    second = np.full(target_number, 0)
    try:
        avals = codes_get_array(bufr, 'second')    # non-integer value, optional
        if (len(avals) < target_number): avals = np.full(target_number, avals[0])
        for n, a in enumerate(avals):
            if (a>0 and a<60): second[n] = round(a)
    except KeyValueNotFoundError:
        logging.info("Caution, no data for second")

    for n, yyyy in enumerate(year):
        this_datetime = datetime(yyyy, month[n], day[n], hour[n], minute[n], second[n])
        time_offset = round((this_datetime - epoch).total_seconds())
        meta_data['dateTime'].append(time_offset)

    # Force longitude into space of -180 to +180 only. Reset to missing if either lat or lon not on earth
    meta_data['longitude'][np.logical_or(meta_data['longitude']<-180.0, meta_data['longitude']>360.0)] = float_missing_value
    meta_data['latitude'][np.logical_or(meta_data['latitude']<-90.0, meta_data['latitude']>90.0)] = float_missing_value
    meta_data['latitude'][np.logical_or(meta_data['longitude']<-180.0, meta_data['longitude']>180.0)] = float_missing_value
    meta_data['longitude'][np.logical_or(meta_data['latitude']<-90.0, meta_data['latitude']>90.0)] = float_missing_value
    for n, longitude in enumerate(meta_data['longitude']):
        if (meta_data['longitude'][n] != float_missing_value and meta_data['longitude'][n] > 360):
            meta_data['longitude'][n] = 360.0 - meta_data['longitude'][n]

    # If the height/altitude is unreasonable, then it is useless.
    meta_data['height'][np.logical_or(meta_data['height']<-425, meta_data['height']>90000)] = int_missing_value

    # Count the locations for which time, lat, lon, or height is nonsense, therefore observation is useless.
    count[1] += target_number
    count[2] += sum(x==1900 for x in year)
    count[2] += sum((y<-90.0 or y>90.0) for y in meta_data['latitude'])
    count[2] += sum((z<-425.0 or z>90000.0) for z in meta_data['height'])

    # Next, get the raw observed weather variables we want.
    # TO-DO: currently all missing are set to float type, probably need different assignment for integers.
    for variable in raw_obsvars:           #  ['airTemperature', 'mixingRatio', 'windDirection', 'windSpeed']
        vals[variable] = []
        try:
            avals = codes_get_array(bufr, variable)
            if (len(avals) < target_number):
                logging.critical("Caution, length mismatch: " + str(len(avals)) + ", " + str(target_number))
            vals[variable] = assign_values(avals, variable)
        except KeyValueNotFoundError:
            logging.warning("Caution, unable to find requested BUFR variable: " + variable)
            vals[variable] = np.full(target_number, float_missing_value, dtype=np.float32)

    # Be done with this BUFR message.
    codes_release(bufr)

    # For any of the MetaData elements that were totally lacking, fill entire vectory with missing.
    for k, v in metaDataKeyList.items():
        if not any(meta_data[k]):
            meta_data[k] = assign_missing_meta(k, target_number)

    # Need to transform some variables to others (wind speed/direction to components for example).
    uwnd = np.full(target_number, float_missing_value)
    vwnd = np.full(target_number, float_missing_value)
    for n, wind_direction in enumerate(vals['windDirection']):
        if (wind_direction >= 0 and wind_direction<=360 and vals['windSpeed'][n] != float_missing_value):
            uwnd[n], vwnd[n] = met_utils.dir_speed_2_uv(wind_direction, vals['windSpeed'][n])

    spfh = np.full(target_number, float_missing_value)
    for n, mixing_ratio in enumerate(vals['mixingRatio']):
        if (mixing_ratio>0 and mixing_ratio<25.E-3):
            spfh[n] = mixing_ratio / (1.0 + mixing_ratio)

    # Move everything into the final data dictionary, including metadata.
    data['eastward_wind'] = np.append(data['eastward_wind'], uwnd)
    data['northward_wind'] = np.append(data['northward_wind'], vwnd)
    data['specific_humidity'] = np.append(data['specific_humidity'], spfh)
    data['air_temperature'] = np.append(data['air_temperature'], vals['airTemperature'])
    for key in meta_keys:
        data[key] = np.append(data[key], meta_data[key])

    logging.info("BUFR message number: " + str(count[0]))
    logging.info("number of observations so far: " + str(count[1]))
    logging.info("number of invalid or useless observations: " + str(count[2]))
    return data, count, start_pos


if __name__ == "__main__":

    import argparse

    parser = argparse.ArgumentParser(
        description=(
            'Read aircraft (AMDAR) BUFR file and convert into IODA output file')
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

    if args.debug:
        logging.basicConfig(level=logging.INFO)
    elif args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.ERROR)


    for file_name in args.file_names:
        if not os.path.isfile(file_name):
            parser.error('Input (-i option) file: ', file_name, ' does not exist')

    main(args.file_names, args.output_file)
