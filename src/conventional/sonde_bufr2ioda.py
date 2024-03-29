#!/usr/bin/env python3

from datetime import datetime
import os
import sys
import time
import logging
import json
from pathlib import Path

import numpy as np
import netCDF4 as nc
import eccodes as ecc
from cartopy import geodesic
from copy import deepcopy as dcop

# These modules need the path to lib-python modules
import pyiodaconv.ioda_conv_engines as iconv
import pyiodaconv.meteo_utils as meteo_utils
from pyiodaconv.orddicts import DefaultOrderedDict
from collections import defaultdict

os.environ["TZ"] = "UTC"

geod = geodesic.Geodesic()  # generate ellipsoid, defaults to Earth WGS84 parameters

locationKeyList = [
    ("latitude", "float", "degrees_north", "keep"),
    ("longitude", "float", "degrees_east", "keep"),
    ("stationElevation", "float", "m", "keep"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z", "keep"),
    ("releaseTime", "long", "seconds since 1970-01-01T00:00:00Z", "keep"),
    ("pressure", "float", "Pa", "keep"),
    ("geopotentialHeight", "float", "m", "keep"),
    ("vertSignificance", "integer", "", "toss"),
    ("latDisplacement", "float", "degrees", "toss"),
    ("lonDisplacement", "float", "degrees", "toss"),
    ("timeDisplacement", "float", "s", "toss"),
    ("positionEstimated", "integer", "", "keep"),
    ("wmoBlockNumber", "integer", "", "toss"),
    ("wmoStationNumber", "integer", "", "toss"),
    ("wigosIdentifier", "string", "", "toss"),
    ("stationIdentification", "string", "", "keep"),
    ("stationLongName", "string", "", "toss"),
    ("instrumentIdentifier", "integer", "", "keep"),
    ("instrumentCorrectionInfo", "integer", "", "keep"),
    # ("instrumentHumidityCorrectionInfo", "integer", "", "keep"),
    # ("temperatureSensorType", "integer", "", "keep"),
    # ("humiditySensorType", "integer", "", "keep"),
    ("year", "integer", "", "toss"),
    ("month", "integer", "", "toss"),
    ("day", "integer", "", "toss"),
    ("hour", "integer", "", "toss"),
    ("minute", "integer", "", "toss"),
    ("second", "integer", "", "toss"),
]
meta_keys = [m_item[0] for m_item in locationKeyList]

metaDataKeyList = {
    'latitude': ['latitude'],
    'longitude': ['longitude'],
    'stationElevation': ['Constructed', 'heightOfBarometerAboveMeanSeaLevel',
                         'heightOfStationGroundAboveMeanSeaLevel', 'heightOfStation', 'height'],
    'dateTime': ['Constructed'],
    'releaseTime': ['Constructed'],
    'pressure': ['pressure', 'nonCoordinatePressure'],
    'geopotentialHeight': ['nonCoordinateGeopotentialHeight', 'geopotentialHeight'],
    'vertSignificance': ['extendedVerticalSoundingSignificance', 'verticalSoundingSignificance'],
    'latDisplacement': ['latitudeDisplacement'],
    'lonDisplacement': ['longitudeDisplacement'],
    'timeDisplacement': ['timePeriod'],
    'positionEstimated': ['Constructed'],
    'wmoBlockNumber': ['blockNumber'],
    'wmoStationNumber': ['stationNumber'],
    'wigosIdentifier': ['wigosLocalIdentifierCharacter'],
    'stationIdentification': ['Constructed'],
    'stationLongName': ['shipOrMobileLandStationIdentifier'],
    'instrumentIdentifier': ['radiosondeType'],
    'instrumentCorrectionInfo': ['solarAndInfraredRadiationCorrection'],
    # "instrumentHumidityCorrectionInfo": ['correctionAlgorithmsForHumidityMeasurements'],
    # "temperatureSensorType": ['temperatureSensorType'],
    # "humiditySensorType": ['humiditySensorType'],
    # "instrumentSerialNum": ['radiosondeSerialNumber'],
    # "instrumentSoftwareVersion": ['softwareVersionNumber'],
    'year': ['year'],
    'month': ['month'],
    'day': ['day'],
    'hour': ['hour'],
    'minute': ['minute'],
    'second': ['second'],
}

# True incoming BUFR observed variables.
raw_obsvars = ['airTemperature', 'dewpointTemperature', 'windDirection', 'windSpeed']

# A dictionary of station ID, lat, lon, elev is needed in case missing from BUFR msg.
STATIONS = {}

# Keep track of the stations that were found and those that are not.
stations_found = []
sites_not_found = []

# The outgoing IODA variables (ObsValues), their units, and assigned constant ObsError.
obsvars = ['airTemperature', 'virtualTemperature', 'specificHumidity', 'windEastward', 'windNorthward']
obsvars_units = ['K', 'K', 'kg kg-1', 'm s-1', 'm s-1']
obserrlist = [1.2, 1.2, 0.75E-3, 1.7, 1.7]

VarDims = {
    'airTemperature': ['Location'],
    'virtualTemperature': ['Location'],
    'specificHumidity': ['Location'],
    'windEastward': ['Location'],
    'windNorthward': ['Location']
}

AttrData = {
    'converter': os.path.basename(__file__),
    'ioda_version': 2,
    'description': 'Radiosonde observations converted from BUFR',
    'source': 'LDM at NCAR-RAL',
    'sourceFiles': ''
}

DimDict = {
}

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
double_missing_value = nc.default_fillvals['f8']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'
bufr_missing_value = ecc.CODES_MISSING_LONG

missing_vals = {'string': string_missing_value,
                'integer': int_missing_value,
                'long': long_missing_value,
                'float': float_missing_value,
                'double': double_missing_value}
dtypes = {'string': object,
          'integer': np.int32,
          'long': np.int64,
          'float': np.float32,
          'double': np.float64}

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])


def main(file_names, station_table, output_file, datetimeRef):

    # initialize
    count = [1, 0, 0, 0, 0]
    start_time = time.time()
    varDict = defaultdict(lambda: DefaultOrderedDict(dict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    obs_data = {}          # The final outputs.
    data = {}              # Before assigning the output types into the above.
    for key in obsvars:
        data[key] = []
    for key in meta_keys:
        data[key] = []

    # Load a static list (json) of station info: WMO number, lat, lon, elev
    STATIONS = loadStations(station_table)

    # Open one input file at a time.
    for fname in file_names:
        count[4] = 0
        AttrData['sourceFiles'] += ", " + fname

        # Read from a BUFR file.
        data, count = read_file(fname, count, data, datetimeRef)
        print(f"Finished reading {fname} and found {count[4]} distinct sites\n")

    AttrData['datetimeReference'] = datetimeRef
    AttrData['sourceFiles'] = AttrData['sourceFiles'][2:]
    logging.debug("All source files: " + AttrData['sourceFiles'])

    if not data:
        logging.critical("ABORT: no message data was captured, stopping execution.")
        sys.exit()

    if len(sites_not_found) > 0:
        print(f"\nList of stations with no lat/lon info and not in lookup table...")
        for site in sites_not_found:
            print(f"{site}")
        print("\n")

    logging.info("--- {:9.4f} BUFR read seconds ---".format(time.time() - start_time))

    nlocs = count[1]
    DimDict = {'Location': nlocs}

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

    # Set units of the MetaData variables and all _FillValues.
    for key in meta_keys:
        dtypestr = locationKeyList[meta_keys.index(key)][1]
        keep_or_toss = locationKeyList[meta_keys.index(key)][3]
        if (keep_or_toss == "keep"):
            if locationKeyList[meta_keys.index(key)][2]:
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


def read_file(file_name, count, data, datetimeRef):

    f = open(file_name, 'rb')
    fsize = os.path.getsize(file_name)
    logging.info(f"Processing file {file_name} with size: {fsize}")
    start_pos = 0

    while True:
        logging.info(f"  within file, reading BUFR msg ({count[0]}) starting at: {start_pos}")
        # Use eccodes to decode each bufr message in the file
        data, count, start_pos = read_bufr_message(f, count, start_pos, data, datetimeRef)
        count[0] += 1

        if ((start_pos is None) or (start_pos >= fsize)):
            break

    return data, count


# Replace BUFR missing value indicators with IODA missings.
def assign_values(data, key):

    if isinstance(data[0], str):
        for n, d in enumerate(data):
            data[n] = ''.join(c for c in d if c.isalnum())
            if not data[n]:
                data[n] = string_missing_value
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
        return np.array(data, dtype=object)

    logging.critical("ABORT, no matching datatype found for key: " + key)


# Assign the correct IODA missing value to MetaData element.
def assign_missing_meta(data, key, num, start):

    dtypestr = locationKeyList[meta_keys.index(key)][1]
    if dtypestr is None:
        logging.critical("ABORT, no matching datatype found for key: " + key)
    else:
        if start == 0:
            return np.full(num, missing_vals[dtypestr], dtype=dtypes[dtypestr])
        else:
            out_data = np.full(num, missing_vals[dtypestr], dtype=dtypes[dtypestr])
            for n in range(0, start):
                out_data[n] = data[n]
            return out_data


# Return True if all elements in the array are missing, otherwise False.
def is_all_missing(data):

    if data.dtype == np.float32:
        return all(x == float_missing_value for x in data)
    if data.dtype == np.float64:
        return all(x == double_missing_value for x in data)
    elif data.dtype == np.int32:
        return all(x == int_missing_value for x in data)
    elif data.dtype == np.int64:
        return all(x == long_missing_value for x in data)
    elif data.dtype.kind in {'U', 'S'}:
        return all(x == string_missing_value for x in data)

    logging.warning("data type not determined, returning False")
    return False


def get_normalized_bit(value, bit_index):
    return (value >> bit_index) & 1


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


def specialty_time(tvals, year, month, day, hour, minute, second):

    bad_date = False
    result = []

    # UGH, some BUFR uses 2-digit year, awful.
    if (year >= 0 and year <= 50):
        year += 2000
    elif (year > 50 and year <= 99):
        year += 1900

    if minute == int_missing_value:
        minute = 0
    elif minute < 0 or minute > 59:
        minute = 0

    if second == int_missing_value:
        second = 0
    elif second < 0 or second > 59:
        second = 0

    if (year < 1900 or year > 2499 or month < 1 or month > 12 or day < 1 or day > 31 or hour < 0 or hour > 23 or minute < 0 or minute > 59):
        bad_date = True

    if bad_date:
        year = 1900
        month = 1
        day = 1
        hour = 0
        minute = 0

    # This should be the launch or release time of sonde.
    logging.debug(f"Launch time info {year}-{month}-{day}T{hour}:{minute}:{second}Z")
    try:
        this_datetime = datetime(year, month, day, hour, minute, second)
    except Exception:
        logging.critical(f"Bogus launch time info {year}-{month}-{day}T{hour}:{minute}:{second}Z")
        year = 1900
        month = 1
        day = 1
        hour = 0
        minute = 0
        this_datetime = datetime(year, month, day, hour, minute, second)

    time_offset = round((this_datetime - epoch).total_seconds())

    result = np.full(len(tvals), time_offset)
    for n, tval in enumerate(tvals):
        if (tval < 0 or tval > 6*3600):
            result[n] = result[n-1]
        else:
            result[n] = time_offset + tval

    return result


def read_bufr_message(f, count, start_pos, data, datetimeRef):

    global STATIONS         # From JSON file of station ID, lat, lon, elev
    global stations_found   # As we find them, add to list.
    global sites_not_found  # Some sites missing lat/lon entirely; also do not exist in station table.
    temp_data = {}          # A temporary dictionary to hold things.
    meta_data = {}          # All the various MetaData go in here.
    vals = {}               # Floating-point ObsValues variables go in here.
    avals = []              # Temporarily hold array of values.
    start_pos = f.tell()

    met_utils = meteo_utils.meteo_utils()
    significance_table = def_significance_table()

    try:
        bufr = ecc.codes_bufr_new_from_file(f)
        try:
            msg_size = ecc.codes_get_message_size(bufr)
            logging.info(f"   will attempt to read BUFR msg with size: ({msg_size} bytes)")
        except:      # noqa
            return data, count, None
    except ecc.CodesInternalError:
        logging.critical(f"Useless BUFR message (codes_bufr_new_from_file)")
        start_pos = f.tell()
        return data, count, start_pos

    try:
        ecc.codes_set(bufr, 'skipExtraKeyAttributes', 1)   # Supposedly this is ~25 percent faster
        ecc.codes_set(bufr, 'unpack', 1)
    except ecc.CodesInternalError:
        ecc.codes_release(bufr)
        logging.info(f"INCOMPLETE BUFR message, skipping ({msg_size} bytes)")
        start_pos += int(0.5*msg_size)
        f.seek(start_pos)
        return data, count, start_pos

    # Some BUFR messages have subsets (multiple) soundings in a single message (China especially).
    # Therefore we have to loop through a vector of the delayedDescriptorReplicationFactor.
    try:
        nsubsets = ecc.codes_get(bufr, 'numberOfSubsets')
    except ecc.KeyValueNotFoundError:
        nsubsets = 1
        pass

    # Are the data compressed or uncompressed?  If the latter, then when nsubsets>1, we
    # have to do things differently.
    compressed = ecc.codes_get(bufr, 'compressedData')

    # Unfortunately, there is absolutely no way to handle compressed data with number of subsets>1
    if compressed and nsubsets > 1:
        logging.warning(f"CANNOT handle compressed data, skipping ({msg_size} bytes)")
        return data, count, start_pos

    '''
        This will print absolutely every BUFR key in the message.
            print(" ")
            iterid = ecc.codes_keys_iterator_new(bufr)
            while ecc.codes_keys_iterator_next(iterid):
                keyname = ecc.codes_keys_iterator_get_name(iterid)
                print(f" name: {keyname}")
    '''

    # If multiple soundings repfacs will be vector of length of each sounding.
    repfacs = []
    try:
        repfacs = ecc.codes_get_array(bufr, 'extendedDelayedDescriptorReplicationFactor').tolist()
    except ecc.KeyValueNotFoundError:
        try:
            repfacs = ecc.codes_get_array(bufr, 'extendedDelayedDescriptorAndDataRepetitionFactor').tolist()
        except ecc.KeyValueNotFoundError:
            try:
                repfacs = ecc.codes_get_array(bufr, 'delayedDescriptorReplicationFactor').tolist()
            except ecc.KeyValueNotFoundError:
                try:
                    repfacs = ecc.codes_get_array(bufr, 'delayedDescriptorAndDataRepetitionFactor').tolist()
                except ecc.KeyValueNotFoundError:
                    try:
                        repfacs = ecc.codes_get_array(bufr, 'shortDelayedDescriptorReplicationFactor').tolist()
                    except ecc.KeyValueNotFoundError:
                        pass

    # First, get the MetaData we are interested in (list is in metaDataKeyList)
    max_mlen = 0
    for k, v in metaDataKeyList.items():
        temp_data[k] = []
        if (len(v) > 1):
            for var in v:
                if (var != 'Constructed'):
                    try:
                        avals = ecc.codes_get_array(bufr, var)
                        max_mlen = max(max_mlen, len(avals))
                        temp_data[k] = assign_values(avals, k)
                        if not is_all_missing(temp_data[k]):
                            break
                    except ecc.KeyValueNotFoundError:
                        logging.debug("Caution: unable to find requested BUFR key: " + var)
                        temp_data[k] = None
                else:
                    temp_data[k] = None
        else:
            if (v[0] != 'Constructed'):
                try:
                    avals = ecc.codes_get_array(bufr, v[0])
                    max_mlen = max(max_mlen, len(avals))
                    temp_data[k] = assign_values(avals, k)
                except ecc.KeyValueNotFoundError:
                    logging.debug("Caution, unable to find requested BUFR key: " + v[0])
                    temp_data[k] = None
            else:
                temp_data[k] = None

    # Let us hope that date info is present, but there are no guarantees.
    if (temp_data['year'] is None) and (temp_data['month'] is None) and \
            (temp_data['day'] is None) and (temp_data['hour'] is None):
        logging.warning(f"BUFR msg contains NO DATE INFO. Assuming {datetimeRef}")
        temp_data['year'] = int(datetimeRef[0:4])
        temp_data['month'] = int(datetimeRef[5:7])
        temp_data['day'] = int(datetimeRef[8:10])
        temp_data['hour'] = int(datetimeRef[11:13])

    # There should also be latitude and longitude info, but also no guarantees.
    if (temp_data['latitude'] is None) and (temp_data['longitude'] is None):
        logging.warning("BUFR msg contains NO LAT/LON INFO, will try to find via station lookup.")
    else:
        lat_found = False
        for n, lat in enumerate(temp_data['latitude']):
            if lat > -90 and lat < 90:
                lat_found = True
                break
        if not lat_found:
            logging.warning(f"BUFR msg contains no valid latitude, will try to find via station lookup.")

    # Next, get the raw observed weather variables we want.
    # TO-DO: currently all ObsValue variables are float type, might need integer/other.
    max_dlen = 0
    repfactors = {}
    for variable in raw_obsvars:
        temp_data[variable] = []
        if not compressed and nsubsets > 1:
            repfactors[variable] = []
            for n in range(nsubsets):
                var = '/subsetNumber=' + str(n+1) + '/' + variable
                try:
                    avals = ecc.codes_get_array(bufr, var)
                    repfactors[variable].append(len(avals))
                    temp_data[variable] = np.append(temp_data[variable], assign_values(avals, variable))
                except ecc.KeyValueNotFoundError:
                    logging.debug("Caution, unable to find requested BUFR variable: " + variable)
                    temp_data[variable] = None
        else:
            repfactors[variable] = []
            try:
                avals = ecc.codes_get_array(bufr, variable)
                repfactors[variable].append(len(avals))
                max_dlen = max(max_dlen, len(avals))
                temp_data[variable] = assign_values(avals, variable)
            except ecc.KeyValueNotFoundError:
                logging.debug("Caution, unable to find requested BUFR variable: " + variable)
                temp_data[variable] = None

    if not repfacs:
        for variable in raw_obsvars:
            if repfactors[variable]:
                repfacs = repfactors[variable]
                break

    # From repfacs, make begin/end indicies for single or multiple soundings.
    nbeg = []
    nend = []
    if repfacs:
        if nsubsets > 1:
            if nsubsets != len(repfacs):
                logging.warning(f"Nonsense: number of subsets, {nsubsets} is not equal to "
                                f"the length of repfacs vector, {len(repfacs)}")
            nend = np.cumsum(repfacs)
            nbeg = np.insert(nend[:-1], 0, 0)
        else:
            nbeg.append(0)
            nend.append(repfacs[0]-1)
    else:
        nbeg.append(0)
        nend.append(int(1E6))

    # Be done with this BUFR message.
    ecc.codes_release(bufr)

    # Loop over each pair of beginning and ending indices and transfer data
    # In some circumstances, we have no idea how many vertical levels of data in a sonde are
    # upcoming, so we use 1E6 as largest possible number and hope to discover the true
    # number from some other variable (the multi-IF-block test below).
    obnum = 0
    for b, e in tuple(zip(nbeg, nend)):
        if b < 0 or e < 0 or e <= b:
            logging.warning(f"Skipping nonsense BUFR msg with a negative index [{b},{e}]")
            return data, count, start_pos
        logging.debug(f"Within BUFR msg, processing ob {obnum+1} with bounds: [{b},{e-1}]")
        if e < 999999:
            target_number = e - b
        else:
            logging.debug("Msg did not contain repfacs, trying to determine target number of obs")
            if temp_data['vertSignificance'] is not None:
                target_number = len(temp_data['vertSignificance'])
            elif temp_data['timeDisplacement'] is not None:
                target_number = len(temp_data['timeDisplacement'])
            elif temp_data['latDisplacement'] is not None:
                target_number = len(temp_data['latDisplacement'])
            elif temp_data['pressure'] is not None:
                target_number = len(temp_data['air_pressure'])
            elif temp_data['airTemperature'] is not None:
                target_number = len(temp_data['airTemperature'])
            elif temp_data['windSpeed'] is not None:
                target_number = len(temp_data['windSpeed'])
            else:
                print("HOW on earth is target_number zero?  BUFR sucks!")
                return data, count, start_pos
            e = target_number

        # For any of the MetaData elements that were totally lacking, fill entire vector with missing.
        empty = []
        for k, v in metaDataKeyList.items():
            meta_data[k] = assign_missing_meta(empty, k, target_number, 0)
            if temp_data[k] is None:
                next
            elif b == 0 and len(temp_data[k]) == 1:
                meta_data[k] = np.full(target_number, temp_data[k][0])
            else:
                if len(temp_data[k]) == nsubsets:
                    meta_data[k] = np.full(target_number, temp_data[k][obnum])
                else:
                    try:
                        meta_data[k] = temp_data[k][b:e]
                        if len(meta_data[k]) < target_number:
                            meta_data[k] = np.full(target_number, meta_data[k][0])
                    except Exception:
                        logging.warning(f"Failed copying temp_data to meta_data, var: {k}, ({b},{e}):{target_number}, len:{len(temp_data[k])}")
                        count[2] += target_number
                        return data, count, start_pos

        # Sondes are special with a launch time and time displacement.
        if temp_data['timeDisplacement'] is not None:
            meta_data['dateTime'] = specialty_time(temp_data['timeDisplacement'][b:e],
                      meta_data['year'][0], meta_data['month'][0], meta_data['day'][0],      # noqa
                      meta_data['hour'][0], meta_data['minute'][0], meta_data['second'][0])  # noqa
            meta_data['releaseTime'] = np.full(target_number, meta_data['dateTime'][0])
        else:
            meta_data['dateTime'][0] = specialty_time([0],
                      meta_data['year'][0], meta_data['month'][0], meta_data['day'][0],      # noqa
                      meta_data['hour'][0], meta_data['minute'][0], meta_data['second'][0])  # noqa
            meta_data['dateTime'] = np.full(target_number, meta_data['dateTime'][0])
            meta_data['releaseTime'] = np.full(target_number, meta_data['dateTime'][0])

        # Forcably create stationIdentification 5-char string from WMO block+station number.
        # If no WMO numbers, there could be WIGOS identifer or stationLongName
        meta_data['stationIdentification'] = np.full(target_number, string_missing_value, dtype='<S5')
        for n, block in enumerate(meta_data['wmoBlockNumber']):
            number = meta_data['wmoStationNumber'][n]
            if (block > 0 and block < 100 and number > 0 and number < 1000):
                meta_data['stationIdentification'][n] = "{:02d}".format(block) + "{:03d}".format(number)
                meta_data['stationIdentification'][n] = meta_data['stationIdentification'][n]
            elif meta_data['wigosIdentifier'][n] != string_missing_value:
                if n == 0:
                    logging.info(f"  this site lacks WMO number, using wigosIdentifier: {meta_data['wigosIdentifier'][n]}")
                    try:
                        block = int(meta_data['stationIdentification'][n][0:2])
                        number = int(meta_data['stationIdentification'][n][2:5])
                        meta_data['stationIdentification'][n] = "{:02d}".format(block) + "{:03d}".format(number)
                    except Exception:
                        meta_data['stationIdentification'][n] = meta_data['wigosIdentifier'][n]
                meta_data['stationIdentification'][n] = meta_data['stationIdentification'][0]
            elif meta_data['stationLongName'][n] != string_missing_value:
                meta_data['stationIdentification'][n] = meta_data['stationLongName'][n]
                if n == 0:
                    logging.info(f"  this site lacks WMO number, using stationLongName: {meta_data['stationIdentification'][n]}")
        count[3] += 1
        logging.debug(f"Processing sonde for station: {meta_data['stationIdentification'][0]}")

        # Sondes often have lat/lon displacement from launch/release location.
        if temp_data['latDisplacement'] is not None and temp_data['lonDisplacement'] is not None:
            need_guess_trajectory = False
            for n, delta_lat in enumerate(temp_data['latDisplacement'][b:e]):
                delta_lon = temp_data['lonDisplacement'][n+b]
                meta_data['latitude'][n] = meta_data['latitude'][0] + delta_lat
                meta_data['longitude'][n] = meta_data['longitude'][0] + delta_lon
        else:
            need_guess_trajectory = True
            lenl = len(meta_data['latitude'])
            if lenl == 1:
                if (meta_data['latitude'][0] == float_missing_value) or (meta_data['longitude'][0] == float_missing_value):
                    logging.critical(f"MISSING lat,lon of launch location for site {meta_data['stationIdentification'][0]}.")
                    # Get launch location from lookup table using stationId (perhaps)
                    station_str = meta_data['stationIdentification'][0].decode('UTF-8')
                    station = getStationInfo(wmoId=station_str)
                    if station is None:
                        logging.critical(f"NO match for {meta_data['stationIdentification'][0]} in station table.")
                        logging.critical(" there is literally no way to rescue this sounding data without lat/lon.")
                        sites_not_found.append(station_str)
                        count[2] += target_number
                        return data, count, start_pos
                    else:
                        logging.critical(f"MUST use launch location from table since missing in the data for {station_str}.")
                        meta_data['latitude'][0] = station['lat']
                        meta_data['longitude'][0] = station['lon']
                        meta_data['stationElevation'][0] = station['elev']
                        meta_data['latitude'] = np.full(target_number, meta_data['latitude'][0])
                        meta_data['longitude'] = np.full(target_number, meta_data['longitude'][0])
                        meta_data['stationElevation'] = np.full(target_number, meta_data['stationElevation'][0])
                else:
                    meta_data['latitude'] = np.full(target_number, meta_data['latitude'][0])
                    meta_data['longitude'] = np.full(target_number, meta_data['longitude'][0])
            elif lenl != target_number:
                logging.critical(f"POSSIBLE problem, mismatch in number: target = {target_number} but {lenl} found.")
                count[2] += target_number
                return data, count, start_pos
            else:
                yy = all(y == float_missing_value for y in meta_data['latitude'])
                xx = all(x == float_missing_value for x in meta_data['longitude'])
                if xx or yy:
                    station_str = meta_data['stationIdentification'][0].decode('UTF-8')
                    station = getStationInfo(wmoId=station_str)
                    if station is None:
                        logging.critical(f"NO match for {meta_data['stationIdentification'][0]} in station table.")
                        logging.critical(" there is literally no way to rescue this sounding data without lat/lon.")
                        sites_not_found.append(station_str)
                        count[2] += target_number
                        return data, count, start_pos
                    else:
                        logging.critical(f"MUST use launch location from table since missing in the data for {station_str}.")
                        meta_data['latitude'][0] = station['lat']
                        meta_data['longitude'][0] = station['lon']
                        meta_data['stationElevation'][0] = station['elev']
                        meta_data['stationElevation'] = np.full(target_number, meta_data['stationElevation'][0])
                        need_guess_trajectory = True
                else:
                    need_guess_trajectory = False

        # Force longitude into space of -180 to +180 only. Reset both lat/lon missing if either absent.
        mask_lat = np.logical_or(meta_data['latitude'] < -90.0, meta_data['latitude'] > 90.0)
        mask_lon = np.logical_or(meta_data['longitude'] < -180.0, meta_data['longitude'] > 360.0)
        meta_data['latitude'][mask_lat] = float_missing_value
        meta_data['longitude'][mask_lon] = float_missing_value
        meta_data['latitude'][mask_lon] = float_missing_value
        meta_data['longitude'][mask_lat] = float_missing_value
        for n, longitude in enumerate(meta_data['longitude']):
            if (meta_data['longitude'][n] != float_missing_value and meta_data['longitude'][n] > 180):
                meta_data['longitude'][n] = meta_data['longitude'][n] - 360.0

        # We will do anything we possibly can to rescue data having missing lat/lon info.
        if (meta_data['latitude'][0] == float_missing_value) or (meta_data['longitude'][0] == float_missing_value):
            station_str = meta_data['stationIdentification'][0].decode('UTF-8')
            station = getStationInfo(wmoId=station_str)
            if station is None:
                logging.critical(f"NO match for {meta_data['stationIdentification'][0]} in station table.")
                logging.critical(" there is literally no way to rescue this sounding data without lat/lon.")
                sites_not_found.append(station_str)
                count[2] += target_number
                return data, count, start_pos
            else:
                logging.critical(f"MUST use launch location from table since missing in the data for {station_str}.")
                meta_data['latitude'][0] = station['lat']
                meta_data['longitude'][0] = station['lon']
                meta_data['stationElevation'][0] = station['elev']
                meta_data['stationElevation'] = np.full(target_number, meta_data['stationElevation'][0])
                need_guess_trajectory = True

        # It is NOT IDEAL, but if missing a lat or lon during ascent, fill with prior known value for now.
        for n, lon in enumerate(meta_data['longitude']):
            lat = meta_data['latitude'][n]
            if (lon < -180 or lon > 180):
                meta_data['longitude'][n] = meta_data['longitude'][n-1]
            if (lat < -90 or lat > 90):
                meta_data['latitude'][n] = meta_data['latitude'][n-1]

        # TO-DO: test if more sounding levels in newer data compared to previous to decide to keep/skip.
        if meta_data['stationIdentification'][0] in stations_found:
            logging.debug(f"It appears {meta_data['stationIdentification'][0]} is a duplicate, skipping")
            return data, count, start_pos

        count[4] += 1
        this_id = meta_data['stationIdentification'][0]
        this_lat = meta_data['latitude'][0]
        this_lon = meta_data['longitude'][0]
        this_ele = meta_data['stationElevation'][0]
        logging.debug(f"Processing sonde for station: {this_id}  {this_lat}, {this_lon}, {this_ele}")

        # Very odd, sometimes the first level of data has some variables set to zero. Reset to missing.
        if (meta_data['geopotentialHeight'][0] == 0 or meta_data['pressure'][0] == 0):
            meta_data['geopotentialHeight'][0] = float_missing_value
            meta_data['pressure'][0] = float_missing_value

        # If all dateTime elements are the same (i.e., release time), then assume balloon ascends 5 m/s.
        tt = all(t == meta_data['dateTime'][0] for t in meta_data['dateTime'])
        if tt:
            for nt in range(1, len(meta_data['dateTime'])):
                prev_time = meta_data['dateTime'][nt-1]
                z2 = meta_data['geopotentialHeight'][nt]
                z1 = meta_data['geopotentialHeight'][nt-1]
                if (z1 != float_missing_value) and (z2 != float_missing_value):
                    dtime = (z2-z1)*0.2
                    meta_data['dateTime'][nt] = prev_time + dtime
                else:
                    meta_data['dateTime'][nt] = prev_time

        # And now processing the observed variables we care about.
        nbad = 0
        for variable in raw_obsvars:
            vals[variable] = np.full(target_number, float_missing_value)
            if temp_data[variable] is not None:
                try:
                    vals[variable] = temp_data[variable][b:e]
                    if len(vals[variable]) < target_number:
                        nbad += 1
                        logging.warning(f" var {variable} has {len(vals[variable])} "
                                        f"elements while expecting {target_number}")
                        vals[variable] = np.full(target_number, float_missing_value)
                except Exception:
                    logging.warning(f"Unable to copy {variable} data, "
                                    f"either index [{b},{e}] must be out of range.")
            else:
                nbad += 1

        if nbad == len(raw_obsvars):
            logging.warning(f"No usable data in this ob, skipping it.")
            count[2] += target_number
            return data, count, start_pos

        count[1] += target_number

        '''
          Need to transform some variables (wind speed/direction to components for example).
          In the ideal world, we could assume that the meteorological variables were given
          well-bounded values, but in BUFR, they could be garbage, so ensure that values
          are all sensible before calling the transformation functions.
        '''
        uwnd = np.full(target_number, float_missing_value)
        vwnd = np.full(target_number, float_missing_value)
        for n, wdir in enumerate(vals['windDirection']):
            wspd = vals['windSpeed'][n]
            if wdir and wspd:
                if (wdir >= 0 and wdir <= 360 and wspd >= 0 and wspd < 300):
                    uwnd[n], vwnd[n] = met_utils.dir_speed_2_uv(wdir, wspd)

        spfh = np.full(target_number, float_missing_value)
        for n, dewpoint in enumerate(vals['dewpointTemperature']):
            pres = meta_data['pressure'][n]
            if dewpoint and pres:
                if (dewpoint > 196 and dewpoint < 325 and pres > 100 and pres < 109900):
                    spfh[n] = met_utils.specific_humidity(dewpoint, pres)

        airt = np.full(target_number, float_missing_value)
        for n, temp in enumerate(vals['airTemperature']):
            if temp:
                if (temp > 50 and temp < 345):
                    airt[n] = temp

        tvirt = np.full(target_number, float_missing_value)
        for n, temp in enumerate(airt):
            pres = meta_data['pressure'][n]
            if (temp != float_missing_value and spfh[n] != float_missing_value and pres < 108000 and pres > 10000):
                qvapor = max(1.0e-12, spfh[n]/(1.0-spfh[n]))
                tvirt[n] = temp*(1.0 + 0.61*qvapor)

        # Finally fill up the output data dictionary with observed variables.
        data['windEastward'] = np.append(data['windEastward'], uwnd)
        data['windNorthward'] = np.append(data['windNorthward'], vwnd)
        data['specificHumidity'] = np.append(data['specificHumidity'], spfh)
        data['airTemperature'] = np.append(data['airTemperature'], airt)
        data['virtualTemperature'] = np.append(data['virtualTemperature'], tvirt)

        """
        If the lat/lon were missing, psuedo-guess the trajectory.
        Based on height and time and the wind componenents, predict the lat, lon positions
        as the balloon ascends.  Generally the balloon ascends at 5 m/s, which was already
        assumed in the creation of each timestamp.
        """

        if need_guess_trajectory:
            meta_data['positionEstimated'] = np.full(target_number, 1)
            location = [meta_data['longitude'][0], meta_data['latitude'][0], None]
            previous_loc = [meta_data['longitude'][0], meta_data['latitude'][0], None]
            delta_t = np.diff(meta_data['dateTime'])

            for idx in range(1, len(delta_t)):
                if (data['windEastward'][idx-1] != float_missing_value and data['windNorthward'][idx-1] != float_missing_value):
                    # move north-south
                    d_north = data['windNorthward'][idx-1] * delta_t[idx-1]
                    location = geod.direct(points=previous_loc[:2], azimuths=0., distances=d_north)[0]
                    meta_data['latitude'][idx] = location[1]
                    # move east-west
                    d_east = data['windEastward'][idx-1] * delta_t[idx-1]
                    location = geod.direct(points=location[:2], azimuths=90., distances=d_east)[0]
                    meta_data['longitude'][idx] = location[0]
                else:
                    meta_data['latitude'][idx] = meta_data['latitude'][idx-1]
                    meta_data['longitude'][idx] = meta_data['longitude'][idx-1]

                # store location for next step calculations
                previous_loc = dcop(location)

            # Be sure to delete the prior location info upon finishing
            del location
            del previous_loc
        else:
            meta_data['positionEstimated'] = np.full(target_number, 0)

        obnum += 1
        stations_found.append(meta_data['stationIdentification'][0])

        # Finally transfer the meta_data to the output array.
        for key in meta_keys:
            data[key] = np.append(data[key], meta_data[key])

    logging.info("number of observations so far: " + str(count[1]))
    logging.info("number of invalid or useless observations: " + str(count[2]))
    logging.info("number of sonde locations: " + str(count[3]))
    start_pos = f.tell()
    return data, count, start_pos


def loadStations(stationfile, skipIfLoaded=True):
    """
    Load station info from a .json file into the global station list. This info is needed
    when parsing data that is missing lat, lon, or elevation data at a site.
    :param stationfile: The filename to load
    :param skipIfLoaded: If True, don't load the station file if one has already been loaded
    :return: True on success, False on error
    """

    global STATIONS

    if skipIfLoaded and len(STATIONS) > 0:
        return True

    try:
        fh = open(stationfile, "r")
        data = fh.read()
        fh.close()
        STATIONS = json.loads(data)
        return True
    except Exception as e:
        logger.error("Could not read station info from json file '%s': %s" % (stationfile, e))
        sys.exit()


def getStationInfo(icaoId=None, wmoId=None):
    """
    Get info for a station given either an icaoId or a wmoId
    :param icaoId:
    :param wmoId:
    :return: A dict containing station info or None
    """
    if icaoId is None and wmoId is None:
        return None

    if len(STATIONS) == 0:
        logger.warning("Station file not loaded")
        return None

    station = None
    if wmoId:
        station = STATIONS[wmoId] if wmoId in STATIONS else None
    else:
        for synop in STATIONS:
            st = STATIONS[synop]
            if st['icao'].lower() == icaoId.lower():
                station = st
                wmoId = synop
                break

    if station:
        station['wmo'] = wmoId

    return station


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
    required.add_argument('-t', '--table', dest='station_table',
                          action='store', default=None, required=True,
                          help='Station table file (JSON)')

    parser.set_defaults(debug=False)
    parser.set_defaults(verbose=False)
    parser.set_defaults(datetimeReference=" ")
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--debug', action='store_true',
                          help='enable debug messages')
    optional.add_argument('--verbose', action='store_true',
                          help='enable verbose debug messages')
    optional.add_argument('--date', dest='datetimeReference',
                          action='store', default=' ',
                          help='date reference string (ISO8601)')

    args = parser.parse_args()

    if args.debug:
        logging.basicConfig(level=logging.INFO)
    elif args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.ERROR)

    for file_name in args.file_names:
        if not os.path.isfile(file_name):
            parser.error(f"Input (-i option) file: {file_name} does not exist")

    args.output_file = os.path.abspath(args.output_file)
    apath, afile = os.path.split(args.output_file)
    # create output directory path if necessary
    if not os.path.exists(apath):
        print("creating output directory: ", apath)
        os.makedirs(apath)

    main(args.file_names, args.station_table, args.output_file, args.datetimeReference)
