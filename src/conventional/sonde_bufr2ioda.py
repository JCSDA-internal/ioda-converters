#!/usr/bin/env python3

from datetime import datetime
import dateutil.parser
import os
from pathlib import Path
import sys
import time
import logging

import numpy as np
import netCDF4 as nc
import eccodes as ecc

# set path to ioda_conv_engines module
IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

# These modules need the path to lib-python modules
import ioda_conv_engines as iconv
import meteo_utils
from orddicts import DefaultOrderedDict
from collections import defaultdict, OrderedDict

os.environ["TZ"] = "UTC"

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
    ("wmoBlockNumber", "integer", "", "toss"),
    ("wmoStationNumber", "integer", "", "toss"),
    ("stationWMO", "string", "", "keep"),
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
    'wmoBlockNumber': ['blockNumber'],
    'wmoStationNumber': ['stationNumber'],
    'stationWMO': ['Constructed'],
    # "stationLongName": 'shipOrMobileLandStationIdentifier',
    # "instrumentType": 'radiosondeType',
    # "instrumentSerialNum": 'radiosondeSerialNumber',
    # "instrumentSoftwareVersion": 'softwareVersionNumber',
    # "instrumentHumidityCorrectionInfo": 'correctionAlgorithmsForHumidityMeasurements',
    # "instrumentRadiationCorrectionInfo": 'solarAndInfraredRadiationCorrection',
}

# True incoming BUFR observed variables.
raw_obsvars = ['airTemperature', 'dewpointTemperature', 'windDirection', 'windSpeed']

# The outgoing IODA variables (ObsValues), their units, and assigned constant ObsError.
obsvars = ['airTemperature', 'specificHumidity', 'windEastward', 'windNorthward']
obsvars_units = ['K', 'kg kg-1', 'm s-1', 'm s-1']
obserrlist = [1.2, 0.75E-3, 1.7, 1.7]

VarDims = {
    'airTemperature': ['Location'],
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


def main(file_names, output_file, datetimeRef):

    # initialize
    count = [1, 0, 0, 0]
    start_time = time.time()
    varDict = defaultdict(lambda: DefaultOrderedDict(dict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    obs_data = {}          # The final outputs.
    data = {}              # Before assigning the output types into the above.
    for key in obsvars:
        data[key] = []
    for key in meta_keys:
        data[key] = []

    # Open one input file at a time.
    for fname in file_names:
        AttrData['sourceFiles'] += ", " + fname

        # Read from a BUFR file.
        data, count = read_file(fname, count, data)

    AttrData['datetimeReference'] = datetimeRef
    AttrData['sourceFiles'] = AttrData['sourceFiles'][2:]
    logging.debug("All source files: " + AttrData['sourceFiles'])

    if not data:
        logging.critical("ABORT: no message data was captured, stopping execution.")
        sys.exit()

    logging.info("--- {:9.4f} BUFR read seconds ---".format(time.time() - start_time))

    nlocs = len(data['dateTime'])
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


def read_file(file_name, count, data):

    f = open(file_name, 'rb')
    fsize = os.path.getsize(file_name)
    logging.info(f"Processing file {file_name} with size: {fsize}")
    start_pos = 0

    while True:
        logging.info(f"  within file, reading BUFR msg ({count[0]}) starting at: {start_pos}")
        # Use eccodes to decode each bufr message in the file
        data, count, start_pos = read_bufr_message(f, count, start_pos, data)
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


def specialty_time(bufr, tvals):

    bad_date = False
    vals = []
    result = []

    try:
        vals = ecc.codes_get_array(bufr, 'year')
        year = vals[0]
        # UGH, some BUFR uses 2-digit year, awful.
        if (year >= 0 and year <= 50):
            year += 2000
        elif (year > 50 and year <= 99):
            year += 1900
    except ecc.KeyValueNotFoundError:
        logging.warning("Caution, no data for year")
        bad_date = True

    try:
        vals = ecc.codes_get_array(bufr, 'month')
        month = vals[0]
    except ecc.KeyValueNotFoundError:
        logging.warning("Caution, no data for month")
        bad_date = True

    try:
        vals = ecc.codes_get_array(bufr, 'day')
        day = vals[0]
    except ecc.KeyValueNotFoundError:
        logging.warning("Caution, no data for day")
        bad_date = True

    try:
        vals = ecc.codes_get_array(bufr, 'hour')
        hour = vals[0]
    except ecc.KeyValueNotFoundError:
        logging.warning("Caution, no data for hour")
        bad_date = True

    minute = 0
    try:
        vals = ecc.codes_get_array(bufr, 'minute')
        minute = vals[0]
        if (minute < 0 or minute > 59):
            minute = 0
    except ecc.KeyValueNotFoundError:
        logging.warning("Caution, no data for minute")
        bad_date = True

    second = 0
    try:
        vals = ecc.codes_get_array(bufr, 'second')
        second = vals[0]
        if (second < 0 or second > 59):
            second = 0
    except ecc.KeyValueNotFoundError:
        logging.info("Caution, no data for second")

    if (year < 1900 or year > 2499 or month < 1 or month > 12 or day < 1 or day > 31 or hour < 0 or hour > 23 or minute < 0 or minute > 59):
        bad_date = True

    if bad_date:
        year = 1900
        month = 1
        day = 1
        hour = 0
        minute = 0

    # This should be the launch or release time of sonde.
    logging.info(f"Launch time info {year}-{month}-{day}T{hour}:{minute}:{second}Z")
    this_datetime = datetime(year, month, day, hour, minute, second)
    time_offset = round((this_datetime - epoch).total_seconds())

    result = np.full(len(tvals), time_offset)
    for n, tval in enumerate(tvals):
        if (tval < 0 or tval > 6*3600):
            result[n] = result[n-1]
        else:
            result[n] = time_offset + tval

    return result


def read_bufr_message(f, count, start_pos, data):

    meta_data = {}         # All the various MetaData go in here.
    vals = {}              # Floating-point ObsValues variables go in here.
    avals = []             # Temporarily hold array of values.
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
        numberOfSubsets = ecc.codes_get(bufr, 'numberOfSubsets')
    except ecc.CodesInternalError:
        ecc.codes_release(bufr)
        logging.info(f"INCOMPLETE BUFR message, skipping ({msg_size} bytes)")
        start_pos += int(0.5*msg_size)
        f.seek(start_pos)
        return data, count, start_pos

    drepfac = 0
    krepfac = 0
    target_number = 0

    # First, get the MetaData we are interested in (list is in metaDataKeyList)
    for k, v in metaDataKeyList.items():
        meta_data[k] = []
        if (len(v) > 1):
            for var in v:
                if (var != 'Constructed'):
                    try:
                        avals = ecc.codes_get_array(bufr, var)
                        meta_data[k] = assign_values(avals, k)
                        if not is_all_missing(meta_data[k]):
                            break
                    except ecc.KeyValueNotFoundError:
                        logging.warning("Caution: unable to find requested BUFR key: " + var)
        else:
            if (v[0] != 'Constructed'):
                try:
                    avals = ecc.codes_get_array(bufr, v[0])
                    meta_data[k] = assign_values(avals, k)
                except ecc.KeyValueNotFoundError:
                    logging.warning("Caution, unable to find requested BUFR key: " + v[0])

    # Delay replication numbers most often associated with number of vertical levels in sonde.
    try:
        drepfac = ecc.codes_get(bufr, 'delayedDescriptorReplicationFactor')
    except ecc.KeyValueNotFoundError:
        pass
    try:
        krepfac = ecc.codes_get(bufr, 'extendedDelayedDescriptorReplicationFactor')
    except ecc.KeyValueNotFoundError:
        pass

    if krepfac > 0:
        target_number = krepfac
    elif drepfac > 0:
        target_number = drepfac
    if target_number > 0:
        logging.debug("Delayed and extendedDelayed replications: " + str(drepfac) + ", " + str(krepfac))
    else:
        if len(meta_data['vertSignificance']) > 0:
            target_number = len(meta_data['vertSignificance'])
        elif len(meta_data['timeDisplacement']) > 0:
            target_number = len(meta_data['timeDisplacement'])
        elif len(meta_data['latDisplacement']) > 0:
            target_number = len(meta_data['latDisplacement'])
        elif len(meta_data['latitude']) > 0:
            target_number = len(meta_data['latitude'])
    if target_number == 0:
        print("HOW on earth is target_number zero?  BUFR sucks!")
        return data, count, start_pos

    count[1] += target_number

    # For any of the MetaData elements that were totally lacking, fill entire vector with missing.
    empty = []
    for k, v in metaDataKeyList.items():
        if not any(meta_data[k]):
            meta_data[k] = assign_missing_meta(empty, k, target_number, 0)
        length = len(meta_data[k])
        if (length == 1):
            meta_data[k] = np.full(target_number, meta_data[k][0])
        elif (length < target_number):
            logging.warning(f"Key called {k} contains only {length} "
                            f" elements, whereas {target_number} were expected.")
            meta_data[k] = assign_missing_meta(meta_data[k], k, target_number, length-1)
        elif (length > target_number):
            logging.warning(f"Key called {k} contains {length} "
                            f" elements, whereas {target_number} were expected.")
            meta_data[k] = meta_data[k][:target_number]

    # Sondes are special with a launch time and time displacement.
    if meta_data['timeDisplacement'].any():
        meta_data['dateTime'] = specialty_time(bufr, meta_data['timeDisplacement'])
        meta_data['releaseTime'] = np.full(target_number, meta_data['dateTime'][0])
    else:
        count[2] += target_number
        return data, count, start_pos

    # Sondes also have lat/lon displacement from launch/release location.
    if (meta_data['latDisplacement'].any() and meta_data['lonDisplacement'].any()):
        for n, delta_lat in enumerate(meta_data['latDisplacement']):
            delta_lon = meta_data['lonDisplacement'][n]
            meta_data['latitude'][n] = meta_data['latitude'][0] + delta_lat
            meta_data['longitude'][n] = meta_data['longitude'][0] + delta_lon
    else:
        count[2] += target_number
        return data, count, start_pos

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

    # It is NOT IDEAL, but if missing a lat or lon, fill with prior known value for now.
    for n, lon in enumerate(meta_data['longitude']):
        lat = meta_data['latitude'][n]
        if (lon < -180 or lon > 180):
            meta_data['longitude'][n] = meta_data['longitude'][n-1]
        if (lat < -90 or lat > 90):
            meta_data['latitude'][n] = meta_data['latitude'][n-1]

    # Forcably create station_id 5-char string from WMO block+station number.
    meta_data['stationWMO'] = np.full(target_number, string_missing_value, dtype='<S5')
    for n, block in enumerate(meta_data['wmoBlockNumber']):
        number = meta_data['wmoStationNumber'][n]
        if (block > 0 and block < 100 and number > 0 and number < 1000):
            meta_data['stationWMO'][n] = "{:02d}".format(block) + "{:03d}".format(number)
        if n == 1:
            count[3] += 1
            logging.info(f"Processing sonde for station: {meta_data['stationWMO'][n]}")

    # Next, get the raw observed weather variables we want.
    # TO-DO: currently all ObsValue variables are float type, might need integer/other.
    for variable in raw_obsvars:
        vals[variable] = []
        try:
            avals = ecc.codes_get_array(bufr, variable)
            if (len(avals) != target_number):
                logging.warning(f"Variable called {variable} contains {len(avals)} "
                                f" elements, wheras {target_number} were expected.")
                count[2] += target_number
                return data, count, start_pos
            vals[variable] = assign_values(avals, variable)
        except ecc.KeyValueNotFoundError:
            logging.warning("Caution, unable to find requested BUFR variable: " + variable)
            vals[variable] = np.full(target_number, float_missing_value, dtype=np.float32)

    # Be done with this BUFR message.
    ecc.codes_release(bufr)

    # Need to transform some variables to others (wind speed/direction to components for example).
    uwnd = np.full(target_number, float_missing_value)
    vwnd = np.full(target_number, float_missing_value)
    for n, wdir in enumerate(vals['windDirection']):
        wspd = vals['windSpeed'][n]
        if (wdir >= 0 and wdir <= 360 and wspd >= 0 and wspd < 300):
            uwnd[n], vwnd[n] = met_utils.dir_speed_2_uv(wdir, wspd)

    spfh = np.full(target_number, float_missing_value)
    for n, dewpoint in enumerate(vals['dewpointTemperature']):
        pres = meta_data['pressure'][n]
        if (dewpoint > 50 and dewpoint < 325 and pres > 100 and pres < 109900):
            spfh[n] = met_utils.specific_humidity(dewpoint, pres)

    # Very odd, sometimes the first level of data has some variables set to zero. Reset to missing.
    if (meta_data['geopotentialHeight'][0] == 0 or meta_data['pressure'][0] == 0):
        meta_data['geopotentialHeight'][0] = float_missing_value
        meta_data['pressure'][0] = float_missing_value
    mask_T = np.logical_or(vals['airTemperature'] < 50, vals['airTemperature'] > 335)
    vals['airTemperature'][mask_T] = float_missing_value

    # Move everything into the final data dictionary, including metadata.
    data['windEastward'] = np.append(data['windEastward'], uwnd)
    data['windNorthward'] = np.append(data['windNorthward'], vwnd)
    data['specificHumidity'] = np.append(data['specificHumidity'], spfh)
    data['airTemperature'] = np.append(data['airTemperature'], vals['airTemperature'])
    for key in meta_keys:
        data[key] = np.append(data[key], meta_data[key])

    logging.info("number of observations so far: " + str(count[1]))
    logging.info("number of invalid or useless observations: " + str(count[2]))
    logging.info("number of sonde locations: " + str(count[3]))
    start_pos = f.tell()
    return data, count, start_pos


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
            parser.error('Input (-i option) file: ', file_name, ' does not exist')

    args.output_file = os.path.abspath(args.output_file)
    apath, afile = os.path.split(args.output_file)
    # create output directory path if necessary
    if not os.path.exists(apath):
        print("creating output directory: ", apath)
        os.makedirs(apath)

    main(args.file_names, args.output_file, args.datetimeReference)
