#!/usr/bin/env python3

from datetime import datetime, timedelta
import dateutil.parser
import os
from pathlib import Path
import sys
import time
import logging

import numpy as np
import netCDF4 as nc
import eccodes as ecc
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
    ("station_id", "string", "", "keep"),
    # ("station_name", "string", "", "keep"),
    ("wmoBlockNumber", "integer", "", "toss"),
    ("wmoStationNumber", "integer", "", "toss"),
    ("latitude", "float", "degrees_north", "keep"),
    ("longitude", "float", "degrees_east", "keep"),
    ("station_elevation", "float", "m", "keep"),
    ("height", "float", "m", "keep"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z", "keep"),
    ("year", "integer", "", "toss"),
    ("month", "integer", "", "toss"),
    ("day", "integer", "", "toss"),
    ("hour", "integer", "", "toss"),
    ("minute", "integer", "", "toss"),
    ("second", "integer", "", "toss")
]
meta_keys = [m_item[0] for m_item in locationKeyList]

metaDataKeyList = {
    'wmoBlockNumber': ['blockNumber'],
    'wmoStationNumber': ['stationNumber'],
    # 'station_name': ['stationOrSiteName'],   This fails due to unicode characters
    'latitude': ['latitude'],
    'longitude': ['longitude'],
    'station_elevation': ['heightOfStationGroundAboveMeanSeaLevel'],
    'height': ['Constructed',
               'heightOfBarometerAboveMeanSeaLevel',
               'heightOfStationGroundAboveMeanSeaLevel'],
    'station_id': ['Constructed'],
    'dateTime': ['Constructed'],
    'year': ['year'],
    'month': ['month'],
    'day': ['day'],
    'hour': ['hour'],
    'minute': ['minute'],
    'second': ['second']
}

var_mimic_length = "latitude"

# True incoming BUFR observed variables.
raw_obsvars = ['airTemperature',
               'dewpointTemperature',
               'windDirection',
               'windSpeed',
               'nonCoordinatePressure']

# The outgoing IODA variables (ObsValues), their units, and assigned constant ObsError.
obsvars = ['air_temperature',
           'specific_humidity',
           'virtual_temperature',
           'eastward_wind',
           'northward_wind',
           'surface_pressure']
obsvars_units = ['K', 'kg kg-1', 'K', 'm s-1', 'm s-1', 'Pa']
obserrlist = [1.2, 0.75E-3, 1.5, 1.7, 1.7, 120.0]

VarDims = {
    'air_temperature': ['nlocs'],
    'specific_humidity': ['nlocs'],
    'virtual_temperature': ['nlocs'],
    'eastward_wind': ['nlocs'],
    'northward_wind': ['nlocs'],
    'surface_pressure': ['nlocs']
}

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

AttrData = {
    'converter': os.path.basename(__file__),
    'ioda_version': 2,
    'description': 'Surface (SYNOP) observations converted from BUFR',
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
bufr_missing_value = ecc.CODES_MISSING_LONG

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

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


def main(file_names, output_file):

    # initialize
    count = [0, 0, 0]
    start_pos = None
    start_time = time.time()

    obs_data = {}          # The final outputs.
    data = {}              # Before assigning the output types into the above.
    for key in obsvars:
        data[key] = []
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
            for n in range(start, num-1, 1):
                np.append(data, missing_vals[dtypestr])
                return data


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


def specialty_time(year, month, day, hour, minute, second):

    bad_date = False

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

    try:
        this_datetime = datetime(year, month, day, hour, minute, second)
    except Exception:
        logging.critical(f"Bogus time info {year}-{month}-{day}T{hour}:{minute}:{second}Z")
        year = 1900
        month = 1
        day = 1
        hour = 0
        minute = 0
        this_datetime = datetime(year, month, day, hour, minute, second)

    time_offset = round((this_datetime - epoch).total_seconds())

    return time_offset


def read_file(file_name, count, start_pos, data):

    f = open(file_name, 'rb')

    while True:
        count[0] += 1
        # Use eccodes to decode each bufr message in the file
        data, count, start_pos = read_bufr_message(f, count, start_pos, data)

        if start_pos is None:
            break

    return data, count, start_pos


def read_bufr_message(f, count, start_pos, data):

    temp_data = {}         # A temporary dictionary to hold things.
    meta_data = {}         # All the various MetaData go in here.
    vals = {}              # Floating-point ObsValues variables go in here.
    avals = []             # Temporarily hold array of values.
    call_fail = False
    start_pos = f.tell()

    met_utils = meteo_utils.meteo_utils()

    try:
        bufr = ecc.codes_bufr_new_from_file(f)
        try:
            msg_size = ecc.codes_get_message_size(bufr)
            logging.info(f"Will attempt to read BUFR msg [{count[0]}] with size: ({msg_size} bytes)")
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

    # Some BUFR messages have subsets (multiple) observations in a single message.
    # Therefore we have to loop through a vector of the delayedDescriptorReplicationFactor.
    try:
        nsubsets = ecc.codes_get(bufr, 'numberOfSubsets')
    except ecc.KeyValueNotFoundError:
        nsubsets = 1
        pass

    # Are the data compressed or uncompressed?  If the latter, then when nsubsets>1, we
    # have to do things differently.
    compressed = ecc.codes_get(bufr, 'compressedData')

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
        if temp_data[k] is not None:
            logging.info(f" length of var {k} is: {len(temp_data[k])}")

    # These meta data elements are so critical that we should quit quickly if lacking them:
    if (temp_data['year'] is None) and (temp_data['month'] is None) and \
            (temp_data['day'] is None) and (temp_data['hour'] is None):
        logging.warning("Useless ob without date info.")
        count[2] += target_number
        return data, count, start_pos
    if (temp_data['wmoBlockNumber'] is None) and (temp_data['wmoStationNumber'] is None) and \
            (temp_data['latitude'] is None) and (temp_data['longitude'] is None):
        logging.warning("Useless ob without lat,lon or station number info.")
        count[2] += target_number
        return data, count, start_pos

    # Next, get the raw observed weather variables we want.
    # TO-DO: currently all ObsValue variables are float type, might need integer/other.
    max_dlen = 0
    for variable in raw_obsvars:
        temp_data[variable] = []
        if not compressed and nsubsets > 1:
            for n in range(nsubsets):
                var = '/subsetNumber=' + str(n+1) + '/' + variable
                try:
                    avals = ecc.codes_get_array(bufr, var)
                    temp_data[variable] = np.append(temp_data[variable], assign_values(avals, variable))
                except ecc.KeyValueNotFoundError:
                    logging.debug("Caution, unable to find requested BUFR variable: " + variable)
                    temp_data[variable] = None
        else:
            try:
                avals = ecc.codes_get_array(bufr, variable)
                max_dlen = max(max_dlen, len(avals))
                temp_data[variable] = assign_values(avals, variable)
            except ecc.KeyValueNotFoundError:
                logging.debug("Caution, unable to find requested BUFR variable: " + variable)
                temp_data[variable] = None
        if temp_data[k] is not None:
            logging.info(f" length of var {variable} is: {len(temp_data[variable])}")

    # Be done with this BUFR message.
    ecc.codes_release(bufr)

    # Transfer the meta data from its temporary vector into final its meta data var.
    target_number = nsubsets
    empty = []
    for k, v in metaDataKeyList.items():
        meta_data[k] = assign_missing_meta(empty, k, target_number, 0)
        if temp_data[k] is None:
            next
        else:
            if len(temp_data[k]) == target_number:
                meta_data[k] = temp_data[k]
            else:
                meta_data[k] = np.full(target_number, temp_data[k][0])

    # To construct a dateTime, we always need its components.
    if temp_data['year'] is not None:
        for n in range(nsubsets):
            meta_data['dateTime'][n] = specialty_time(meta_data['year'][n], meta_data['month'][n],
                                       meta_data['day'][n], meta_data['hour'][n],            # noqa
                                       meta_data['minute'][n], meta_data['second'][n])       # noqa

    # Force longitude into space of -180 to +180 only. Reset both lat/lon missing if either absent.
    mask_lat = np.logical_or(meta_data['latitude'] < -90.0, meta_data['latitude'] > 90.0)
    mask_lon = np.logical_or(meta_data['longitude'] < -180.0, meta_data['longitude'] > 360.0)
    meta_data['latitude'][mask_lat] = float_missing_value
    meta_data['longitude'][mask_lon] = float_missing_value
    meta_data['latitude'][mask_lon] = float_missing_value
    meta_data['longitude'][mask_lat] = float_missing_value
    for n, longitude in enumerate(meta_data['longitude']):
        if (meta_data['longitude'][n] != float_missing_value and meta_data['longitude'][n] > 360):
            meta_data['longitude'][n] = 360.0 - meta_data['longitude'][n]

    # If the height/altitude is unreasonable, then it is useless.
    mask_height = np.logical_or(meta_data['height'] < -425, meta_data['height'] > 8500)
    meta_data['height'][mask_height] = float_missing_value

    # If the height of the observation (sensor) is missing, try to fill it with station_elevation.
    for n, elev in enumerate(meta_data['station_elevation']):
        if (elev > -425 and elev < 8500):
            meta_data['height'][n] = elev + 2

    # Forcably create station_id 5-char string from WMO block+station number.
    meta_data['station_id'] = np.full(target_number, string_missing_value, dtype='<S5')
    for n, block in enumerate(meta_data['wmoBlockNumber']):
        number = meta_data['wmoStationNumber'][n]
        if (block > 0 and block < 100 and number > 0 and number < 1000):
            meta_data['station_id'][n] = "{:02d}".format(block) + "{:03d}".format(number)

    # And now processing the observed variables we care about.
    nbad = 0
    for variable in raw_obsvars:
        vals[variable] = np.full(target_number, float_missing_value)
        if temp_data[variable] is not None:
            logging.info(f" length of var {variable} is: {len(temp_data[variable])}")
            if len(temp_data[variable]) == target_number:
                vals[variable] = temp_data[variable]
            elif len(temp_data[variable]) < target_number:
                logging.warning(f" var {variable} has {len(temp_data[variable])} "
                                f"elements while expecting {target_number}")
                lendat = len(temp_data[variable])
                vals[variable][0:lendat] = temp_data[variable]
            else:
                logging.warning(f" var {variable} has {len(temp_data[variable])} "
                                f"elements while expecting {target_number}")
                vals[variable] = temp_data[variable][0:target_number]
        else:
            nbad += 1

    if nbad == len(raw_obsvars):
        logging.warning(f"No usable data in this ob.")
        count[2] += target_number
        return data, count, start_pos

    count[1] += target_number

    # Finally transfer the meta_data to the output array.
    for key in meta_keys:
        data[key] = np.append(data[key], meta_data[key])

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

    airt = np.full(target_number, float_missing_value)
    for n, temp in enumerate(vals['airTemperature']):
        if temp:
            if (temp > 50 and temp < 345):
                airt[n] = temp

    psfc = np.full(target_number, float_missing_value)
    for n, p in enumerate(vals['nonCoordinatePressure']):
        if p:
            if (p > 30000 and p < 109900):
                psfc[n] = p

    spfh = np.full(target_number, float_missing_value)
    tvirt = np.full(target_number, float_missing_value)
    for n, dewpoint in enumerate(vals['dewpointTemperature']):
        if dewpoint:
            if (dewpoint > 90 and dewpoint < 325 and psfc[n] != float_missing_value):
                spfh[n] = met_utils.specific_humidity(dewpoint, psfc[n])
                if (airt[n] != float_missing_value):
                    qvapor = max(1.0e-12, spfh[n]/(1.0-spfh[n]))
                    tvirt[n] = airt[n]*(1.0 + 0.61*qvapor)

    # Finally fill up the output data dictionary with observed variables.
    data['eastward_wind'] = np.append(data['eastward_wind'], uwnd)
    data['northward_wind'] = np.append(data['northward_wind'], vwnd)
    data['specific_humidity'] = np.append(data['specific_humidity'], spfh)
    data['air_temperature'] = np.append(data['air_temperature'], airt)
    data['virtual_temperature'] = np.append(data['virtual_temperature'], tvirt)
    data['surface_pressure'] = np.append(data['surface_pressure'], psfc)

    logging.info(f"number of observations so far: {count[1]} from {count[0]} BUFR msgs.")
    logging.info(f"number of invalid or useless observations: {count[2]}")
    start_pos = f.tell()
    return data, count, start_pos


if __name__ == "__main__":

    import argparse

    parser = argparse.ArgumentParser(
        description=(
            'Read surface obs (SYNOP) BUFR file and convert into IODA output file')
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
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--debug', action='store_true',
                          help='enable debug messages')
    optional.add_argument('--verbose', action='store_true',
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
