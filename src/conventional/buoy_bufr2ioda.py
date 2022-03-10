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
    ("station_id", "integer", ""),
    ("station_name", "string", ""),
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("station_elevation", "float", "m"),
    ("height", "float", "m"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z")
]
meta_keys = [m_item[0] for m_item in locationKeyList]

metaDataKeyList = {
    'station_id': ['marineObservingPlatformIdentifier'],
    'station_name': ['stationOrSiteName'],
    'latitude': ['latitude'],
    'longitude': ['longitude'],
    'station_elevation': ['heightOfStationGroundAboveMeanSeaLevel'],
    'height': ['heightOfSensorAboveWaterSurface',
               'heightOfBarometerAboveMeanSeaLevel'],
    'dateTime': ['Constructed']
}

var_mimic_length = "latitude"

# True incoming BUFR observed variables.
raw_obsvars = ['airTemperature',
               'dewpointTemperature',
               'seaSurfaceTemperature',
               'windDirection',
               'windSpeed',
               'nonCoordinatePressure',
               'pressureReducedToMeanSeaLevel']

# The outgoing IODA variables (ObsValues), their units, and assigned constant ObsError.
obsvars = ['air_temperature',
           'specific_humidity',
           'sea_surface_temperature',
           'eastward_wind',
           'northward_wind',
           'surface_pressure']
obsvars_units = ['K', 'kg kg-1', 'K', 'm s-1', 'm s-1', 'Pa']
obserrlist = [1.2, 0.75E-3, 2.2, 1.7, 1.7, 120.0]

VarDims = {
    'air_temperature': ['nlocs'],
    'specific_humidity': ['nlocs'],
    'sea_surface_temperature': ['nlocs'],
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
    'description': 'Surface (Ship) observations converted from BUFR',
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
    logging.info("BUFR message number: " + str(count[0]))

    met_utils = meteo_utils.meteo_utils()

    try:
        bufr = ecc.codes_bufr_new_from_file(f)
    except:  # noqa
        logging.critical("ABORT, failue when attempting to call:  codes_bufr_new_from_file")

    try:
        ecc.codes_set(bufr, 'skipExtraKeyAttributes', 1)   # Supposedly this is ~25 percent faster
        ecc.codes_set(bufr, 'unpack', 1)
    except:  # noqa
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

    # Determine the target number of observation points from a critical variable (i.e., latitude).
    target_number = len(meta_data[var_mimic_length])
    logging.debug("Target number of obs in this BUFR message: " + str(target_number))

    # For any of the MetaData elements that were totally lacking, fill entire vector with missing.
    empty = []
    for k, v in metaDataKeyList.items():
        if not any(meta_data[k]):
            meta_data[k] = assign_missing_meta(empty, k, target_number, 0)
        if (len(meta_data[k]) == 1):
            meta_data[k] = np.full(target_number, meta_data[k][0])
        elif (len(meta_data[k]) < target_number):
            logging.warning(f"Key called {k} contains only {len(meta_data[k])} "
                            f" elements, wheras {target_number} were expected.")
            meta_data[k] = assign_missing_meta(meta_data[k], k, target_number, len(meta_data[k])-1)
        elif (len(meta_data[k]) > target_number):
            logging.warning(f"Key called {k} contains {len(meta_data[k])} "
                            f" elements, wheras {target_number} were expected.")
            meta_data[k] = meta_data[k][:target_number]

    # Plus, to construct a dateTime, we always need its components.
    try:
        year = ecc.codes_get_array(bufr, 'year')
        if (len(year) < target_number):
            year = np.full(target_number, year[0])
        year[year < 1900] = 1900
        year[year > 2399] = 1900
    except ecc.KeyValueNotFoundError:
        logging.warning("Caution, no data for year")
        year = np.full(target_number, 1900)

    try:
        month = ecc.codes_get_array(bufr, 'month')
        if (len(month) < target_number):
            month = np.full(target_number, month[0])
        year[np.logical_or(month < 1, month > 12)] = 1900
        month[np.logical_or(month < 1, month > 12)] = 1
    except ecc.KeyValueNotFoundError:
        logging.warning("Caution, no data for month")
        year = np.full(target_number, 1900)
        month = np.full(target_number, 1)

    try:
        day = ecc.codes_get_array(bufr, 'day')
        if (len(day) < target_number):
            day = np.full(target_number, day[0])
        year[np.logical_or(day < 1, day > 31)] = 1900
        day[np.logical_or(day < 1, day > 31)] = 1
    except ecc.KeyValueNotFoundError:
        logging.warning("Caution, no data for day")
        year = np.full(target_number, 1900)
        day = np.full(target_number, 1)

    try:
        hour = ecc.codes_get_array(bufr, 'hour')
        if (len(hour) < target_number):
            hour = np.full(target_number, hour[0])
        year[np.logical_or(hour < 0, hour > 23)] = 1900
        hour[np.logical_or(hour < 0, hour > 23)] = 0
    except ecc.KeyValueNotFoundError:
        logging.warning("Caution, no data for hour")
        year = np.full(target_number, 1900)
        hour = np.full(target_number, 0)

    try:
        minute = ecc.codes_get_array(bufr, 'minute')
        if (len(minute) < target_number):
            minute = np.full(target_number, minute[0])
        year[np.logical_or(minute < 0, minute > 59)] = 1900
        minute[np.logical_or(minute < 0, minute > 59)] = 0
    except ecc.KeyValueNotFoundError:
        logging.warning("Caution, no data for minute")
        year = np.full(target_number, 1900)
        minute = np.full(target_number, 0)

    second = np.full(target_number, 0)
    try:
        avals = ecc.codes_get_array(bufr, 'second')    # non-integer value, optional
        if (len(avals) < target_number):
            avals = np.full(target_number, avals[0])
        for n, a in enumerate(avals):
            if (a > 0 and a < 60):
                second[n] = round(a)
    except ecc.KeyValueNotFoundError:
        logging.info("Caution, no data for second")

    for n, yyyy in enumerate(year):
        this_datetime = datetime(yyyy, month[n], day[n], hour[n], minute[n], second[n])
        time_offset = round((this_datetime - epoch).total_seconds())
        if (time_offset > -1E9):
            meta_data['dateTime'][n] = time_offset

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
    mask_height = np.logical_or(meta_data['height'] < -425, meta_data['height'] > 800)
    meta_data['height'][mask_height] = float_missing_value

    # If the height of the observation (sensor) is missing, try to fill it with station_elevation.
    for n, elev in enumerate(meta_data['station_elevation']):
        if (elev > -425 and elev < 800 and np.abs(meta_data['height'][n]-elev) > 50):
            meta_data['height'][n] = elev + 2
        else:
            meta_data['station_elevation'][n] = 0.5
            meta_data['height'][n] = 2.0

    # Next, get the raw observed weather variables we want.
    # TO-DO: currently all ObsValue variables are float type, might need integer/other.
    for variable in raw_obsvars:
        vals[variable] = []
        try:
            avals = ecc.codes_get_array(bufr, variable)
            if (len(avals) != target_number):
                logging.warning(f"Variable called {variable} contains only {len(avals)} "
                                f" elements, wheras {target_number} were expected.")
                count[2] += target_number
                return data, count, start_pos
            vals[variable] = assign_values(avals, variable)
        except ecc.KeyValueNotFoundError:
            logging.warning("Caution, unable to find requested BUFR variable: " + variable)
            vals[variable] = np.full(target_number, float_missing_value, dtype=np.float32)

    # Be done with this BUFR message.
    ecc.codes_release(bufr)

    # Count the locations for which time, lat, lon, or height is nonsense, therefore ob is useless.
    count[1] += target_number
    mask_date = np.full(target_number, 0, dtype=np.int32)
    mask_ll = np.full(target_number, 0, dtype=np.int32)
    mask_z = np.full(target_number, 0, dtype=np.int32)
    mask_date[year == 1900] = 1
    mask_ll[meta_data['latitude'] == float_missing_value] = 1
    mask_z[meta_data['height'] == float_missing_value] = 1
    for n, x in enumerate(mask_date):
        if (mask_date[n] == 1 or mask_ll[n] == 1 or mask_z[n] == 1):
            count[2] += 1

    # Need to transform some variables to others (wind speed/direction to components for example).
    uwnd = np.full(target_number, float_missing_value)
    vwnd = np.full(target_number, float_missing_value)
    for n, wdir in enumerate(vals['windDirection']):
        if (wdir >= 0 and wdir <= 360 and vals['windSpeed'][n] != float_missing_value):
            uwnd[n], vwnd[n] = met_utils.dir_speed_2_uv(wdir, vals['windSpeed'][n])

    # Most ships are floating at or near sea level, so assign MSLP to surface_pressure if needed.
    for n, psfc in enumerate(vals['nonCoordinatePressure']):
        mslp = vals['pressureReducedToMeanSeaLevel'][n]
        if ((psfc < 75000 or psfc > 107900) and (mslp > 75000 and mslp < 107900)):
            vals['nonCoordinatePressure'][n] = mslp

    spfh = np.full(target_number, float_missing_value)
    for n, dewpoint in enumerate(vals['dewpointTemperature']):
        psfc = vals['nonCoordinatePressure'][n]
        if (dewpoint > 90 and dewpoint < 325 and psfc > 30000 and psfc < 109900):
            spfh[n] = met_utils.specific_humidity(dewpoint, psfc)

    # Move everything into the final data dictionary, including metadata.
    data['eastward_wind'] = np.append(data['eastward_wind'], uwnd)
    data['northward_wind'] = np.append(data['northward_wind'], vwnd)
    data['specific_humidity'] = np.append(data['specific_humidity'], spfh)
    data['air_temperature'] = np.append(data['air_temperature'], vals['airTemperature'])
    data['surface_pressure'] = np.append(data['surface_pressure'], vals['nonCoordinatePressure'])
    data['sea_surface_temperature'] = np.append(data['sea_surface_temperature'], vals['seaSurfaceTemperature'])
    for key in meta_keys:
        data[key] = np.append(data[key], meta_data[key])

    logging.info("number of observations so far: " + str(count[1]))
    logging.info("number of invalid or useless observations: " + str(count[2]))
    return data, count, start_pos


if __name__ == "__main__":

    import argparse

    parser = argparse.ArgumentParser(
        description=(
            'Read buoy (surface obs) BUFR file and convert into IODA output file')
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
