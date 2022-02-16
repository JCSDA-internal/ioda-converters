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
    'presure': ['Constructed'],
    'dateTime': ['Constructed'],
}

# True incoming BUFR observed variables.
raw_obsvars = ['airTemperature', 'mixingRatio', 'windDirection', 'windSpeed']

# The outgoing IODA variables and their units.
obsvars = {
    'ob_temp': 'air_temperature',
    'ob_spfh': 'specific_humidity',
    'ob_uwnd': 'eastward_wind',
    'ob_vwnd': 'northward_wind',
}
obsvars_units = ['K', 'kg kg-1', 'm s-1', 'm s-1']

VarDims = {
    'ob_temp': ['nlocs'],
    'ob_spfh': ['nlocs'],
    'ob_uwnd': ['nlocs'],
    'ob_vwnd': ['nlocs'],
}

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
bufr_missing_float = 1.E11
bufr_missing_int = 1.E11
bufr_missing_string = '???'

ioda_datetime_epoch = datetime(1970, 1, 1, 0, 0, 0)  # Jan 1, 1970 00Z


def main(file_names, output_file):

    # initialize
    obs_data = {}
    count = [0, 0]
    start_pos = None
    start_time = time.time()

    varDict = defaultdict(lambda: DefaultOrderedDict(dict))
    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    for fname in file_names:
        print("INFO: Reading file: ", fname)

        AttrData['source_files'] += ", " + fname

        file_obs_data, count, start_pos = read_file(fname, count, start_pos)
        if obs_data:
            concat_obs_dict(obs_data, file_obs_data)
        else:
            obs_data = file_obs_data

    AttrData['source_files'] = AttrData['source_files'][2:]
    print("INFO: All source files: ", AttrData['source_files'])

    if not obs_data:
        print("WARNING: no message data was captured, stopping execution.")
        sys.exit()

    print("--- %s BUFR read seconds ---" % (time.time() - start_time))

    nlocs = obs_data[('dateTime', 'MetaData')].shape[0]
    DimDict = {'nlocs': nlocs}
    AttrData['nlocs'] = np.int32(DimDict['nlocs'])

    # Set coordinates and units of the ObsValues.
    n = 0
    for iodavar in obsvars.values():
        varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, iconv.OvalName()]['units'] = obsvars_units[n]
        varAttrs[iodavar, iconv.OerrName()]['units'] = obsvars_units[n]
        varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'
        n += 1

    # Set units of the MetaData variables and all _FillValues.
    for key in meta_keys:
        varAttrs[(key, "MetaData")]['units'] = locationKeyList[meta_keys.index(key)][2]
        if (locationKeyList[meta_keys.index(key)][1] == "string"):
            VarAttrs[(key, "MetaData")]['_FillValue'] = string_missing_value
        elif (locationKeyList[meta_keys.index(key)][1] == "integer"):
            VarAttrs[(key, "MetaData")]['_FillValue'] = int_missing_value
        elif (locationKeyList[meta_keys.index(key)][1] == "long"):
            VarAttrs[(key, "MetaData")]['_FillValue'] = long_missing_value
        elif (locationKeyList[meta_keys.index(key)][1] == "float"):
            VarAttrs[(key, "MetaData")]['_FillValue'] = float_missing_value

    print("INFO: Writing file: ", output_file)

    # setup the IODA writer
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(obs_data, VarDims, varAttrs, AttrData)

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


def read_bufr_message(f, count, start_pos):

    meta_data = {}
    obs_data = {}
    call_fail = False
    if start_pos == f.tell():
        return obs_data, count, None
    start_pos = f.tell()
    # print ( "starting pos: ", f.tell() )

    met_utils = meteo_utils.meteo_utils()

    count[0] += 1

    try:
        bufr = codes_bufr_new_from_file(f)

        codes_set(bufr, 'unpack', 1)

        # First, get the MetaData we are interested in (list is in metaDataKeyList)
        for k, v in metaDataKeyList.items():
            if (v.len() > 1):
                for var in v:
                    if (var != 'Constructed'):
                        meta_data[k] = codes_get(bufr, v)
                        if (meta_data[k] != bufr_missing_float):
                            break
            else:
                if (v[0] != 'Constructed'):
                    meta_data[k] = codes_get(bufr, v[0])

        # Replace BUFR missing value indicators with IODA missings.
        for k, v in metaDataKeyList.items():
            if (meta_data[k] == bufr_missing_float and locationKeyList[meta_keys.index(k)][1] == "string"):
                meta_data[k] = string_missing_value
            elif (meta_data[k] == bufr_missing_float and locationKeyList[meta_keys.index(k)][1] == "integer"):
                meta_data[k] = int_missing_value
            elif (meta_data[k] == bufr_missing_float and locationKeyList[meta_keys.index(k)][1] == "long"):
                meta_data[k] = long_missing_value
            elif (meta_data[k] == bufr_missing_float and locationKeyList[meta_keys.index(k)][1] == "float"):
                meta_data[k] = float_missing_value

        # Plus, to construct a dateTIme, we always need its components.
        year = codes_get(bufr, 'year')
        month = codes_get(bufr, 'month')
        day = codes_get(bufr, 'day')
        hour = codes_get(bufr, 'hour')
        minute = codes_get(bufr, 'minute')
        second = codes_get(bufr, 'second')    # non-integer value, optional
        if (second >= 0 and second <= 59):
            second = round(second)
        else:
            second = 0
        meta_data['dateTime'] = long_missing_value
        if (year>1900 and year<2199 and month>=1 and month<=12 and day>=1 and day<=31
            and hour>=0 and hour<=23 and minute>=0 and minute<=59 and second>=0 and second<=59):
            this_datetime = datetime(year, month, day, hour, minute, second)
            time_offset = (this_datetime - epoch).total_seconds()
            meta_data['dateTime'] = time_offset.astype('int64')
        else:
            count[1] += 1

        # Always need lat, lon or else the observation is useless, so let us double-check.
        meta_data['latitude'] = float_missing_value
        meta_data['longitude'] = float_missing_value
        if (meta_data['latitude'] >= -90 and meta_data['latitude'] <= 90
            and meta_data['longitude'] >= -180 and meta_data['longitude'] <= 360):
            if (meta_data['longitude'] > 180):
                meta_data['longitude'] = 360.0 - meta_data['longitude']
        else:
            count[1] += 1

        # Next, get the raw observed weather variables we want.
        for variable in raw_obsvars:           #  ['airTemperature', 'mixingRatio', 'windDirection', 'windSpeed']
            vals[variable] = codes_get(bufr, variable)
            if (vals[variable] == bufr_missing_float):
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

        # Move everything into the final obs_data dictionary, including metadata.
        obs_data['eastward_wind'] = uwnd
        obs_data['northward_wind'] = vwnd
        obs_data['specific_humidity'] = spfh
        obs_data['air_temperature'] = vals['airTemperature']
        for key in meta_keys:
            obs_data[k] = meta_data[k]

        # print ( "ending pos: ", f.tell() )
        count[0] += 1

        return obs_data, count, start_pos

    except BaseException:
        # print ( "invalid bufr message" )
        if call_fail:
            sys.exit()
        count[1] += 1
        print ( "number of total msgs: ", count[0] )
        print ( "number of invalid or useless msgs: ", count[1] )
        return obs_data, count, start_pos


if __name__ == "__main__":

    from argparse import ArgumentParser

    parser = ArgumentParser(
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
    args = parser.parse_args()

    for file_name in args.file_names:
        if not os.path.isfile(file_name):
            parser.error('Input (-i option) file: ', file_name, ' does not exist')

    main(args.file_names, args.output_file)
