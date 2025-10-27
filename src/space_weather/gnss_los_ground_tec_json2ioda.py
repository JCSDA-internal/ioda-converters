#!/usr/bin/env python3

#
# (C) Copyright 2019-2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

#
# decode TENET formatted ground-based GNSS Total Electron Content (TEC)
#

import sys
import os
import time
from datetime import datetime, timezone
import numpy as np
import logging
import json

# These modules need the path to lib-python modules
import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

os.environ["TZ"] = "UTC"

# these are the unique values in the raw input file
varDict = {
    'totalElectronContent': ['totalElectronContent', "float", 'TECU'],
}
# extend the variable keys including 'Error', and 'Flag'
extended_varDict_keys = set()
for key in varDict.keys():
    extended_varDict_keys.update([key, key + 'Error', key + 'Flag'])

# these are the MetaData common to each input
locationKeyList = [
    ('latitude', 'float', 'degrees_north'),
    ('longitude', 'float', 'degrees_east'),
    ('elevationAngleGNSS', 'float', 'GNSS transmitter satellite elevation angle in degrees'),
    ('sensorAzimuthAngle', 'float', 'aziumuth angle viewing GNSS transmitter in degrees west'),
    ('xECEFPosition', 'float', 'receiving station Earth Centered Earth Fixed X-coordinate in meters'),
    ('yECEFPosition', 'float', 'receiving station Earth Centered Earth Fixed Y-coordinate in meters'),
    ('zECEFPosition', 'float', 'receiving station Earth Centered Earth Fixed Z-coordinate in meters'),
    ('xECEFPositionGNSS', 'float', 'GNSS transmitting satellite Earth Centered Earth Fixed X-coordinate in meters'),
    ('yECEFPositionGNSS', 'float', 'GNSS transmitting satellite Earth Centered Earth Fixed Y-coordinate in meters'),
    ('zECEFPositionGNSS', 'float', 'GNSS transmitting satellite Earth Centered Earth Fixed Z-coordinate in meters'),
    ('latitudeIPP', 'float', 'latitude of Ionospheric Pierce Point in degrees_north'),
    ('longitudeIPP', 'float', 'longitude of Ionospheric Pierce Point in degrees_east'),
    ('dateTime', 'long', iso8601_string),
]

meta_keys = [m_item[0] for m_item in locationKeyList]

GlobalAttrs = {
    'converter': os.path.basename(__file__),
    'ioda_version': 3,
    'description': 'Ground-based Total Electron Content',
    'source': 'TENET Network',
}

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)
string_missing_value = '_'

missing_vals = {'string': string_missing_value,
                'integer': int_missing_value,
                'long': long_missing_value,
                'float': float_missing_value}
dtypes = {'string': object,
          'integer': np.int32,
          'long': np.int64,
          'float': np.float32}


def main(args):

    file_names = args.input
    output_file = args.output

    start_time = time.time()

    data = None
    any_data = False
    for fname in file_names:
        logging.info(f"Reading file:  {fname}")
        file_data, any_data = read_file(fname, any_data)
        # if there is data do something
        if file_data:
            # first successful read
            if not data:
                data = file_data
            # subsequent successful read
            else:
                for key in extended_varDict_keys.union(meta_keys):
                    # data[key] = np.concatenate(data[key], file_data[key]) # why not and gotta be much better way
                    data[key] = np.append(data[key], file_data[key])

    # if all files have no data
    if not any_data:
        logging.error("No data to write, stopping execution.")
        sys.exit()
    dtg = datetime.fromtimestamp(data['dateTime'][0])
    datetimeRef = dtg.isoformat() + "Z"

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs = {
        'sourceFiles': ", ".join(file_names),
        'datetimeReference': datetimeRef
    }

    nlocs = len(data['dateTime'])
    logging.info(f" found a total of {nlocs} observations")
    DimDict = {'Location': nlocs}

    varDims = {}
    for key in varDict.keys():
        variable = varDict[key][0]
        varDims[variable] = ['Location']

    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    # Set units of the MetaData variables and all _FillValues.
    for key in meta_keys:
        dtype = locationKeyList[meta_keys.index(key)][1]
        if locationKeyList[meta_keys.index(key)][2]:
            varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtype]
    for key in varDict.keys():
        if 'totalElectronContent' in key:
            continue
        dtype = varDict[key][1]
        units = varDict[key][2]
        if units:
            varAttrs[(key, metaDataName)]['units'] = units
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtype]

    # Set units and FillValue attributes for groups associated with observed variable.
    for key in varDict.keys():
        variable = varDict[key][0]
        dtype = varDict[key][1]
        units = varDict[key][2]
        varAttrs[(variable, obsValName)]['units'] = units
        varAttrs[(variable, obsErrName)]['units'] = units
        varAttrs[(variable, obsValName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, obsErrName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, qcName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, obsValName)]['_FillValue'] = missing_vals[dtype]
        varAttrs[(variable, obsErrName)]['_FillValue'] = missing_vals[dtype]
        varAttrs[(variable, qcName)]['_FillValue'] = int_missing_value

    # Fill the final IODA data:  MetaData then ObsValues, ObsErrors, and QC
    ioda_data = {}

#   should populate ioda_data directly rather than creating another copy
    for key in meta_keys:
        dtype = locationKeyList[meta_keys.index(key)][1]
        ioda_data[(key, metaDataName)] = np.array(data[key], dtype=dtypes[dtype])
    for key in varDict.keys():
        variable = varDict[key][0]
        dtype = varDict[key][1]
        if 'totalElectronContent' not in key:
            logging.info(f" the variable: {variable} will be placed into MetaData of ioda_data")
            # these MetaData are arrays nlocs long already
            ioda_data[(key, metaDataName)] = np.array(data[variable], dtype=dtypes[dtype])
        else:
            variable = varDict[key][0]
            logging.info(f" the variable: {variable} will be placed into ObsValue of ioda_data")
            ioda_data[(variable, obsValName)] = np.array(data[variable], dtype=np.float32)
            ioda_data[(variable, obsErrName)] = np.array(data[variable+'Error'], dtype=np.float32)

    logging.debug("Writing file: " + output_file)

    # setup the IODA writer and write everything out.
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)
    writer.BuildIoda(ioda_data, varDims, varAttrs, GlobalAttrs)

    logging.info("--- {:9.4g} total seconds ---".format(time.time() - start_time))


def read_file(file_name, any_data):
    print(file_name)
    local_data = init_data_dict()

    # Open the file
    with open(file_name, 'r') as file:
        data = json.load(file)
        for line in data:
            local_data = populate_obsValue(line, local_data)

    any_data = True
    # repeat all the metaData values
    nlocs = len(local_data['dateTime'])
    if nlocs == 0:
        # if the header was not sucessfully read return nothing
        local_data = None
    return local_data, any_data


def populate_obsValue(line, local_data):

    dateTime = convert_string_to_dateTime(line['obTime'])
    if 'GPS' in line['obSetId']:
        PRN = 401
    else:
        PRN = 402
    latitudeIPP = line['seoList'][4]['obArray'][0]
    longitudeIPP = line['seoList'][4]['obArray'][1]

    xECEFPositionGNSS = line['seoList'][5]['obArray'][0]
    yECEFPositionGNSS = line['seoList'][5]['obArray'][1]
    zECEFPositionGNSS = line['seoList'][5]['obArray'][2]

    tec_value = line['seoList'][0]['obValue']
    tec_error = line['seoList'][1]['obValue']
    elevationAngleGNSS = line['seoList'][2]['obValue']
    sensorAzimuthAngle = line['seoList'][3]['obValue']

    local_data['dateTime'] = np.append(local_data['dateTime'], dateTime)
    local_data['latitudeIPP'] = np.append(local_data['latitudeIPP'], latitudeIPP)
    local_data['longitudeIPP'] = np.append(local_data['longitudeIPP'], longitudeIPP)
    local_data['elevationAngleGNSS'] = np.append(local_data['elevationAngleGNSS'], elevationAngleGNSS)
    local_data['sensorAzimuthAngle'] = np.append(local_data['sensorAzimuthAngle'], sensorAzimuthAngle)
    local_data['totalElectronContent'] = np.append(local_data['totalElectronContent'], tec_value)
    local_data['totalElectronContentError'] = np.append(local_data['totalElectronContentError'], tec_error)
    local_data['xECEFPositionGNSS'] = np.append(local_data['xECEFPositionGNSS'], xECEFPositionGNSS)
    local_data['yECEFPositionGNSS'] = np.append(local_data['yECEFPositionGNSS'], yECEFPositionGNSS)
    local_data['zECEFPositionGNSS'] = np.append(local_data['zECEFPositionGNSS'], zECEFPositionGNSS)

    local_data['latitude'] = np.append(local_data['latitude'], line['lat'])
    local_data['longitude'] = np.append(local_data['longitude'], line['lon'])
    local_data['xECEFPosition'] = np.append(local_data['xECEFPosition'], line['senPos'][0])
    local_data['yECEFPosition'] = np.append(local_data['yECEFPosition'], line['senPos'][1])
    local_data['zECEFPosition'] = np.append(local_data['zECEFPosition'], line['senPos'][2])
    return local_data


def convert_string_to_dateTime(time):

    # convert strings of time into python dateTime object
    try:
        # Combine and parse the string as a datetime object
        dtg = datetime.strptime(time, '%Y-%m-%dT%H:%M:%S.000000')
        # Set timezone to UTC
        dtg = dtg.replace(tzinfo=timezone.utc)
        # Convert to Unix time in seconds since epoch
        dateTime = np.int64(round((dtg - epoch).total_seconds()))

        return dateTime

    except ValueError as e:
        # Raise an informative error if parsing fails
        raise ValueError(f"Invalid date or time format in input '{time}': {e}")


def init_data_dict():
    local_data = {}              # Before assigning the output types into the above.
    for key in extended_varDict_keys.union(meta_keys):
        local_data[key] = []
    return local_data


if __name__ == "__main__":

    from argparse import ArgumentParser

    parser = ArgumentParser(
        description=('Read a TENET formated Json file containing line of sight TEC'
                     ' and convert into IODA output file')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument('-i', '--input', nargs='+',
                          action='store', default=None, required=True,
                          help='input files')
    required.add_argument('-o', '--output',
                          action='store', default=None, required=True,
                          help='output file')

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--debug', action='store_true', default=False,
                          help='enable debug messages')
    optional.add_argument('--verbose', action='store_true', default=False,
                          help='enable verbose debug messages')

    args = parser.parse_args()

    if args.debug:
        logging.basicConfig(level=logging.INFO)
    elif args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.ERROR)

    for file_name in args.input:
        if not os.path.isfile(file_name):
            parser.error('Input (-i option) file: ', file_name, ' does not exist')

    main(args)
