#!/usr/bin/env python3

#
# (C) Copyright 2019-2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

#
# decode bespoke ground-based GNSS Zenith Total Delay (ZTD)
#

import sys
import os
import time
from datetime import datetime, timezone
import numpy as np
import logging

# These modules need the path to lib-python modules
import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

os.environ["TZ"] = "UTC"

# these are the unique values in the raw input file
varDict = {
    'zenithTotalDelay': ['zenithTotalDelay', "float", 'm'],
}
# extend the variable keys including 'Error', and 'Flag'
extended_varDict_keys = set()
for key in varDict.keys():
    extended_varDict_keys.update([key, key + 'Error', key + 'Flag'])

# these are the MetaData common to each input
locationKeyList = [
    ('latitude', 'float', 'degree_north'),
    ('longitude', 'float', 'degree_east'),
    ('dateTime', 'long', iso8601_string),
    ('stationIdentification', 'string', ''),#, 'GNSS ground-based receiving station name'),
    ('stationElevation', 'float', 'm'),
]

meta_keys = [m_item[0] for m_item in locationKeyList]

GlobalAttrs = {
    'converter': os.path.basename(__file__),
    'ioda_version': 3,
    'description': 'GNSS Ground-based Zenith Total Delay',
    'source': 'GNSS Network',
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
    for fname in file_names:
        logging.info(f"Reading file:  {fname}")
        file_data = read_file(fname)
        # if there is data do something
        if file_data:
            # first successful read
            if not data:
                data = file_data
            # subsequent successful read
            else:
                for key in extended_varDict_keys.union(meta_keys):
                    data[key] = np.append(data[key], file_data[key])

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
        if 'zenithTotalDelay' in key:
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
        varAttrs[(variable, qcName)]['units'] = units
        varAttrs[(variable, obsValName)]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[(variable, obsErrName)]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[(variable, qcName)]['coordinates'] = 'longitude latitude stationElevation'
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
        if 'zenithTotalDelay' not in key:
            logging.info(f" the variable: {variable} will be placed into MetaData of ioda_data")
            # these MetaData are arrays nlocs long already
            ioda_data[(key, metaDataName)] = np.array(data[variable], dtype=dtypes[dtype])
        else:
            variable = varDict[key][0]
            logging.info(f" the variable: {variable} will be placed into ObsValue of ioda_data")
            ioda_data[(variable, obsValName)] = np.array(data[variable], dtype=np.float32)
            # observation error is defined here
            ioda_data[(variable, obsErrName)] = np.full(len(ioda_data[(variable, obsValName)]), 0.02, dtype=np.float32)

    logging.debug("Writing file: " + output_file)

    # setup the IODA writer and write everything out.
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)
#   import pdb
#   pdb.set_trace()
#   import sys
#   sys.exit()
    writer.BuildIoda(ioda_data, varDims, varAttrs, GlobalAttrs)

    logging.info("--- {:9.4g} total seconds ---".format(time.time() - start_time))


def read_file(file_name):

    local_data = init_data_dict()

    # Open the file
    with open(file_name, 'r') as file:
        # Create an iterator from the file object
        file_iterator = iter(file)

        while True:
            try:

                local_data, header_read = get_header(file_iterator, local_data)
                logging.debug(f'header was read w/o error: {header_read}')

                while header_read:
                    # Get the next line from the iterator
                    line = next(file_iterator)
                    local_data = populate_obsValue(line, local_data, file_name)

            except StopIteration:
                # If StopIteration is raised, break from the loop
                break

    # repeat all the metaData values
    nlocs = len(local_data['dateTime'])
    if nlocs == 0:
        # if the header was not sucessfully read return nothing
        local_data = None
    return local_data


def get_header(file_iterator, local_data):
    #####################################################
    # get header (2 lines)
    #####################################################

    # Line #1: HIUS1 KJPL 080213
    # SID       LON       LAT       ALT       ZTD gradientE gradientN       ZDD       ZWD        PW

    header_read = False
    # read first line
    line = next(file_iterator)

    # read second line
    line = next(file_iterator)

    header_read = True

    return local_data, header_read


def populate_obsValue(line, local_data, fname):

    # get the zenith totaly delay retrieved from GNSS transmitter
    # if can correctly parse all fields populate local_data otherwise do nothing

    # ObsValue data row (example)
    # SID  YYYYMMDD  HHMMSS       LON       LAT       ALT       ZTD gradientE gradientN       ZDD       ZWD        PW
    #SSSS     (UTC)   (UTC)     (deg)     (deg)       (m)       (m)        ()        ()       (m)       (m)       (m)

    # read data lines beginning at third line
    try:
        sid, yyyymmdd, hhmmss, lon, lat, alt, ztd, gradiente, gradientn, zdd, zwd, pw = line.split()
    except ValueError:
        local_data = fill_data_with_missing(local_data)
        return local_data

    # get date and time from filename (example)
    # ZTD_2022-06-24_02:00:00
    #_, yymmdd, hhmmss = fname.split('_')
    #yymmdd = yymmdd.replace('-', '')[2:]
    #hhmmss = hhmmss.replace(':', '')
    yymmdd = yyyymmdd[2:]
    dateTime = convert_string_to_dateTime(yymmdd, hhmmss)

    local_data['dateTime'] = np.append(local_data['dateTime'], dateTime)
    local_data['stationIdentification'] = np.append(local_data['stationIdentification'], sid)
    local_data['stationElevation'] = np.append(local_data['stationElevation'], float(alt))
    local_data['latitude'] = np.append(local_data['latitude'], float(lat))
    local_data['longitude'] = np.append(local_data['longitude'], float(lon))
    local_data['zenithTotalDelay'] = np.append(local_data['zenithTotalDelay'], float(ztd))

    return local_data


def convert_string_to_dateTime(yymmdd, hhmmss):

    # convert strings of time into python dateTime object
    try:
        # Combine and parse the string as a datetime object
        dtg = datetime.strptime(f"{yymmdd}{hhmmss}", '%y%m%d%H%M%S')
        # Set timezone to UTC
        dtg = dtg.replace(tzinfo=timezone.utc)
        # Convert to Unix time in seconds since epoch
        dateTime = np.int64(round((dtg - epoch).total_seconds()))

        return dateTime

    except ValueError as e:
        # Raise an informative error if parsing fails
        raise ValueError(f"Invalid date or time format in inputs '{yymmdd}' and '{hhmmss}': {e}")


def fill_data_with_missing(local_data):
    # fill the data records from TENET line 4 with missing
    local_data['dateTime'] = np.append(local_data['dateTime'], int_missing_value)
    local_data['stationIdentification'] = np.append(local_data['stationIdentification'], string_missing_value)
    local_data['stationElevation'] = np.append(local_data['stationElevation'], float_missing_value)
    local_data['latitude'] = np.append(local_data['latitude'], float_missing_value)
    local_data['longitude'] = np.append(local_data['longitude'], float_missing_value)
    local_data['zenithTotalDelay'] = np.append(local_data['zenithTotalDelay'], float_missing_value)
    return local_data


def init_data_dict():
    local_data = {}              # Before assigning the output types into the above.
    for key in extended_varDict_keys.union(meta_keys):
        local_data[key] = []
    return local_data


if __name__ == "__main__":

    from argparse import ArgumentParser

    parser = ArgumentParser(
        description=('Read a satwind AMV ascii/csv file from SSEC'
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
