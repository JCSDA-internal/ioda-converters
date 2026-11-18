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
    ('satelliteTransmitterId', 'integer', 'GNSS transmitter ID constellation pseudoRandomNoise PRN code'),
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
    ('stationIdentifierWMO', 'integer', 'WMO assigned number for the site'),
    ('stationIdentifier', 'string', 'GNSS ground-based receiving station name'),
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
            ioda_data[(variable, qcName)] = np.array(data[variable+'Flag'], dtype=np.int32)

    logging.debug("Writing file: " + output_file)

    # setup the IODA writer and write everything out.
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)
    writer.BuildIoda(ioda_data, varDims, varAttrs, GlobalAttrs)

    logging.info("--- {:9.4g} total seconds ---".format(time.time() - start_time))


def read_file(file_name, any_data):

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
                    local_data, endReport = populate_obsValue(line, local_data)
                    if endReport:
                        header_read = False
                        break

            except StopIteration:
                # If StopIteration is raised, break from the loop
                break

    any_data = True
    # repeat all the metaData values
    nlocs = len(local_data['dateTime'])
    if nlocs == 0:
        # if the header was not sucessfully read return nothing
        local_data = None
    return local_data, any_data


def get_header(file_iterator, local_data):
    #####################################################
    # get header (3 lines)
    #####################################################

    # Line #1: HIUS1 KJPL 080213
    # This line is standard in all TENET files the only thing that changes is the numeric value 080213
    # This value indicates the Coordinated Universal Time (UTC) JPL produces the TENET file
    # The format is: DDHHMM (where DD = day, HH = UTC hour, and MM = minutes)

    # Line #2: TENET
    # This line is standard in all TENET files and never changes. It indicates the nature of the file – TENET

    # Line #3: 23074 0694/ 271608 NRIL 000064539804 002253881156 005946178085
    # WMO number, latitude (N), longitude (W), station name, ECEF station coordinates

    header_read = False
    # read first line
    line = next(file_iterator)
    try:
        _, _, report_time = line.split()
    except ValueError:
        return local_data, header_read

    # read second line
    line = next(file_iterator)
    if 'TENET' not in line:
        return local_data, header_read     # not necessarily needed could try to continue

    # read third line
    line = next(file_iterator)
    try:
        WMOid, lat, lon, stationName, xECEFPosition, yECEFPosition, zECEFPosition = line.split()
    except ValueError:
        return local_data, header_read

    xECEFPosition = convert_ECEF_string(xECEFPosition)
    yECEFPosition = convert_ECEF_string(yECEFPosition)
    zECEFPosition = convert_ECEF_string(zECEFPosition)

    lat = parse_latitude(lat)

    try:
        local_data['latitude'] = np.append(local_data['latitude'], float(lat))
        local_data['longitude'] = np.append(local_data['longitude'], float(lon)/1000.)
        local_data['stationIdentifier'] = np.append(local_data['stationIdentifier'], stationName)
        local_data['stationIdentifierWMO'] = np.append(local_data['stationIdentifierWMO'], int(WMOid))
        local_data['xECEFPosition'] = np.append(local_data['xECEFPosition'], xECEFPosition)
        local_data['yECEFPosition'] = np.append(local_data['yECEFPosition'], yECEFPosition)
        local_data['zECEFPosition'] = np.append(local_data['zECEFPosition'], zECEFPosition)
    except ValueError:
        return local_data, header_read

    header_read = True

    return local_data, header_read


def populate_obsValue(line, local_data):

    # get the electron content retrieved from GNSS transmitter
    # if can correctly parse all fields populate local_data otherwise do nothing

    # ObsValue data row (example)
    # Line #4: 11111 180208 021000 280771 26911 2165166150 4210322950 0233/ 3558/ 102029725204 114864753823 022550194624
    # unknown, YYMMDD, HHMMSS, PRN/LatitudeIPP, LongitudeIPP, VOBS, SOBS, satelite elevation angle, azimuth angle, GNSS ECEF coordinates

    # for all ECEF coordinates 0 in front indicates positive, and 1 indicates negative.
    # IPP = Ionospheric Pierce Point

    endReport = False
    if '99999' in line[0:5]:
        # reset for next record
        endReport = True
        return local_data, endReport

    # read data lines beginning at fourth line
    try:
        _, yymmdd, hhmmss, PRNlatitudeIPP, longitudeIPP, vobs, sobs, elevationAngle, azimuthAngle, \
            xECEFPositionGNSS, yECEFPositionGNSS, zECEFPositionGNSS = line.split()
    except ValueError:
        local_data = fill_data_with_missing(local_data)
        return local_data, endReport

    dateTime = convert_string_to_dateTime(yymmdd, hhmmss)
    PRN, latitudeIPP = parse_station_and_latitude(PRNlatitudeIPP)

    xECEFPositionGNSS = convert_ECEF_string(xECEFPositionGNSS)
    yECEFPositionGNSS = convert_ECEF_string(yECEFPositionGNSS)
    zECEFPositionGNSS = convert_ECEF_string(zECEFPositionGNSS)

    # the slant TEC needs to be 10 digits for the reader to parse correctly
    if len(sobs.lstrip('/')) != 10:
        local_data = fill_data_with_missing(local_data)
        return local_data, endReport

    try:
        tec_value, tec_error, tec_flag = tenet_10digit_reader(sobs.lstrip('/'))
        elevationAngleGNSS = float(elevationAngle.rstrip('/'))/10.
        sensorAzimuthAngle = float(azimuthAngle.rstrip('/'))/10.
    except ValueError:
        local_data = fill_data_with_missing(local_data)
        return local_data, endReport

    local_data['dateTime'] = np.append(local_data['dateTime'], dateTime)
    local_data['satelliteTransmitterId'] = np.append(local_data['satelliteTransmitterId'], PRN)
    local_data['latitudeIPP'] = np.append(local_data['latitudeIPP'], latitudeIPP)
    local_data['longitudeIPP'] = np.append(local_data['longitudeIPP'], float(longitudeIPP)/100.)
    local_data['elevationAngleGNSS'] = np.append(local_data['elevationAngleGNSS'], elevationAngleGNSS)
    local_data['sensorAzimuthAngle'] = np.append(local_data['sensorAzimuthAngle'], sensorAzimuthAngle)
    local_data['totalElectronContent'] = np.append(local_data['totalElectronContent'], tec_value)
    local_data['totalElectronContentError'] = np.append(local_data['totalElectronContentError'], tec_error)
    local_data['totalElectronContentFlag'] = np.append(local_data['totalElectronContentFlag'], tec_flag)
    local_data['xECEFPositionGNSS'] = np.append(local_data['xECEFPositionGNSS'], xECEFPositionGNSS)
    local_data['yECEFPositionGNSS'] = np.append(local_data['yECEFPositionGNSS'], yECEFPositionGNSS)
    local_data['zECEFPositionGNSS'] = np.append(local_data['zECEFPositionGNSS'], zECEFPositionGNSS)

    # repeat the metaData
    if len(local_data['latitude']) < len(local_data['latitudeIPP']):
        for key in ['latitude', 'longitude', 'stationIdentifier', 'stationIdentifierWMO', 'xECEFPosition', 'yECEFPosition', 'zECEFPosition']:
            local_data[key] = np.append(local_data[key], local_data[key][-1])

    return local_data, endReport


def tenet_10digit_reader(int_10digit_number):

    """Read TENET data files.

    Parameters
    ----------
    int_10digit_number : string
        10 digit integer number from .tec files.

    Returns
    -------

    tec : flt
        TEC value.
    error : flt
        Uncertainty of TEC value.
    flag : int
        Integer flag.

    Notes

    -----
    This function converts tenet 10 digit number into:
       tec, error, and quality flag
    Source: "SWAFS TENET File Data Definition", dated 24 August, 2001
    """

    # extract exponential and convert into TEC Units (TEC): 1 TECU = 10^16 electrons m-3
    exponential = np.power(10, int(10 + int(int_10digit_number[8: 9]))) / 1e16

    flag = int(int_10digit_number[9: 10])
    tec = float(int_10digit_number[0: 4]) / 100. * exponential
    error = float(int_10digit_number[4: 8]) / 100. * exponential

    return tec, error, flag


def convert_ECEF_string(c):
    try:
        # Attempt the conversion logic

        # Per the TENET format documentation:
        # The station geographic coordinates are provided in units of meters in the Earth Centered Earth Fixed (ECEF) coordinate system.
        # The format for these coordinates is: XXXXXXXXXXXX,
        # where the first number X represents the sign of the value and is either positive (0) or negative (1).
        # The decimal point is located between positions 6 and 7.
        dec = f'{c[0:6]}.{c[6:]}'
        result = -float(dec[1:]) if dec[0] == '1' else float(dec[1:]) if dec[0] == '0' else float_missing_value
        return result
    except (IndexError, ValueError) as e:
        # Raise an exception with a descriptive message
        raise ValueError(f"Invalid input string '{c}': {e}")


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


def parse_station_and_latitude(code):
    try:
        # Extract the station code (first two digits)
        station_code = int(code[:2])

        # Determine latitude sign based on the third digit
        latitude_sign = 1 if code[2] == '0' else -1 if code[2] == '1' else None
        if latitude_sign is None:
            raise ValueError(f"Invalid latitude sign indicator '{code[2]}'; expected '0' for positive or '1' for negative.")

        # Extract the latitude (remaining digits) and apply sign and scale
        latitude = latitude_sign * int(code[3:]) / 10.0

        return station_code, latitude

    except ValueError as e:
        raise ValueError(f"Invalid input '{code}': {e}")


def parse_latitude(lat_str):
    try:
        # Determine the latitude sign based on the first digit
        latitude_sign = 1 if lat_str[0] == '0' else -1 if lat_str[0] == '1' else None
        if latitude_sign is None:
            raise ValueError(f"Invalid latitude sign indicator '{lat_str[0]}'; expected '0' for positive or '1' for negative.")

        # Convert the remaining digits to latitude, apply the sign, and divide by 10
        latitude = latitude_sign * float(lat_str[1:-1]) / 10.0

        return latitude

    except ValueError as e:
        raise ValueError(f"Invalid latitude input '{lat_str}': {e}")


def fill_data_with_missing(local_data):
    # fill the data records from TENET line 4 with missing
    local_data['dateTime'] = np.append(local_data['dateTime'], int_missing_value)
    local_data['satelliteTransmitterId'] = np.append(local_data['satelliteTransmitterId'], int_missing_value)
    local_data['latitudeIPP'] = np.append(local_data['latitudeIPP'], float_missing_value)
    local_data['longitudeIPP'] = np.append(local_data['longitudeIPP'], float_missing_value)
    local_data['elevationAngleGNSS'] = np.append(local_data['elevationAngleGNSS'], float_missing_value)
    local_data['sensorAzimuthAngle'] = np.append(local_data['sensorAzimuthAngle'], float_missing_value)
    local_data['totalElectronContent'] = np.append(local_data['totalElectronContent'], float_missing_value)
    local_data['totalElectronContentError'] = np.append(local_data['totalElectronContentError'], float_missing_value)
    local_data['totalElectronContentFlag'] = np.append(local_data['totalElectronContentFlag'], int_missing_value)
    local_data['xECEFPositionGNSS'] = np.append(local_data['xECEFPositionGNSS'], float_missing_value)
    local_data['yECEFPositionGNSS'] = np.append(local_data['yECEFPositionGNSS'], float_missing_value)
    local_data['zECEFPositionGNSS'] = np.append(local_data['zECEFPositionGNSS'], float_missing_value)
    if len(local_data['latitude']) < len(local_data['latitudeIPP']):
        for key in ['latitude', 'longitude', 'stationIdentifier', 'stationIdentifierWMO', 'xECEFPosition', 'yECEFPosition', 'zECEFPosition']:
            local_data[key] = np.append(local_data[key], local_data[key][-1])
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
