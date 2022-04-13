#!/usr/bin/env python3

import sys
import os
from pathlib import Path
import time
from datetime import datetime
import csv
import numpy as np
import netCDF4 as nc
import logging

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

varDict = {'eastward_wind': ['eastward_wind', 'm s-1'],
           'northward_wind': ['northward_wind', 'm s-1']}

locationKeyList = [("latitude", "float", "degrees_north"),
                   ("longitude", "float", "degrees_east"),
                   ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z"),
                   ("air_pressure", "float", "Pa"),
                   ("sensorCentralFrequency", "float", "Hz"),
                   ("sensorZenithAngle", "float", "degrees"),
                   ("windTrackingCorrelation", "float", "1"),
                   ("windHeightAssignMethod", "integer", ""),
                   ("satelliteID", "integer", "")]

meta_keys = [m_item[0] for m_item in locationKeyList]

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
double_missing_value = nc.default_fillvals['f8']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'

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


def main(file_names, output_file, datetimeRef):

    start_time = time.time()

    ioda_data = {}         # The final outputs.
    data = {}              # Before assigning the output types into the above.
    for key in varDict.keys():
        data[key] = []
    for key in meta_keys:
        data[key] = []

    for fname in file_names:
        logging.info(f"Reading file:  {fname}")
        data = read_file(fname, data)

    if not data:
        logging.error("No data to write, stopping execution.")
        sys.exit()

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs = {
        'soureFiles': ", ".join(file_names),
        'datetimeReference': datetimeRef
    }

    nlocs = len(data['dateTime'])
    logging.info(f" found a total of {nlocs} observations")
    DimDict = {'nlocs': nlocs}

    varDims = {}
    for key in varDict.keys():
        variable = varDict[key][0]
        varDims[variable] = ['nlocs']

    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    # Set units of the MetaData variables and all _FillValues.
    for key in meta_keys:
        dtypestr = locationKeyList[meta_keys.index(key)][1]
        if locationKeyList[meta_keys.index(key)][2]:
            varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]

    # Set units and FillValue attributes for groups associated with observed variable.
    for key in varDict.keys():
        variable = varDict[key][0]
        units = varDict[key][1]
        varAttrs[(variable, obsValName)]['units'] = units
        varAttrs[(variable, obsErrName)]['units'] = units
        varAttrs[(variable, obsValName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, obsErrName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, qcName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, obsValName)]['_FillValue'] = float_missing_value
        varAttrs[(variable, obsErrName)]['_FillValue'] = float_missing_value
        varAttrs[(variable, qcName)]['_FillValue'] = int_missing_value

    # Fill the final IODA data:  MetaData then ObsValues, ObsErrors, and QC
    for key in meta_keys:
        dtypestr = locationKeyList[meta_keys.index(key)][1]
        ioda_data[(key, metaDataName)] = np.array(data[key], dtype=dtypes[dtypestr])

    for key in varDict.keys():
        variable = varDict[key][0]
        logging.info(f" the variable: {variable} will be placed into ioda_data")
        ioda_data[(variable, obsValName)] = np.array(data[variable], dtype=np.float32)
        ioda_data[(variable, obsErrName)] = np.full(nlocs, 3.0, dtype=np.float32)
        ioda_data[(variable, qcName)] = np.full(nlocs, 2, dtype=np.int32)

    logging.debug("Writing file: " + output_file)

    # setup the IODA writer and write everything out.
    writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)
    writer.BuildIoda(ioda_data, varDims, varAttrs, GlobalAttrs)

    logging.info("--- {:9.4f} total seconds ---".format(time.time() - start_time))


def read_file(file_name, data):

    with open(file_name, newline='') as f:
        reader = csv.DictReader(f, skipinitialspace=True, delimiter=' ')
        keyerr = False
        freqerr = False
        saterr = False

        for row in reader:
            try:
                year = int(row['day'][0:4])
                month = int(row['day'][5:6])
                day = int(row['day'][7:8])
                hour = int(row['hms'][0:1])
                minute = int(row['hms'][2:3])
                second = 0
                dtg = datetime(year, month, day, hour, minute, second)
                time_offset = np.int64(round((dtg - epoch).total_seconds()))
                data['dateTime'] = np.append(data['dateTime'], time_offset)
                data['longitude'] = np.append(data['longitude'], float(row['lon']))
                data['latitude'] = np.append(data['latitude'], float(row['lat']))

                freq, freqerr = get_frequency(row['type'], freqerr)
                satid, saterr = get_id(row['sat'], saterr)
                data['satelliteID'] = np.append(data['satelliteID'], satid)
                data['sensorCentralFrequency'] = np.append(data['sensorCentralFrequency'], freq)
                data['sensorZenithAngle'] = np.append(data['sensorZenithAngle'], float(row['rff']))
                data['windTrackingCorrelation'] = np.append(data['windTrackingCorrelation'], float(row['qi']))
                data['windHeightAssignMethod'] = np.append(data['windHeightAssignMethod'], int(row['int']))

                pres = float(row['pre'])*100.
                data['air_pressure'] = np.append(data['air_pressure'], pres)
                wdir = float(row['dir'])*1.0
                wspd = float(row['spd'])*1.0
                if (wdir >= 0 and wdir <= 360 and wspd >= 0 and wspd < 300):
                    uwnd, vwnd = meteo_utils.meteo_utils().dir_speed_2_uv(wdir, wspd)
                else:
                    uwnd = float_missing_value
                    vwnd = float_missing_value

                data['eastward_wind'] = np.append(data['eastward_wind'], uwnd)
                data['northward_wind'] = np.append(data['northward_wind'], vwnd)

            except KeyError as e:
                if not keyerr:
                    logging.warning(file_name + ' is missing variable ' + e.args[0])
                keyerr = True

                if (e.args[0] == 'dir') | (e.args[0] == 'spd'):
                    data['eastward_wind'] = np.append(data['eastward_wind'], float_missing_value)
                    data['northward_wind'] = np.append(data['northward_wind'], float_missing_value)
                else:
                    outname, missing = get_outname(e.args[0])
                    data[outname] = np.append(data[outname], missing)
    return data


def get_frequency(obs_type, freqerr):

    if obs_type == 'IR':
        freq = 2.99792458E+14/10.7
    elif (obs_type == 'WVCA') | (obs_type == 'WVCT') | (obs_type == 'WV'):
        freq = 2.99792458E+14/6.7
    elif (obs_type == 'SWIR'):
        freq = 2.99792458E+14/3.9
    elif (obs_type == 'VIS'):
        freq = 2.99792458E+14/0.65
    else:
        if not freqerr:
            logging.warning('Unknown channel type: ' + obs_type)
        freqerr = True
        freq = float_missing_value

    return freq, freqerr


def get_id(sat_name, saterr):

    if sat_name == 'HMWR08':
        satid = 173
    elif sat_name == 'MET11':
        satid = 70
    elif sat_name == 'MET8':
        satid = 55
    elif sat_name == 'GOES16':
        satid = 270
    elif sat_name == 'GOES17':
        satid = 271
    else:
        if not saterr:
            logging.warning('Unknown satellite: ' + sat_name)
        saterr = True
        satid = int_missing_value

    return satid, saterr


def get_outname(key):

    if key == 'type':
        outname = 'sensorCentralFrequency'
        missing = float_missing_value
    elif key == 'sat':
        outname = 'satellite'
        missing = int_missing_value
    elif (key == 'day') | (key == 'hms'):
        outname = 'dateTime'
        missing = int_missing_value
    elif key == 'lat':
        outname = 'latitude'
        missing = float_missing_value
    elif key == 'lon':
        outname = 'longitude'
        missing = float_missing_value
    elif key == 'pre':
        outname = 'air_pressure'
        missing = float_missing_value
    elif key == 'rff':
        outname = 'sensorZenithAngle'
        missing = float_missing_value
    elif key == 'qi':
        outname = 'windTrackingCorrelation'
        missing = float_missing_value
    elif key == 'int':
        outname = 'windHeightAssignMethod'
        missing = int_missing_value

    return outname, missing


if __name__ == "__main__":

    from argparse import ArgumentParser

    parser = ArgumentParser(
        description=('Read a satwind AMV ascii/csv file from NESDIS'
                     ' and convert into IODA output file')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument('-i', '--input-files', nargs='+', dest='file_names',
                          action='store', default=None, required=True,
                          help='input files')
    required.add_argument('-o', '--output-file', dest='output_file',
                          action='store', default=None, required=True,
                          help='output file')
    required.add_argument('-d', '--date', dest='datetimeReference',
                          action='store',
                          help='date reference string (YYYYMMDDHH)')

    parser.set_defaults(debug=False)
    parser.set_defaults(verbose=False)
    parser.set_defaults(datetimeReference=" ")
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

    dtg = datetime.strptime(args.datetimeReference, '%Y%m%d%H')
    datetimeRef = dtg.isoformat() + "Z"

    main(args.file_names, args.output_file, datetimeRef)
