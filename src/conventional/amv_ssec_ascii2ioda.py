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

known_freq = {'IR': 2.99792458E+14/10.7,
              'WVCA': 2.99792458E+14/6.7,
              'WVCT': 2.99792458E+14/6.7,
              'WV': 2.99792458E+14/6.7,
              'SWIR': 2.99792458E+14/3.9,
              'VIS': 2.99792458E+14/0.65}

known_sat = {'HMWR08': 173,
             'MET11': 70,
             'MET8': 55,
             'GOES16': 270,
             'GOES17': 271}

known_var = {'type': ['sensorCentralFrequency', float_missing_value],
             'sat': ['satellite', int_missing_value],
             'day': ['dateTime', int_missing_value],
             'hms': ['dateTime', int_missing_value],
             'lat': ['latitude', float_missing_value],
             'lon': ['longitude', float_missing_value],
             'pre': ['air_pressure', float_missing_value],
             'rff': ['sensorZenithAngle', float_missing_value],
             'qi': ['windTrackingCorrelation', float_missing_value],
             'int': ['windHeightAssignMethod', int_missing_value],
             'spd': ['speed', float_missing_value],
             'dir': ['direction', float_missing_value]}


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
        'sourceFiles': ", ".join(file_names),
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
    local_data = {}              # Before assigning the output types into the above.
    for key in varDict.keys():
        local_data[key] = []
    for key in meta_keys:
        local_data[key] = []

    with open(file_name, newline='') as f:
        reader = csv.DictReader(f, skipinitialspace=True, delimiter=' ')
        keyerr = False  # no key errors in file

        unk_freq = []   # list of unknown frequencies in file
        unk_sat = []    # list of unknown satellites in file

        for row in reader:
            try:
                year = int(row['day'][0:4])
                month = int(row['day'][4:6])
                day = int(row['day'][6:])
                hour = int(row['hms'][0:2])
                minute = int(row['hms'][2:])
                second = 0
                dtg = datetime(year, month, day, hour, minute, second)
                time_offset = np.int64(round((dtg - epoch).total_seconds()))
                local_data['dateTime'] = np.append(local_data['dateTime'], time_offset)
                local_data['longitude'] = np.append(local_data['longitude'], float(row['lon']))
                local_data['latitude'] = np.append(local_data['latitude'], float(row['lat']))

                pres = float(row['pre'])*100.
                local_data['air_pressure'] = np.append(local_data['air_pressure'], pres)
                wdir = float(row['dir'])*1.0
                wspd = float(row['spd'])*1.0
                if (wdir >= 0 and wdir <= 360 and wspd >= 0 and wspd < 300):
                    uwnd, vwnd = meteo_utils.meteo_utils().dir_speed_2_uv(wdir, wspd)
                else:
                    uwnd = float_missing_value
                    vwnd = float_missing_value

                local_data['eastward_wind'] = np.append(local_data['eastward_wind'], uwnd)
                local_data['northward_wind'] = np.append(local_data['northward_wind'], vwnd)

                if row['type'] in known_freq.keys():
                    freq = known_freq[row['type']]
                else:
                    freq = float_missing_value
                    unk_freq.append(row['type'])
                local_data['sensorCentralFrequency'] = np.append(local_data['sensorCentralFrequency'], freq)

                if row['sat'] in known_sat.keys():
                    satid = known_sat[row['sat']]
                else:
                    satid = int_missing_value
                    unk_sat.append(row['sat'])
                local_data['satelliteID'] = np.append(local_data['satelliteID'], satid)

                local_data['sensorZenithAngle'] = np.append(local_data['sensorZenithAngle'], float(row['rff']))
                local_data['windTrackingCorrelation'] = np.append(local_data['windTrackingCorrelation'], float(row['qi']))
                local_data['windHeightAssignMethod'] = np.append(local_data['windHeightAssignMethod'], int(row['int']))

            except KeyError as e:
                if not keyerr:
                    logging.warning(file_name + ' is missing variable ' + e.args[0])
                keyerr = True

                if (e.args[0] == 'dir') or (e.args[0] == 'spd'):
                    local_data['eastward_wind'] = np.append(local_data['eastward_wind'], float_missing_value)
                    local_data['northward_wind'] = np.append(local_data['northward_wind'], float_missing_value)
                else:
                    local_data[known_var[e.args[0]][0]] = np.append(local_data[known_var[e.args[0]][0]], known_var[e.args[0]][1])

        for f in unk_freq:
            logging.warning(file_name + ' contains unknown frequency ' + f)

        for s in unk_sat:
            logging.warning(file_name + ' contains unknown satellite ID ' + s)

        if len(row.keys()) > len(known_var.keys()):
            for k in row.keys():
                if k not in known_var.keys():
                    logging.warning(file_name + ' contains unknown variable ' + k)

    for key in varDict.keys():
        data[key] = np.append(data[key], local_data[key])
    for key in meta_keys:
        data[key] = np.append(data[key], local_data[key])

    return data


if __name__ == "__main__":

    from argparse import ArgumentParser

    parser = ArgumentParser(
        description=('Read a satwind AMV ascii/csv file from SSEC'
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
