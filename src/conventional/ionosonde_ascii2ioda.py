#!/usr/bin/env python3

import sys
import os
import time
from datetime import datetime
import numpy as np
import netCDF4 as nc
import logging

# These modules need the path to lib-python modules
import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

varDict = {'frequency': ['frequency', 'm s-1'],                         # is this a chirp frequency output or observed
           'f_conf': ['frequencyConfidence', 'fractional percent']}    # not sure
           'density': ['FLayer', 'Unknown']}
           'density_conf': ['FLayerConfidence', 'Unknown']}
           'ARTIST': ['ionosondeScalingParameters', 'Unknown']}         # 3.71 F2 - this is something about how the values are calculated
           'POLAN': ['antennaPolarization', 'Unknown']}                 # 3.91 F - more about sets of assumed parameters? antenna polarization?

locationKeyList = [("latitude", "float", "degrees_north"),
                   ("longitude", "float", "degrees_east"),
                   ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z"),
                   ("height", "float", "m"),
                   ("hmf2", "float", "Unknown"),
                   ("fof2", "float", "Unknown"),
                   ("fof2Confidence", "float", "Unknown"),
                   ("nmf2", "float", "Unknown"),
                   ("nmf2Confidence", "float", "Unknown"),
                   ("stationIdentifier", "integer", "")]

meta_keys = [m_item[0] for m_item in locationKeyList]

GlobalAttrs = {
    'converter': os.path.basename(__file__),
    'ioda_version': 2,
    'description': 'Ionosonde profiler',
#   'source': 'Unknown',
}

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

# variables in "raw" file
known_var = {'date': ['dateTime', long_missing_value],
             'lat': ['latitude', float_missing_value],
             'lon': ['longitude', float_missing_value],
             'height': ['height', float_missing_value],
             'station': ['stationIdentifier', int_missing_value],
             'freq': ['frequency', float_missing_value],
             'f_conf': ['frequencyConfidence', float_missing_value],
             'density': ['FLayer', float_missing_value],
             'density_conf': ['FLayerConfidence', float_missing_value],
             'ARTIST': ['ionosondeScalingParameters', float_missing_value],
             'POLAN': ['antennaPolarization', float_missing_value],
             'dir': ['direction', float_missing_value]}


def main(args):

    file_names = args.input
    output_file = args.output
    dtg = datetime.strptime(args.date, '%Y%m%d%H')
    datetimeRef = dtg.isoformat() + "Z"

    start_time = time.time()

# MetaData appears constant per profile
#   hmf2    fof2    conf         nmf2         conf     lat     lon
# 277.09    4.22    0.00   0.2213E+12   0.5119E+09   45.07  276.44
# AQI=3 AL945_2020275000730 Problem Flags.................. qualscan V2009.10

# ObsValue data row
# height    freq  f_conf      density density_conf  ARTIST   POLAN     ART   POL   ART   POL   ART   POL Slope POLAN 1/2
# 186.00  -99.00  -99.00  -0.9900E+02  -0.9900E+02  0.00 c   2.04 F    4.2   4.2 293.4 277.1  41.7  50.8 563.2        1

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
    DimDict = {'Location': nlocs}

    varDims = {}
    for key in varDict.keys():
        variable = varDict[key][0]
        varDims[variable] = ['Location']

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
                dtg = datetime.strptime(f"{row['day']} {row['hms']}", '%Y%m%d %H%M')
                time_offset = np.int64(round((dtg - epoch).total_seconds()))
                local_data['dateTime'] = np.append(local_data['dateTime'], time_offset)
                local_data['longitude'] = np.append(local_data['longitude'], float(row['lon'])*-1.0)
                local_data['latitude'] = np.append(local_data['latitude'], float(row['lat']))

                pres = float(row['pre'])*100.
                local_data['pressure'] = np.append(local_data['pressure'], pres)

                wdir = float(row['dir'])*1.0
                wspd = float(row['spd'])*1.0
                if (wdir >= 0 and wdir <= 360 and wspd >= 0 and wspd < 300):
                    uwnd, vwnd = meteo_utils.meteo_utils().dir_speed_2_uv(wdir, wspd)
                else:
                    uwnd = float_missing_value
                    vwnd = float_missing_value

                local_data['windEastward'] = np.append(local_data['windEastward'], uwnd)
                local_data['windNorthward'] = np.append(local_data['windNorthward'], vwnd)

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

                local_data['satelliteIdentifier'] = np.append(local_data['satelliteIdentifier'], satid)
                local_data['sensorZenithAngle'] = np.append(local_data['sensorZenithAngle'], float(row['rff']))
                local_data['windTrackingCorrelation'] = np.append(local_data['windTrackingCorrelation'], float(row['qi']))
                local_data['windHeightAssignMethod'] = np.append(local_data['windHeightAssignMethod'], int(row['int']))

            except KeyError as e:
                if not keyerr:
                    logging.warning(file_name + ' is missing variable ' + e.args[0])
                keyerr = True

                if (e.args[0] == 'dir') or (e.args[0] == 'spd'):
                    local_data['windEastward'] = np.append(local_data['windEastward'], float_missing_value)
                    local_data['windNorthward'] = np.append(local_data['windNorthward'], float_missing_value)
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
    required.add_argument('-i', '--input', nargs='+',
                          action='store', default=None, required=True,
                          help='input files')
    required.add_argument('-o', '--output',
                          action='store', default=None, required=True,
                          help='output file')
    required.add_argument('-d', '--date',
                          action='store',
                          help='date reference string (YYYYMMDDHH)')

    parser.set_defaults(debug=False)
    parser.set_defaults(verbose=False)
    parser.set_defaults(date=" ")

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

    for file_name in args.input:
        if not os.path.isfile(file_name):
            parser.error('Input (-i option) file: ', file_name, ' does not exist')

    main(args)
