#!/usr/bin/env python3

import sys
import os
import time
from datetime import datetime, timezone, timedelta
import numpy as np
import logging
from urllib.request import Request, urlopen

# These modules need the path to lib-python modules
import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

os.environ["TZ"] = "UTC"

# these are the unique values in the raw input file
varDict = {'height': ['height', "float", "km"],
           'electronDensity': ['electronDensity', "float", 'number cm-3'],
           'electronDensityConfidence': ['electronDensityConfidence', "float", 'number cm-3']}

# these are the MetaData common to each input
locationKeyList = [("latitude", "float", "degrees_north"),
                   ("longitude", "float", "degrees_east"),
                   ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z"),
                   ("sequenceNumber", "integer", "record for use to identify and group edp profile"),
                   ("stationIdentifier", "string", "")]

meta_keys = [m_item[0] for m_item in locationKeyList]

GlobalAttrs = {
    'converter': os.path.basename(__file__),
    'ioda_version': 3,
    'description': 'Ionosonde profiler',
    'source': 'GlobalIonosphereRadioObservatory',
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
    output_base = args.output

    start_time = time.time()

    all_data = None
    any_data = False
    recordNumber = args.recordnumber
    for fname in file_names:
        logging.info(f"Reading file:  {fname}")
        file_data, any_data = read_file(fname, recordNumber, any_data, qc_strict=args.qc_strict)
        # if there is data do something
        if file_data:
            # first successful read
            if not all_data:
                all_data = file_data
            # subsequent successful read
            else:
                for key in set(varDict.keys()).union(meta_keys):
                    # data[key] = np.concatenate(data[key], file_data[key]) # why not and gotta be much better way
                    all_data[key] = np.append(all_data[key], file_data[key])
            recordNumber += 1

    for key in all_data.keys():
        all_data[key] = np.asarray(all_data[key])

    # if all files have no data
    if not any_data:
        logging.error("No data to write, stopping execution.")
        sys.exit()

    dtg_start = datetime.fromtimestamp(all_data['dateTime'][0])
    dtg_end = datetime.fromtimestamp(all_data['dateTime'][-1])
    dtg = dtg_start
    window = args.window
    datetimeRef = dtg.isoformat() + "Z"
    while dtg <= dtg_end:

        data = {}
        start = dtg.timestamp()
        end_dtg = dtg + timedelta(hours=window)
        end = end_dtg.timestamp()
        time_id = (all_data['dateTime'] > start) & (all_data['dateTime'] <= end)

        for key in all_data.keys():
            data[key] = all_data[key][time_id]

        # prepare global attributes we want to output in the file,
        # in addition to the ones already loaded in from the input file
        GlobalAttrs = {
            'sourceFiles': ", ".join(file_names),
            'datetimeReference': datetimeRef}

        nlocs = len(data['height'])
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
            if 'electronDensity' in key:
                continue
            dtype = varDict[key][1]
            units = varDict[key][2]
            if units:
                varAttrs[(key, metaDataName)]['units'] = units
            varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtype]

        # Set units and FillValue attributes for groups associated with observed variable.
        variable = varDict['electronDensity'][0]
        dtype = varDict['electronDensity'][1]
        units = varDict['electronDensity'][2]
        unitsErr = varDict['electronDensityConfidence'][2]
        varAttrs[(variable, obsValName)]['units'] = units
        varAttrs[(variable, obsErrName)]['units'] = unitsErr
        varAttrs[(variable, obsValName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, obsErrName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, qcName)]['coordinates'] = 'longitude latitude'
        varAttrs[(variable, obsValName)]['_FillValue'] = missing_vals[dtype]
        varAttrs[(variable, obsErrName)]['_FillValue'] = missing_vals[dtype]
        varAttrs[(variable, qcName)]['_FillValue'] = int_missing_value

        # Fill the final IODA data:  MetaData then ObsValues, ObsErrors, and QC
        ioda_data = {}

        # should populate ioda_data directly rather than creating another copy
        for key in meta_keys:
            dtype = locationKeyList[meta_keys.index(key)][1]
            ioda_data[(key, metaDataName)] = np.array(data[key], dtype=dtypes[dtype])
        for key in varDict.keys():
            variable = varDict[key][0]
            dtype = varDict[key][1]
            if 'electronDensity' not in key:
                logging.info(f" the variable: {variable} will be placed into MetaData of ioda_data")
                # these MetaData are arrays nlocs long already
                ioda_data[(key, metaDataName)] = np.array(data[variable], dtype=dtypes[dtype])
            elif 'Confidence' not in key:
                # (electronDensityConfidence) is used as the ObsError
                variable = varDict[key][0]
                logging.info(f" the variable: {variable} will be placed into ObsValue of ioda_data")
                ioda_data[(variable, obsValName)] = np.array(data[variable], dtype=np.float32)
                ioda_data[(variable, obsErrName)] = np.array(data[variable+'Confidence'], dtype=np.float32)
                qc_array_hack = apply_gross_quality_control(data, qc_strict=args.qc_strict)
                ioda_data[(variable, qcName)] = np.array(qc_array_hack, dtype=np.int32)  # how to interpret AQI ?

        mid = dtg + timedelta(hours=window/2)
        outdate = dtg.strftime('%Y%m%dT%H%M%SZ')
        output_file = f'{output_base}obs.{outdate}_PT{window}H_ionosonde.nc4'
        logging.debug("Writing file: " + output_file)

        # setup the IODA writer and write everything out.
        writer = iconv.IodaWriter(output_file, locationKeyList, DimDict)
        writer.BuildIoda(ioda_data, varDims, varAttrs, GlobalAttrs)
        dtg = end_dtg

    logging.info("--- {:9.4g} total seconds ---".format(time.time() - start_time))


def read_file(file_name, recordNumber, any_data, qc_strict=True):

    local_data = init_data_dict()

    # get lat lon location from file name
    station_id, date = file_name.split('/')[-1].split('_')
    year = date[0:4]
    month = date[4:6]
    day = date[6:8]
    lat, lon = get_loc(station_id, year, month, day)
    if isinstance(lat, str):
        return local_data, any_data

    # Open the file
    with open(file_name, 'r') as file:
        # Create an iterator from the file object
        file_iterator = iter(file)
        any_data = False
        prof_count = 0
        while True:
            try:
                # Get the next line from the iterator
                line = next(file_iterator)
                hour, minute, second = line.split()[-1].split(':')
                line = next(file_iterator)
                prof_count += 1
                if line.lstrip()[0].isdigit():
                    prof_count += 1
                    any_data = True
                    height = line.split()
                    for ht in height:
                        local_data['height'].append(float(ht))
                        local_data['sequenceNumber'].append(prof_count)
                else:
                    local_data['height'].append(np.nan)
                    local_data['sequenceNumber'].append(prof_count)

                line = next(file_iterator)
                if line.lstrip()[0].isdigit():
                    plasma_freq = line.split()
                    for pf in plasma_freq:
                        local_data['electronDensity'].append(float(pf) ** 2 * 12400)
                        local_data['electronDensityConfidence'].append(float(pf) ** 2 * 12400 * 0.2)
                        local_data['dateTime'].append(datetime.strptime(f'{year}{month}{day}{hour}{minute}{second}', '%Y%m%d%H%M%S').timestamp())
                        local_data['latitude'].append(lat)
                        local_data['longitude'].append(lon)
                        local_data['stationIdentifier'].append(station_id)
                else:
                    local_data['electronDensity'].append(np.nan)
                    local_data['electronDensityConfidence'].append(np.nan)
                    local_data['dateTime'].append(datetime.strptime(f'{year}{month}{day}{hour}{minute}{second}', '%Y%m%d%H%M%S').timestamp())
                    local_data['latitude'].append(lat)
                    local_data['longitude'].append(lon)
                    local_data['stationIdentifier'].append(station_id)
                line = next(file_iterator)
            except StopIteration:
                # If StopIteration is raised, break from the loop
                break

    return local_data, any_data


def init_data_dict():
    local_data = {}              # Before assigning the output types into the above.
    for key in set(varDict.keys()).union(meta_keys):
        local_data[key] = []
    return local_data


def get_loc(station_id, year, month, day):

    '''
    station list taken from https://www.digisonde.com/stationlist.php and from documentation provided by Iurii Cherniak
    '''
    stations = {'AA343': {'lat': 43.18, 'lon': 76.95},
                'AH223': {'lat': 23, 'lon': 72.5},
                'AL945': {'lat': 45.07, 'lon': 276.44},
                'AN438': {'lat': 37.39, 'lon': 126.95},
                'AS00Q': {'lat': -7.95, 'lon': 345.6},
                'AT138': {'lat': 38, 'lon': 23.5},
                'AU930': {'lat': 30.4, 'lon': 262.3},
                'AW426': {'lat': 26.32, 'lon': 127.84},
                'BC840': {'lat': 40, 'lon': 254.7},
                'BE145': {'lat': 44.63, 'lon': 20.75},
                'BLJ03': {'lat': 1.43, 'lon': 311.56},
                'BP440': {'lat': 40.3, 'lon': 116.2},
                'BR52P': {'lat': -27.06, 'lon': 153.06},
                'BV53Q': {'lat': -37.72, 'lon': 145.05},
                'BVJ03': {'lat': 2.8, 'lon': 299.3},
                'CAJ2M': {'lat': -22.7, 'lon': 315},
                'CB53N': {'lat': -35.32, 'lon': 149},
                'CS31K': {'lat': -12.18, 'lon': 96.83},
                'CGK21': {'lat': -20.5, 'lon': 305},
                'CO764': {'lat': 64.9, 'lon': 212},
                'CS999': {'lat': 38.83, 'lon': 255.18},
                'DB049': {'lat': 50.1, 'lon': 4.6},
                'DH224': {'lat': 24.24, 'lon': 54.58},
                'DV36Q': {'lat': -68.6, 'lon': 78},
                'EA036': {'lat': 37.1, 'lon': 353.3},
                'EA653': {'lat': 52.73, 'lon': 185.92},
                'EB040': {'lat': 40.8, 'lon': 0.5},
                'EG931': {'lat': 30.5, 'lon': 273.5},
                'EI764': {'lat': 64.66, 'lon': 212.93},
                'FF051': {'lat': 51.7, 'lon': 358.5},
                'FZA0M': {'lat': -3.9, 'lon': 321.6},
                'GA313': {'lat': 13.46, 'lon': 79.17},
                'GA762': {'lat': 62.38, 'lon': 215},
                'GM037': {'lat': 37.9, 'lon': 14},
                'GR13L': {'lat': -33.3, 'lon': 26.5},
                'GSJ53': {'lat': 53.3, 'lon': 299.7},
                'GU513': {'lat': 13.62, 'lon': 144.86},
                'HA419': {'lat': 19.4, 'lon': 109},
                'HE13N': {'lat': -34.42, 'lon': 19.22},
                'IC437': {'lat': 37.14, 'lon': 127.54},
                'IF843': {'lat': 43.81, 'lon': 247.32},
                'IL008': {'lat': 8.5, 'lon': 4.5},
                'IR352': {'lat': 52.4, 'lon': 104.3},
                'JI91J': {'lat': -12, 'lon': 283.2},
                'JJ433': {'lat': 33.43, 'lon': 126.3},
                'JR055': {'lat': 54.6, 'lon': 13.4},
                'KJ609': {'lat': 9.4, 'lon': 167.2},
                'KR835': {'lat': 35, 'lon': 253.47},
                'KS759': {'lat': 58.4, 'lon': 203.6},
                'LAA38': {'lat': 38.77, 'lon': 332.91},
                'LD160': {'lat': 60, 'lon': 30.7},
                'LL721': {'lat': 21.43, 'lon': 201.85},
                'LM42B': {'lat': -21.8, 'lon': 114.1},
                'LV12P': {'lat': -28.5, 'lon': 21.2},
                'ME929': {'lat': 29.7, 'lon': 278.01},
                'MH453': {'lat': 52, 'lon': 122.52},
                'MHJ45': {'lat': 42.6, 'lon': 288.5},
                'MI540': {'lat': 40.71, 'lon': 141.38},
                'MIJ42': {'lat': 42.5, 'lon': 288.8},
                'MO155': {'lat': 55.47, 'lon': 37.3},
                'MU12K': {'lat': -22.39, 'lon': 30.88},
                'MU834': {'lat': 33.03, 'lon': 72.01},
                'N0369': {'lat': 69.4, 'lon': 88.1},
                'ND328': {'lat': 28.64, 'lon': 77.17},
                'NDA81': {'lat': 81.4, 'lon': 342.5},
                'NI135': {'lat': 35.03, 'lon': 33.16},
                'NQJ61': {'lat': 61.2, 'lon': 314.6},
                'OK426': {'lat': 26.68, 'lon': 128.15},
                'PA836': {'lat': 34.8, 'lon': 239.5},
                'PF765': {'lat': 65.13, 'lon': 212.55},
                'PQ052': {'lat': 50, 'lon': 14.6},
                'PRJ18': {'lat': 18.5, 'lon': 292.9},
                'PSJ5J': {'lat': -51.6, 'lon': 302.1},
                'RL052': {'lat': 51.5, 'lon': 359.4},
                'RO041': {'lat': 41.9, 'lon': 12.5},
                'SA418': {'lat': 18.34, 'lon': 109.42},
                'SA929': {'lat': 29.45, 'lon': 261.39},
                'SAA0K': {'lat': -2.6, 'lon': 315.8},
                'SE834': {'lat': 34.35, 'lon': 253.12},
                'SH427': {'lat': 26.86, 'lon': 111.5},
                'SMJ67': {'lat': 67, 'lon': 309.3},
                'SMK29': {'lat': -29.73, 'lon': 306.29},
                'SN437': {'lat': 37.1, 'lon': 127},
                'SO148': {'lat': 47.63, 'lon': 16.72},
                'THJ76': {'lat': 76.54, 'lon': 291.56},
                'THJ77': {'lat': 77.5, 'lon': 290.8},
                'TM308': {'lat': 8.54, 'lon': 76.87},
                'TR0P2': {'lat': -72.01, 'lon': 2.53},
                'TR169': {'lat': 69.6, 'lon': 19.2},
                'VT139': {'lat': 40.6, 'lon': 17.8},
                'WA619': {'lat': 19.29, 'lon': 166.65},
                'WP937': {'lat': 37.9, 'lon': 284.5},
                'WU430': {'lat': 30.5, 'lon': 114.4},
                'XI434': {'lat': 35.3, 'lon': 113.92},
                'YA462': {'lat': 62, 'lon': 129.6},
                'ZH466': {'lat': 66.8, 'lon': 123.4},
                'ZS36R': {'lat': -69.4, 'lon': 76.4}}
    if station_id not in stations:
        print(f'Unknown station {station_id}. Skipping')
        return 'lat', 'lon'

    return stations[station_id]['lat'], stations[station_id]['lon']


def apply_gross_quality_control(data, qc_strict=False):
    # if strict quality-control is requested
    # apply using simple physical reality check on variables

    # initialize returned variable
    qc_array_hack = np.zeros_like(data['electronDensity'], dtype=np.int32)
    # is requested apply check
    if qc_strict:
        qc_array_hack = np.where(
            (data['electronDensity'].astype(float) < 0)
            | (data['height'].astype(float) < 0)
            | (data['criticalFrequency'].astype(float) < 0),
            1,
            0)
    return qc_array_hack


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
                          help='output base name. Files will be written out as {basename}_PT{window}_{datetime}.nc')

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--debug', action='store_true', default=False,
                          help='enable debug messages')
    optional.add_argument('--window', default=1, type=int,
                          help='output file window in hours')
    optional.add_argument('--quality-control', action='store_true',
                          default=False, dest='qc_strict',
                          help='add PreQC values')
    optional.add_argument('--recordnumber',
                          type=int, default=1,
                          help=' optional starting record number to associate with profile ')
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
