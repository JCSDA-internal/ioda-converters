#!/usr/bin/env python3
#
# (C) Copyright 2020-2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#


"""
Python code to ingest Ocean Surface Wind (OSW) data
"""
import logging
import argparse
from datetime import datetime, timedelta, timezone
import os.path
import sys
import pandas as pd

import h5py
import numpy as np

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import int_missing_value, long_missing_value, float_missing_value
from pyiodaconv.def_jedi_utils import record_time
from collections import defaultdict

# Globals
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
obsQcName = iconv.OqcName()


GlobalAttrs = {
    "converter": os.path.basename(__file__),
    "ioda_version": 2,
    "platformCommonName": "OSW",
    "platformLongDescription": "Ocean Surface Winds retrieved from sea surface. ",
    "source": "OSW",
    "sourceFiles": ""
}

# The outgoing IODA MetaData variables, their data type, units
MetaDataKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z"),
    ("sensorIdentification", "string", ""),
    ("height", "float", "m"),
]
meta_keys = [m_item[0] for m_item in MetaDataKeyList]

# The outgoing IODA variables (ObsValues), their units, and assigned constant ObsError.
obsvars = ['windSpeed', 'windSpeed'+obsErrName, 'windSpeed'+obsQcName]
# Add wind components to be able to assign obsError with dummy values
obsvars_dummy = ['windEastward', 'windNorthward']
# List of the observables
obsvars_list = [obsvars[0]] + obsvars_dummy
obsvars_units = 'm s-1'
obsvars_dtype = 'float'

# Assign dimensions to the obs values
VarDims = {
    'windSpeed': ['Location'],
    'windEastward': ['Location'],
    'windNorthward': ['Location'],
}

# Assign missing value details for the variables
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
    if args.debug:
        logging.basicConfig(level=logging.INFO)
    elif args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.ERROR)

    tic = record_time()

    # Loop through input files and concatenate into dataframe
    file_cnt = 0
    obs_data_list = []
    for file_name in args.file_names:
        # check if file exists
        if not os.path.isfile(file_name):
            logging.debug(f'Input (-i option) file: {file_name} does not exist')
            print(f'Input (-i option) file: {file_name} does not exist')
            sys.exit()
        logging.info(f"Reading input file: {file_name}")

        # Open file
        # (need to move file and dat ref into get_data_from_file, but then need to move adjust data_append inside also.)
        file = h5py.File(file_name, 'r')

        # Figure out what source the file is coming from (e.g. cygnss, muon, spire)
        osw_source = get_data_source(file)

        # Get reference time and convert to epoch time
        dat_ref = get_reference_time(file, osw_source)

        # Get data from file in temporary obs_data_append DF
        obs_data_append = get_data_from_file(file, meta_keys+obsvars, osw_source, file_name)

        # Change time reference
        if osw_source != 'Muon-L3':
            obs_data_append = adjust_dateTime(obs_data_append, dat_ref)

        # Change longitude range
        obs_data_append = adjust_longitude(obs_data_append, osw_source)

        # Apply quality control add strict if selected
        obs_data_append = quality_control(obs_data_append, qc_strict=args.qc_strict)

        # Save for a single concat after the loop (avoids repeated concats
        # against an empty/all-NA seed frame, which pandas now warns about)
        obs_data_list.append(obs_data_append)

        file.close()
        # count files
        file_cnt += 1

    # Combine all files' data into a single dataframe
    if obs_data_list:
        obs_data = pd.concat(obs_data_list, ignore_index=True)
    else:
        obs_data = pd.DataFrame(columns=meta_keys+obsvars)

#   a value of 0. is set. Using missing value triggers UFO rejection
    for iodavar in obsvars_dummy:
        obs_data[iodavar] = 0.
        obs_data[iodavar+obsQcName] = 0

    # replace missing values
    for MetaDataKey in MetaDataKeyList:
        obs_data[MetaDataKey[0]] = obs_data[MetaDataKey[0]].fillna(missing_vals[MetaDataKey[1]])
    iodavar = obsvars[0]
    obs_data[iodavar] = obs_data[iodavar].fillna(missing_vals[obsvars_dtype])

    # find where windSpeed is missing and set the components to missing as well
    mask = obs_data['windSpeed'] == missing_vals[obsvars_dtype]
    for iodavar in obsvars_dummy:
        obs_data[iodavar] = obs_data[iodavar].mask(mask.values, missing_vals[obsvars_dtype])

    # sort by instrument and then time
    if args.sort:
        obs_data.sort_values(['sensorIdentification', 'dateTime'], ascending=[True, True], inplace=True)

    # count number of locations
    ntotal = obs_data.shape[0]

    # set global reference date to release time
    GlobalAttrs['datetimeReference'] = datetime.fromtimestamp(obs_data['dateTime'].min()).strftime("%Y-%m-%dT%H:%M:%SZ")
    # append platform to long description
    long_description = add_long_description(osw_source)
    GlobalAttrs['platformLongDescription'] += long_description

    # Export into IODA formatted netCDF file
    ioda_data = {}
    DimDict = {'Location': ntotal}
    GlobalAttrs['sourceFiles'] = GlobalAttrs['sourceFiles'][2:]
    varDict = defaultdict(lambda: DefaultOrderedDict(dict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    # Set coordinates and units of the ObsValues.
    for iodavar in obsvars_list:
        # set the obs space attributes
        varDict[iodavar]['valKey'] = iodavar, obsValName
        varDict[iodavar]['errKey'] = iodavar, obsErrName
        varDict[iodavar]['qcKey'] = iodavar, obsQcName
        varAttrs[iodavar, obsValName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsErrName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsQcName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsValName]['units'] = obsvars_units
        varAttrs[iodavar, obsErrName]['units'] = obsvars_units

    # Set units of the MetaData variables and all _FillValues.
    for key in meta_keys:
        dtypestr = MetaDataKeyList[meta_keys.index(key)][1]
        if MetaDataKeyList[meta_keys.index(key)][2]:
            varAttrs[(key, metaDataName)]['units'] = MetaDataKeyList[meta_keys.index(key)][2]
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]
        ioda_data[(key, metaDataName)] = np.array(obs_data[key], dtype=dtypes[dtypestr])

    # Transfer from the 1-D data vectors and ensure output data (ioda_data) types using numpy.
    for iodavar in obsvars_list:
        ioda_data[(iodavar, obsValName)] = np.array(obs_data[iodavar], dtype=np.float32)
        ioda_data[(iodavar, obsQcName)] = np.array(obs_data[iodavar+obsQcName], dtype=np.int32)
        if iodavar == 'windSpeed':
            ioda_data[(iodavar, obsErrName)] = np.array(obs_data[iodavar+obsErrName], dtype=np.float32)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output_file, MetaDataKeyList, DimDict)
    # write everything out
    writer.BuildIoda(ioda_data, VarDims, varAttrs, GlobalAttrs)

    # report time
    toc = record_time(tic=tic)


def get_data_source(afile):

    keys = ['title', 'constellation']
    title = None

    for key in keys:
        value = afile.attrs.get(key, None)
        if value is not None:
            title = value.decode('UTF-8') if isinstance(value, bytes) else value
            break

    if title is None:
        # throw error
        pass
    elif 'CYGNSS' in title:
        return 'CYGNSS'
    elif 'Muon' in title:
        if 'Level 3' in title:
            return 'Muon-L3'
        else:
            return 'Muon'
    elif 'MUON' in title:
        return 'MUON'
    elif 'spire' in title:
        return 'Spire'
    elif 'SPIRE' in title:
        return 'Spire-L2'
    else:
        # throw error
        print(f' ... WARNING: did not recognize source title: {title}')
        pass


def get_reference_time(afile, osw_source):
    if osw_source == 'CYGNSS' or osw_source == 'Spire-L2' or osw_source == 'MUON':
        dat_ref = afile['sample_time'].attrs['units'].decode('UTF-8').split('since ')[-1]
        dat_ref = datetime.strptime(dat_ref, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc).timestamp()
    elif osw_source == 'Muon-L3':
        # Parse the start and end times
        time_start = datetime.strptime(afile.attrs['time_start'], '%Y%m%dT%HZ').replace(tzinfo=timezone.utc)
        # time_end does not follow iso_standard (allows T24Z)
        # time_end = datetime.strptime(afile.attrs['time_end'], '%Y%m%dT%HZ').replace(tzinfo=timezone.utc)
        time_end = time_start + timedelta(hours=1)
        # Calculate the average
        dat_ref = (time_start.timestamp() + time_end.timestamp()) / 2
    elif osw_source == 'Muon':
        # note same as CYGNSS except item key is simply time
        dat_ref = afile['time'].attrs['units'].split('since ')[-1]
        dat_ref = datetime.strptime(dat_ref, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc).timestamp()
    elif osw_source == 'Spire':
        # the precision of the seconds appears troublesome when too many digits
        # take the floor of the seconds by stripping of fractional seconds
        dat_ref = afile['sample_time'].attrs['units'].decode('UTF-8').split()[-1]
        dat_ref = dat_ref.split('.')[0]
        dat_ref = datetime.strptime(dat_ref, '%Y-%m-%dT%H:%M:%S').replace(tzinfo=timezone.utc).timestamp()
    return dat_ref


def get_data_from_file(afile, col_names, osw_source, file_name):

    # Pull each data type (variable) and create a list
    if osw_source == 'CYGNSS' or osw_source == 'Spire-L2' or osw_source == "MUON":
        latitude = [v for v in afile['lat']]
        longitude = [v for v in afile['lon']]
        dateTime = [int(v) for v in afile['sample_time']]  # datetime with different ref time
        windSpeed = [v for v in afile['wind_speed']]
        # bits are in afile['sample_flags'].attrs['flag_meanings']
        # ! Bit 1 == 2^0 == poor_overall_quality_flag
        # ! Bit 2 == 2^1 == Ascending Node Flag (1=ascending; 0=descending)
        windSpeedPreQC = [get_normalized_bit(v, bit_index=0) for v in afile['sample_flags']]
        satelliteAscendingFlag = [get_normalized_bit(v, bit_index=1) for v in afile['sample_flags']]
        windSpeedObsError = [v for v in afile['wind_speed_uncertainty']]
        sensorIdentification = [str(v) for v in afile['sv_num']]  # sv_num is the GPS space vehicle number
    elif osw_source == 'Muon-L3':
        import re
        instrument_ref = re.search(r'MuSat\d+', afile.attrs['title'])
        if instrument_ref:
            instrument_ref = instrument_ref.group()
        else:
            instrument_ref = osw_source
        # latitude, longitude and OSW are 2D array
        latitude = [v for row in afile['latitude'] for v in row]
        longitude = [v for row in afile['longitude'] for v in row]
        dat_ref = get_reference_time(afile, osw_source)
        dateTime = [int(dat_ref)] * len(latitude)
        windSpeed = [v for row in afile['ocean_wind_speed_level3'] for v in row]
        fillValue = afile['ocean_wind_speed_level3'].attrs['_FillValue']
        # use all values in data set to FillValue (-9999)
        windSpeedPreQC = [1 if value == fillValue else 0 for value in windSpeed]
        windSpeedObsError = [5.0] * len(latitude)
        sensorIdentification = [instrument_ref]*len(latitude)
    elif osw_source == 'Muon':
        # Get instrument reference
        instrument_ref = afile.attrs['spacecraft_num']

        latitude = [v for v in afile['lat']]
        longitude = [v for v in afile['lon']]
        dateTime = [int(v) for v in afile['time']]  # datetime with different ref time
        windSpeed = [v for v in afile['wind_speed_level2']]
        windSpeedPreQC = [v for v in afile['retrieval_qual_flag']]
        windSpeedObsError = [v for v in afile['wind_speed_level2_error']]
        sensorIdentification = [instrument_ref]*len(latitude)
    elif osw_source == 'Spire':
        # Get instrument reference
        instrument_ref = afile.attrs['tx_id'].decode('UTF-8')

        latitude = [v for v in afile['sp_lat']]
        longitude = [v for v in afile['sp_lon']]
        dateTime = [int(v) for v in afile['sample_time']]  # datetime with different ref time
        windSpeed = [v for v in afile['wind']]
        windSpeedPreQC = [1 - v for v in afile['wind_confidence']]
        # Data with 0 can be assumed to be ice free
        qualIceFlag = [int(v > 0) for v in afile['quality_ice_flag']]
        # combine quality_ice_flag into the wind_confidence
        windSpeedPreQC = [a or b for a, b in zip(windSpeedPreQC, qualIceFlag)]
        windSpeedObsError = [v for v in afile['wind_std']]
        sensorIdentification = [instrument_ref]*len(latitude)

    # Make a column to have a constant elevation for the "station"
    height = [10]*len(dateTime)

    # Make a list of lists to feed into dataframe
    data_lists = list(zip(latitude, longitude, dateTime, sensorIdentification,
                          height, windSpeed, windSpeedObsError, windSpeedPreQC))

    # All observation data for this file to append to the master dataframe
    obs_data_append = pd.DataFrame(data_lists, columns=col_names)
    return obs_data_append


def adjust_dateTime(obs_DF, dat_ref):
    obs_DF['dateTime'] = obs_DF['dateTime']+dat_ref
    return obs_DF


def adjust_longitude(obs_DF, osw_source):
    obs_DF.loc[obs_DF['longitude'] > 180, 'longitude'] -= 360
    return obs_DF


def add_long_description(osw_source):

    # Add some additional information to platformLongDescription global attribute
    long_description = ''
    if osw_source == 'CYGNSS':
        long_description = 'OSW derived from CYGNSS GNSS-R receivers.'
    elif osw_source == 'Muon':
        long_description = 'Proxy Muon OSW data derived from CYGNSS GNSS-R receivers.'
    elif osw_source == 'Spire':
        long_description = 'OSW derived from Spire GNSS-R receivers.'
    elif osw_source == 'Spire-L2':
        long_description = 'NOAA Level-2 OSW from Spire GNSS-R receivers.'
    elif osw_source == 'MUON':
        long_description = 'NOAA Level-2 OSW from Muon GNSS-R receivers.'
    return long_description


def quality_control(obs_data, qc_strict=False):
    # Apply initial QC for physically possible values (wind, lat, lon) need to add swells
    ob_range = [0.0, 75.0]
    lat_range = [-90, 90]
    lon_range = [-180, 180]
    ob_check = ((obs_data['windSpeed'] < ob_range[0]) | (obs_data['windSpeed'] > ob_range[1]))
    lat_check = ((obs_data['latitude'] < lat_range[0]) | (obs_data['latitude'] > lat_range[1]))
    lon_check = ((obs_data['longitude'] < lon_range[0]) | (obs_data['longitude'] > lon_range[1]))

    # (default) Replace with None to be filled with missing value later
    obs_data.loc[ob_check, 'windSpeed'] = None
    obs_data.loc[lat_check, 'latitude'] = None
    obs_data.loc[lon_check, 'longitude'] = None

    if qc_strict:
        # (strict) remove
        obs_data.drop(ob_check[ob_check].index, inplace=True)
        obs_data.drop(lat_check[lat_check].index, inplace=True)
        obs_data.drop(lon_check[lon_check].index, inplace=True)
        # (strict) additional rejection on provider PreQC
        qc_check = (obs_data['windSpeedPreQC'] > 0)
        obs_data.drop(qc_check[qc_check].index, inplace=True)
        logging.debug(f'rejected by PreQC: {sum(qc_check)}')
    return obs_data


def get_normalized_bit(value, bit_index):
    return (value >> bit_index) & 1


if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description=(
            'Read windorne json files and convert into IODA output file')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument('-i', '--input-files', nargs='+', dest='file_names',
                          action='store', default=None, required=True,
                          help='input files')
    required.add_argument('-o', '--output-file', dest='output_file',
                          action='store', default=None, required=True,
                          help='output file')
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--date', metavar="YYYYMMDDHH", type=str, default=None,
                          help='base date for the center of the window')
    optional.add_argument('--debug', action='store_true',
                          help='enable debug messages')
    optional.add_argument('-q', '--qualitycontrol', dest='qc_strict',
                          default=False, action='store_true', required=False,
                          help='turn on strict quality control checks dropping these points on output')
    optional.add_argument('--sort', action='store_true',
                          default=False, help='Sort data by instruments then time')
    optional.add_argument('--verbose', action='store_true',
                          help='enable verbose debug messages')

    # read in arguments to function call
    args = parser.parse_args()

#   # verify time format
#   try:
#       # to do must convert 2022051812 to 2022-05-18T12:00:00Z
#       target_time = datetime.fromisoformat(args.date_string[:-1])
#   except Exception:
#       parser.error('Date format invalid: ', args.date_string, ' must be like: 2022-05-18T12:00:00Z')
#       sys.exit()

    main(args)
