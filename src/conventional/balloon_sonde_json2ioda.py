#!/usr/bin/env python3

#
# (C) Copyright 2020-2023 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#


"""
Python code to ingest JSON WindBorne Data
"""

import pdb
import re
import logging
import math
import os
import sys
import time
import json
from datetime import datetime, timedelta
from pathlib import Path
import pandas as pd

import numpy as np
import netCDF4 as nc

# These modules need the path to lib-python modules
import pyiodaconv.ioda_conv_engines as iconv
import pyiodaconv.meteo_utils as meteo_utils
import pyiodaconv.meteo_sounding_utils as meteo_souding_utils
from pyiodaconv.orddicts import DefaultOrderedDict
from collections import defaultdict

logger = logging.getLogger("decodeSounding")

# The outgoing IODA MetaData variables, their data type, and units.
MetaDataKeyList = [
    ("stationIdentification", "string", ""),
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("stationElevation", "float", "m"),  # put in fake value (10 m). take out later
    ("height", "float", "m"),  # geometricHeight
    ("pressure", "float", "Pa"),
    ("releaseTime", "long", "seconds since 1970-01-01T00:00:00Z"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z")
]
meta_keys = [m_item[0] for m_item in MetaDataKeyList]

# The outgoing IODA variables (ObsValues), their units, and assigned constant ObsError.
obsvars = ['airTemperature',
           'relativeHumidity',
           'windEastward',
           'windNorthward']
obsvars_units = ['K', 'kg kg-1', 'K', 'm s-1', 'm s-1']
obserrlist = [1.2, 0.75E-3, 1.5, 1.7, 1.7]
obsvars_dtype = ['float',
                 'float',
                 'float',
                 'float']

# I'm not sure what this is used for yet ???
VarDims = {
    'airTemperature': ['Location'],
    'relativeHumidity': ['Location'],
    'windEastward': ['Location'],
    'windNorthward': ['Location']
}

# creating data types
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

AttrData = {
    'converter': os.path.basename(__file__),
    'ioda_version': 2,
    "platformCommonName": "Windborne",
    "platformLongDescription": 'Windborne observations converted from json format',
    'source': 'WindBorne Systems',
    'sourceFiles': ''
}

DimDict = {
}

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
double_missing_value = iconv.get_default_fill_val(np.float64)
long_missing_value = iconv.get_default_fill_val(np.int64)
string_missing_value = '_'
iso8601_string = MetaDataKeyList[meta_keys.index('dateTime')][2]
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


def main(args):

    if args.debug:
        logging.basicConfig(level=logging.INFO)
    elif args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.ERROR)

    """
        #---------------------------------------------------------------------------------------------
        Options set. Ingest each file and concatenate.
        #---------------------------------------------------------------------------------------------
    """
    # Loop through input files and concatenate into dataframe
    metaData_files = []
    file_cnt = 0
    for file_name in args.file_names:
        # check if file exists
        if not os.path.isfile(file_name):
            logging.debug(f'Input (-i option) file: {file_name} does not exist')
            print(f'Input (-i option) file: {file_name} does not exist')
            sys.exit()
        logging.debug(f"Reading input file: {file_name}")

        file = json.load(open(file_name))

        if file_cnt == 0:
            # Create data frame to store each file's Meta Dataa
            df_metaData_files = pd.DataFrame(columns=file.keys())
            df_metaData_files.drop(['observations'], axis=1, inplace=True)

            # Create data frame to store all observation data
            obs_data = pd.DataFrame(columns=file['observations'][0].keys())
            obs_data['releaseTime'] = None
            obs_data['stationElevation'] = None

            # Remove data not needed for IODA
            obs_data.drop(['id', 'mission_id'], axis=1, inplace=True)

            # Rename variables to IODA variables
            obs_data.rename(columns={'speed_x': 'windEastward',
                                     'speed_y': 'windNorthward',
                                     'altitude': 'height',
                                     'mission_name': 'stationIdentification',
                                     'humidity': 'relativeHumidity',
                                     'temperature': 'airTemperature',
                                     'timestamp': 'dateTime'}, inplace=True)
        # Fill out each file's meta data
        df_metaData_files.loc[file_cnt] = [file[key] for key in df_metaData_files.keys()]

        # Pull each data type (variable) and create a list
        height = [file['observations'][ii]['altitude'] for ii in range(len(file['observations']))]  # geometric height
        relativeHumidity = [file['observations'][ii]['humidity'] for ii in range(len(file['observations']))]
        latitude = [file['observations'][ii]['latitude'] for ii in range(len(file['observations']))]
        longitude = [file['observations'][ii]['longitude'] for ii in range(len(file['observations']))]
        stationIdentification = [file['observations'][ii]['mission_name'] for ii in range(len(file['observations']))]
        pressure = [file['observations'][ii]['pressure'] for ii in range(len(file['observations']))]
        windEastward = [file['observations'][ii]['speed_x'] for ii in range(len(file['observations']))]
        windNorthward = [file['observations'][ii]['speed_y'] for ii in range(len(file['observations']))]
        airTemperature = [file['observations'][ii]['temperature'] for ii in range(len(file['observations']))]
        dateTime = [file['observations'][ii]['timestamp'] for ii in range(len(file['observations']))]  # datetime

        # List of release time (earliest time in file. this needs to be updated to be earliest time for each instrument, since the file can have multiple)
        releaseTime = [min(dateTime)]*len(height)
        # Make a dummy column to have a constant elevation for the "station"
        stationElevation = [10]*len(height)
        # convert pressure to Pascals from Hectopascals
        try:
            pressure = pressure*100
        except:
            try:
                pressure = [press_i*100 if press_i is not None else press_i for press_i in pressure] # if some values are None
            except:
                pass # if pressure is all None 
        
        # convert temperature from celcius to kelvin
        try:
            airTemperature = airTemperature+np.array(273.15)
        except:
            try:
                airTemperature = [temp_i*273.15 if temp_i is not None else temp_i for temp_i in airTemperature] # if some values are None
            except:
                pass # If airTemperature is all None

        # Make a list of lists to feed into dataframe
        data_lists = list(zip(height, relativeHumidity, latitude, longitude, stationIdentification, pressure,
                              windEastward, windNorthward, airTemperature, dateTime, releaseTime, stationElevation))

        # All observation data for this file to append to the master dataframe
        obs_data_append = pd.DataFrame(data_lists, columns=obs_data.keys())

        # Append to data frame containing all timestamp data
        obs_data = pd.concat([obs_data, obs_data_append], ignore_index=True)

        # count files
        file_cnt += 1

    # replace missing values
    for MetaDataKey in MetaDataKeyList:
        obs_data[MetaDataKey[0]].fillna(missing_vals[MetaDataKey[1]], inplace=True)
    for n, obsvar in enumerate(obsvars):
        obs_data[obsvar].fillna(missing_vals[obsvars_dtype[n]], inplace=True) 

    # sort by instrument and then time
    if args.sort:
        obs_data.sort_values(['stationIdentification', 'dateTime'], ascending=[True, True], inplace=True)

    # count number of locations
    ntotal = obs_data.shape[0]

    # Export into IODA formatted netCDF file
    ioda_data = {}
    DimDict = {'Location': ntotal}
    AttrData['sourceFiles'] = AttrData['sourceFiles'][2:]
    varDict = defaultdict(lambda: DefaultOrderedDict(dict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    # Set coordinates and units of the ObsValues.
    for n, iodavar in enumerate(obsvars):
        # set the obs space attributes
        varDict[iodavar]['valKey'] = iodavar, obsValName
        varDict[iodavar]['errKey'] = iodavar, obsErrName
        varDict[iodavar]['qcKey'] = iodavar, qcName
        varAttrs[iodavar, obsValName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsErrName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, qcName]['coordinates'] = 'longitude latitude'
        varAttrs[iodavar, obsValName]['units'] = obsvars_units[n]
        varAttrs[iodavar, obsErrName]['units'] = obsvars_units[n]

    # Set units of the MetaData variables and all _FillValues.
    for key in meta_keys:
        dtypestr = MetaDataKeyList[meta_keys.index(key)][1]
        if MetaDataKeyList[meta_keys.index(key)][2]:
            varAttrs[(key, metaDataName)]['units'] = MetaDataKeyList[meta_keys.index(key)][2]
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]
        ioda_data[(key, metaDataName)] = np.array(obs_data[key], dtype=dtypes[dtypestr])

    # Transfer from the 1-D data vectors and ensure output data (ioda_data) types using numpy.
    for n, iodavar in enumerate(obsvars):
        ioda_data[(iodavar, obsValName)] = np.array(obs_data[iodavar], dtype=np.float32)
        ioda_data[(iodavar, obsErrName)] = np.full(ntotal, obserrlist[n], dtype=np.float32)
        ioda_data[(iodavar, qcName)] = np.full(ntotal, 2, dtype=np.int32)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output_file, MetaDataKeyList, DimDict)
    # write everything out
    writer.BuildIoda(ioda_data, VarDims, varAttrs, AttrData)


if __name__ == "__main__":

    import argparse

    start_time = time.time()
    today = datetime.today()

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
    optional.add_argument('--debug', action='store_true',
                          help='enable debug messages')
    optional.add_argument('--verbose', action='store_true',
                          help='enable verbose debug messages')
    optional.add_argument('--sort', action='store_true',
                          default=False, help='Sort data by instruments then time')

    # read in arguments to function call
    args = parser.parse_args()

#    # verify time format
#    try:
#        target_time = datetime.fromisoformat(args.date_string[:-1])
#    except Exception:
#        parser.error('Date format invalid: ', args.date_string, ' must be like: 2022-05-18T12:00:00Z')
#        sys.exit()

    main(args)
