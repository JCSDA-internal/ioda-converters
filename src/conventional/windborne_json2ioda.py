import pdb
import re
import logging
import math
import os
import sys
import time
import json
from datetime import datetime, timedelta
import dateutil.parser
from pathlib import Path
import pandas as pd

import numpy as np
import netCDF4 as nc
from cartopy import geodesic
from copy import deepcopy as dcop

# These modules need the path to lib-python modules
import pyiodaconv.ioda_conv_engines as iconv
import pyiodaconv.meteo_utils as meteo_utils
import pyiodaconv.meteo_sounding_utils as meteo_sounding_utils
from pyiodaconv.orddicts import DefaultOrderedDict
from collections import defaultdict

# is this needed ???
os.environ["TZ"] = "UTC"
# what should this be ???
logger = logging.getLogger("decodeSounding")

# The outgoing IODA MetaData variables, their data type, and units.
MetaDataKeyList = [
    ("stationIdentification", "string", ""), # same as mission_name
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("stationElevation", "float", "m"), # could put in fake value (10 m). take out later 
    ("height", "float", "m"), # same as altitude. need to rename height to geometricHeight or similar
    ("pressure", "float", "Pa"), # need to convert to hecta pascal ???
    ("releaseTime", "long", "seconds since 1970-01-01T00:00:00Z"), # use first time in file 
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
    'description': 'Windborne observations converted from json format',
    'source': 'WindBorne Systems',
    'sourceFiles': ''
}

DimDict = {
}

float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
double_missing_value = nc.default_fillvals['f8']
long_missing_value = nc.default_fillvals['i8']
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
    required.add_argument('-d', '--date', dest='date_string', # might need to adjust ???
                          action='store', default=None, help='date format: 2022-05-18T12:00:00Z')

    parser.set_defaults(debug=False)
    parser.set_defaults(verbose=False)
    parser.set_defaults(netCDF=False)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument('--debug', action='store_true',
                          help='enable debug messages')
    optional.add_argument('--verbose', action='store_true',
                          help='enable verbose debug messages')
    optional.add_argument('--netcdf', action='store_true',
                          help='enable netCDF output file (IODA/JEDI Data Conventions)')
    # read in arguments to function call
    args = parser.parse_args()
       
    # verify time format
    try:
        target_time = datetime.fromisoformat(args.date_string[:-1])
    except Exception:
        parser.error('Date format invalid: ', args.date_string, ' must be like: 2022-05-18T12:00:00Z')
        sys.exit()

    if args.debug:
        logging.basicConfig(level=logging.INFO)
    elif args.verbose:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.ERROR)

# """
#     #---------------------------------------------------------------------------------------------
#     Options set. Ingest each file and concatenate.
#     #---------------------------------------------------------------------------------------------
# """

    # Loop through input files and concatenate into dataframe 
 #   ntotal = 0
    metaData_files = []
    file_cnt=0
    for file_name in args.file_names:
        # check if file exists
        if not os.path.isfile(file_name):
            parser.error(f'Input (-i option) file: {file_name} does not exist')
        if args.netcdf:
            AttrData['sourceFiles'] += ", " + file_name
        logging.debug(f"Reading input file: {file_name}")
    
        file = json.load(open(file_name))
        
        if file_cnt==0:
            # Create data frame to store each file's Meta Dataa
            dt_metaData_files = pd.DataFrame(columns= file.keys())
            dt_metaData_files.drop(['observations'], axis=1,inplace=True)
    
            # Create data frame to store all observation data
            df = pd.DataFrame(columns= file['observations'][0].keys())
            df.drop(['id','mission_id'], axis=1,inplace=True)
            df.rename(columns={'speed_x':'windEastward', 'speed_y':'windNorthward'},inplace=True)
    
        # Fill out each file's meta data
        dt_metaData_files.loc[file_cnt] = [file[key] for key in dt_metaData_files.keys()]
    
        # Pull each data type (variable) and create a list 
        altitude      = [file['observations'][ii]['altitude'] for ii in range(len(file['observations']))]  # relative to geod??? assume yes for now.
        humidity      = [file['observations'][ii]['humidity'] for ii in range(len(file['observations']))]
    #    id_inst       = [file['observations'][ii]['id'] for ii in range(len(file['observations']))]
        latitude      = [file['observations'][ii]['latitude'] for ii in range(len(file['observations']))]
        longitude     = [file['observations'][ii]['longitude'] for ii in range(len(file['observations']))]
    #    mission_id    = [file['observations'][ii]['mission_id'] for ii in range(len(file['observations']))]
        mission_name  = [file['observations'][ii]['mission_name'] for ii in range(len(file['observations']))]
        pressure      = [file['observations'][ii]['pressure'] for ii in range(len(file['observations']))]
        windEastward  = [file['observations'][ii]['speed_x'] for ii in range(len(file['observations']))]
        windNorthward = [file['observations'][ii]['speed_y'] for ii in range(len(file['observations']))]
        temperature   = [file['observations'][ii]['temperature'] for ii in range(len(file['observations']))]
        timestamp     = [file['observations'][ii]['timestamp'] for ii in range(len(file['observations']))] # datetime
    
        #pdb.set_trace()
    
        # Make a list of lists to feed into dataframe 
        data_lists = list(zip(altitude, humidity, latitude, longitude, mission_name, 
                              pressure, windEastward, windNorthward, temperature, timestamp )) 
    
        # All observation data for this file to append to the master dataframe 
        df2append = pd.DataFrame(data_lists,columns= df.keys()) 
    
        # Append to data frame containing all timestamp data 
        df = pd.concat([df,df2append], ignore_index=True)
        
        # count files
        file_cnt += 1 

    print(df.keys())
    print(df.head(20))
    print(dt_metaData_files)
    pdb.set_trace()