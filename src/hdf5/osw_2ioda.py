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
from datetime import datetime, timezone
import os.path
import sys
import pandas as pd

import h5py
import numpy as np

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import int_missing_value, long_missing_value, float_missing_value
from collections import defaultdict

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

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
obsvars = ['windSpeed']
obsvars_units = ['m s-1']
# obserrlist = [1.2]
obsvars_dtype = ['float']

# Assign dimensions to the obs values
VarDims = {
    'windSpeed': ['Location'],
}

# creating data types
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

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

    # Loop through input files and concatenate into dataframe
    file_cnt = 0
    for file_name in args.file_names:
        # check if file exists
        if not os.path.isfile(file_name):
            logging.debug(f'Input (-i option) file: {file_name} does not exist')
            print(f'Input (-i option) file: {file_name} does not exist')
            sys.exit()
        logging.debug(f"Reading input file: {file_name}")

        # Open file
        # (need to move file and dat ref into get_data_from_file, but then need to move adjust data_append inside also.)
        file = h5py.File(file_name, 'r')

        # Figure out what source the file is coming from (e.g. cygnss, muon, spire)
        osw_source = get_data_source(file)

        # Get reference time and convert to epoch time
        dat_ref = get_reference_time(file, osw_source)

        if 'obs_data' not in locals():
            # initialize the DF
            obs_data = pd.DataFrame(columns=meta_keys+obsvars)

        # Get data from file to append to obs_data dataframe
        obs_data_append = get_data_from_file(file, obs_data.keys(), osw_source, file_name)

        # Convert variables
        # Change time reference
        obs_data_append = adjust_dateTime(obs_data_append, dat_ref)

        # Change longitude range
        obs_data_append = adjust_longitude(obs_data_append, osw_source)

        # Append to data frame containing all timestamp data
        obs_data = pd.concat([obs_data, obs_data_append], ignore_index=True)

        file.close()
        # count files
        file_cnt += 1

    # Run gross qc on the variables
    obs_data = quality_control(obs_data)

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
#        ioda_data[(iodavar, obsErrName)] = np.full(ntotal, obserrlist[n], dtype=np.float32)
        ioda_data[(iodavar, qcName)] = np.full(ntotal, 2, dtype=np.int32)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output_file, MetaDataKeyList, DimDict)
    # write everything out
    writer.BuildIoda(ioda_data, VarDims, varAttrs, GlobalAttrs)


def get_data_source(afile):
    if 'title' in afile.attrs.keys() and 'CYGNSS' in afile.attrs['title'].decode('UTF-8'):
        return 'CYGNSS'
    elif 'title' in afile.attrs.keys() and 'Muon' in afile.attrs['title'].decode('UTF-8'):
        return 'Muon'
    elif 'constellation' in afile.attrs.keys() and "spire" in afile.attrs['constellation'].decode('UTF-8'):
        return 'Spire'


def get_reference_time(afile, osw_source):
    if osw_source in ('CYGNSS'):
        dat_ref = afile['sample_time'].attrs['units'].decode('UTF-8').split('since ')[-1]
        dat_ref = datetime.strptime(dat_ref, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc).timestamp()
    elif osw_source in ('Muon'):
        # note same as CYGNSS except item key is simply time
        dat_ref = afile['time'].attrs['units'].decode('UTF-8').split('since ')[-1]
        dat_ref = datetime.strptime(dat_ref, '%Y-%m-%d %H:%M:%S').replace(tzinfo=timezone.utc).timestamp()
    elif osw_source in ('Spire'):
        # the precision of the seconds appears troublesome when too many digits
        # take the floor of the seconds by stripping of fractional seconds
        dat_ref = afile['sample_time'].attrs['units'].decode('UTF-8').split()[-1]
        dat_ref = dat_ref.split('.')[0]
        dat_ref = datetime.strptime(dat_ref, '%Y-%m-%dT%H:%M:%S').replace(tzinfo=timezone.utc).timestamp()
    return dat_ref


def get_data_from_file(afile, col_names, osw_source, file_name):

    # Pull each data type (variable) and create a list
    if osw_source == 'CYGNSS':
        latitude = [afile['lat'][ii] for ii in range(len(afile['lat']))]
        longitude = [afile['lon'][ii] for ii in range(len(afile['lon']))]
        dateTime = [int(afile['sample_time'][ii]) for ii in range(len(afile['sample_time']))]  # datetime with different ref time
        windSpeed = [afile['wind_speed'][ii] for ii in range(len(afile['wind_speed']))]
        sensorIdentification = [str(afile['sv_num'][ii]) for ii in range(len(afile['sv_num']))]  # sv_num is the GPS space vehicle number
    elif osw_source == 'Muon':
        # Get instrument reference
        import re
        subst = 'CY..._G..'
        temp = re.compile(subst)
        res = temp.search(file_name)
        instrument_ref = res.group(0)

        latitude = [afile['lat'][ii] for ii in range(len(afile['lat']))]
        longitude = [afile['lon'][ii] for ii in range(len(afile['lon']))]
        dateTime = [int(afile['time'][ii]) for ii in range(len(afile['time']))]  # datetime with different ref time
        windSpeed = [afile['wind_speed_level2'][ii] for ii in range(len(afile['wind_speed_level2']))]
        sensorIdentification = [instrument_ref]*len(latitude)
    elif osw_source == 'Spire':
        # Get instrument reference
        instrument_ref = afile.attrs['tx_id'].decode('UTF-8')

        latitude = [afile['sp_lat'][ii] for ii in range(len(afile['sp_lat']))]
        longitude = [afile['sp_lon'][ii] for ii in range(len(afile['sp_lon']))]
        dateTime = [int(afile['sample_time'][ii]) for ii in range(len(afile['sample_time']))]  # datetime with different ref time
        windSpeed = [afile['wind'][ii] for ii in range(len(afile['wind']))]
        sensorIdentification = [instrument_ref]*len(latitude)

    # Make a column to have a constant elevation for the "station"
    height = [10]*len(dateTime)

    # Make a list of lists to feed into dataframe
    data_lists = list(zip(latitude, longitude, dateTime, sensorIdentification,
                          height, windSpeed))

    # All observation data for this file to append to the master dataframe
    obs_data_append = pd.DataFrame(data_lists, columns=col_names)
    return obs_data_append


def adjust_dateTime(obs_DF, dat_ref):
    obs_DF['dateTime'] = obs_DF['dateTime']+dat_ref
    return obs_DF


def adjust_longitude(obs_DF, osw_source):
    if osw_source in ('CYGNSS', 'Spire'):
        mask = obs_DF['longitude'] > 180
        obs_DF.loc[mask, 'longitude'] = obs_DF['longitude'].loc[mask].values - 360
    return obs_DF


def add_long_description(osw_source):

    # Add some additional information to platformLongDescription global attribute
    long_description = ''
    if osw_source == 'CYGNSS':
        long_description = 'Derived from CYGNSS GNSS-R recievers.'
    elif osw_source == 'Muon':
        long_description = 'Proxy Muon data derived from CYGNSS GNSS-R recievers.'
    elif osw_source == 'Spire':
        long_description = 'Derived from Spire GNSS-R recievers.'
    return long_description


def quality_control(obs_data):
    # Apply initial QC for physically possible values (wind, lat, lon) need to add swells
    wind_range = [0.0, 75.0]
    lat_range = [-90, 90]
    lon_range = [-180, 180]
    # Replace with None to be filled with missing value later
    obs_data.loc[((obs_data['windSpeed'] < wind_range[0]) | (obs_data['windSpeed'] > wind_range[1])), 'windSpeed'] = None
    obs_data.loc[((obs_data['latitude'] < lat_range[0]) | (obs_data['latitude'] > lat_range[1])), 'latitude'] = None
    obs_data.loc[((obs_data['longitude'] < lon_range[0]) | (obs_data['longitude'] > lon_range[1])), 'longitude'] = None

    return obs_data


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
