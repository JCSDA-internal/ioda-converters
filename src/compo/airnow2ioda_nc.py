#!/usr/bin/env python3

#
# (C) Copyright 2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Read airnow text data file and convert to IODA netcdf

import os, sys
from datetime import datetime
from pathlib import Path
import netCDF4 as nc
import numpy as np
import pandas as pd

import pyiodaconv.ioda_conv_engines as iconv
import pyiodaconv.ioda_conv_ncio as iconio
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

os.environ["TZ"] = "UTC"

# Dictionary of output variables (ObsVal, ObsError, and PreQC).
# First is the incoming variable name followed by list of IODA outgoing name and units.

varDict = {'PM2.5': ['particulatematter2p5Surface', 'mg m-3'],
           'OZONE': ['ozoneSurface', 'ppmV'],
           'NO2': ['nitrogendioxideSurface', 'ppmV'],
           'CO': ['carbonmonoxideSurface', 'ppmV'],
           'SO2': ['sulfurdioxideSurface', 'ppmV'],
           }

locationKeyList = [("latitude", "float", "degrees_north"),
                   ("longitude", "float", "degrees_east"),
                   ("dateTime", "long", iso8601_string),
                   ("stationElevation", "float", "m"),
                   ("height", "float", "m"),
                   ("stationIdentification", "string", ""),
                   ]

meta_keys = [m_item[0] for m_item in locationKeyList]

GlobalAttrs = {'converter': os.path.basename(__file__),
               'ioda_version': 2,
               'description': 'AIRNow data (converted from text/csv to IODA',
               'source': 'https://files.airnowtech.org/?prefix=airnow/',
               'sourceFiles': '',
               }

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


def read_monitor_file(sitefile, is_epa):
    if is_epa:
        tmpdf = pd.read_csv(sitefile)
        tmpdf = tmpdf[['stat_id', 'lat', 'lon', 'elevation', 'loc_setting']]
        tmpdf['loc_type'] = np.nan
        for n, loc_type in enumerate(['UNKNOWN', 'RURAL', 'SUBURBAN', 'URBAN AND CENTER CITY']):
            type_filter = (tmpdf['loc_setting'] == loc_type)
            tmpdf.loc[type_filter, 'loc_type'].loc[type_filter] = n
        airnow = tmpdf.rename(columns={'stat_id': 'siteid', 'lat': 'latitude', 'lon': 'longitude'})
    else:
        colsinuse = [0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21]
        airnow = pd.read_csv(sitefile, delimiter='|', header=None,
                             usecols=colsinuse, dtype={0: str}, encoding="ISO-8859-1")
        airnow.columns = [
            'siteid', 'Site_Code', 'Site_Name', 'Status', 'Agency',
            'Agency_Name', 'EPA_region', 'latitude', 'longitude', 'Elevation',
            'GMT_Offset', 'Country_Code', 'CMSA_Code', 'CMSA_Name', 'MSA_Code',
            'MSA_Name', 'state_Code', 'state_Name', 'County_Code',
            'County_Name', 'City_Code']

    airnow['airnow_flag'] = 'AIRNOW'
    airnow.columns = [i.lower() for i in airnow.columns]
    return airnow


def filter_bad_values(df):
    """Short summary.

    Returns
    -------
    type
        Description of returned object.

    """

    df.loc[(df.obs > 3000) | (df.obs < 0), 'obs'] = np.NaN
    return df


def long_to_wide(df):
    from pandas import Series, merge
    w = df.pivot_table(
        values='obs', index=['time', 'siteid'],
        columns='variable').reset_index()
    cols = Series(df.columns)
    g = df.groupby('variable')
    for name, group in g:
        w[name + '_unit'] = group.units.unique()[0]

    return merge(w, df, on=['siteid', 'time'])


def add_data(infile, sitefile, isfull):
    df = pd.read_csv(infile, delimiter='|',
                     header=None,
                     on_bad_lines='warn',
                     encoding='ISO-8859-1')
    cols = ['date', 'time', 'siteid', 'site', 'utcoffset', 'variable', 'units',
            'obs', 'source']
    df.columns = cols
    df['obs'] = df.obs.astype(float)
    df['siteid'] = df.siteid.str.zfill(9)
    df['utcoffset'] = df.utcoffset.astype(int)
    df['time'] = pd.to_datetime(df.date + ' ' + df.time,
                                format='%m/%d/%y %H:%M',
                                exact=True)
    df.drop(['date'], axis=1, inplace=True)
    df['time_local'] = df.time + pd.to_timedelta(df.utcoffset, unit='H')

    monitor_df = read_monitor_file(sitefile, isfull)
    df = pd.merge(df, monitor_df, on='siteid')
    df.drop_duplicates(inplace=True)
    df = filter_bad_values(df)
    return long_to_wide(df)


if __name__ == '__main__':

    import argparse

    parser = argparse.ArgumentParser(
        description=(
            'Reads multiple AIRNow text files '
            ' and converts into IODA formatted output files.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of AIRNow text input file",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-s', '--sitefile',
        help="path of AIRNow site list file",
        type=str, required=True)
    required.add_argument(
        '--epa_list',
        help="define this if sitefile contains land use information from EPA",
        action='store_true', default=False)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-r', '--time_range',
        help="extract a date range to fit the data assimilation window"
        "format -r YYYYMMDDHH YYYYMMDDHH",
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('1970010100', '2170010100'))

    args = parser.parse_args()

    if args.epa_list:
        locationKeyList.append(("airqualityClassification", "integer", ""))
        meta_keys.append("airqualityClassification")

    print('sitefile=', args.sitefile)

    # Initialze local data array
    data = {}
    for key in varDict.keys():
        data[key] = []
        if key in ['PM2.5', 'OZONE', 'NO2', 'CO', 'SO2']:
            data[varDict[key][0]] = []
    for key in meta_keys:
        data[key] = []

    # date range to fit DA window
    date_start = datetime.strptime(args.time_range[0], "%Y%m%d%H")
    date_end = datetime.strptime(args.time_range[1], "%Y%m%d%H")

    total_locs = 0
    for infile in args.input:
        print('Processing infile=', infile)

        f = add_data(infile, args.sitefile, args.epa_list).drop_duplicates(
            subset=['PM2.5', 'OZONE', 'NO2', 'CO', 'SO2', 'siteid', 'latitude', 'longitude'])
        f2 = f.dropna(subset=['PM2.5'], how='any').reset_index()

        time_filter = (f2['time'] >= date_start) & (f2['time'] <= date_end)

        if not any(time_filter):
            print(f'  Skipping {infile}: no data within the time range')
            continue

        f3 = f2.loc[time_filter, :]

        nlocs, columns = f3.shape

        f3['time'] = f3['time'].dt.tz_localize('UTC')
        f3['time'] = f3['time'].dt.to_pydatetime()
        time_offset = round((f3['time'] - epoch).dt.total_seconds())

        # Fill the temporary data arrays from input file column data
        data['stationIdentification'] = np.append(data['stationIdentification'],
                                                  np.full(nlocs, f3.siteid, dtype='S20'))
        data['dateTime'] = np.append(data['dateTime'], np.int64(time_offset))
        data['latitude'] = np.append(data['latitude'], np.array(f3['latitude']))
        data['longitude'] = np.append(data['longitude'], np.array(f3['longitude']))
        data['stationElevation'] = np.append(data['stationElevation'], np.array(f3['elevation']))
        data['height'] = np.append(data['height'], np.array(f3['elevation']) + 10.0)

        if args.epa_list:
            data['airqualityClassification'] = np.append(data['airqualityClassification'],
                                                         np.array(f3['loc_type'], dtype=np.int32))

        GlobalAttrs['sourceFiles'] += str(infile.split('/')[-1]) + ", "

        for key in varDict.keys():
            if key in ['PM2.5', 'CO']:
                data[varDict[key][0]] = np.append(data[varDict[key][0]],
                                                  np.array(f3[key].fillna(float_missing_value)))
            elif key in ['OZONE', 'NO2', 'SO2']:
                data[varDict[key][0]] = np.append(data[varDict[key][0]],
                                                  np.array((f3[key]/1000).fillna(float_missing_value)))

        total_locs += nlocs

    ioda_data = {}         # The final outputs.
    DimDict = {'Location': total_locs}

    varDims = {}
    for key in varDict.keys():
        variable = varDict[key][0]
        varDims[variable] = ['Location']

    # Set units of the MetaData variables and all _FillValues.
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
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
        ioda_data[(variable, obsValName)] = np.array(data[variable], dtype=np.float32)
        ioda_data[(variable, obsErrName)] = np.full(nlocs, 0.1, dtype=np.float32)
        ioda_data[(variable, qcName)] = np.full(nlocs, 2, dtype=np.int32)

    # setup the IODA writer and write everything out.
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(ioda_data, varDims, varAttrs, GlobalAttrs)
