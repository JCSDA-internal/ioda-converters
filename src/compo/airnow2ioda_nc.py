#!/usr/bin/env python3

# Read airnow text data file and convert to IODA netcdf
import os
from datetime import datetime
from pathlib import Path
import netCDF4 as nc
import numpy as np
import pandas as pd

import pyiodaconv.ioda_conv_engines as iconv
import pyiodaconv.ioda_conv_ncio as iconio
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

# Dictionary of output variables (ObsVal, ObsError, and PreQC).
# First is the incoming variable name followed by list of IODA outgoing name and units.

varDict = {'PM2.5': ['particulatematter2p5Surface', 'mg m-3'],
           'OZONE': ['ozoneSurface', 'ppmV'],
           'NO2': ['nitrogendioxideSurface', 'ppmV'],
           'CO': ['carbonmonoxideSurface', 'ppmV'],
           }

locationKeyList = [("latitude", "float", "degrees_north"),
                   ("longitude", "float", "degrees_east"),
                   ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z"),
                   ("stationElevation", "float", "m"),
                   ("height", "float", "m"),
                   ("stationIdentification", "string", ""),
                   #("surfaceQualifier", "integer", ""),
                   ]

meta_keys = [m_item[0] for m_item in locationKeyList]

GlobalAttrs = {'converter': os.path.basename(__file__),
               'ioda_version': 2,
               'description': 'AIRNow data (converted from text/csv to IODA',
               'source': 'Unknown (ftp)'}

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

def read_monitor_file(sitefile, isfull):

    if isfull:
        tmpdf = pd.read_csv(sitefile) 
        tmpdf = tmpdf[['stat_id', 'lat', 'lon', 'elevation', 'loc_setting']]
        tmpdf['loc_type'] = np.nan
        for n, loc_type in enumerate(['UNKNOWN', 'RURAL', 'SUBURBAN', 'URBAN AND CENTER CITY']):
            type_filter = tmpdf['loc_setting']==loc_type
            tmpdf.loc[type_filter,'loc_type'].loc[type_filter] = n
        airnow = tmpdf.rename(columns={'stat_id':'siteid', 'lat':'latitude', 'lon':'longitude'})
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
            'Reads single AIRNow text file '
            ' and converts into IODA formatted output files.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of AIRNow text input file",
        type=str, required=True)
    required.add_argument(
        '-s', '--sitefile',
        help="path of AIRNow site list file",
        type=str, required=True)
    required.add_argument(
        '--fulllist',
        help="define this if sitefile is a full list",
        action='store_true', default=False)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    args = parser.parse_args()
    print('infile=', args.input, args.sitefile)
    f = add_data(args.input, args.sitefile, args.fulllist).drop_duplicates(subset=['PM2.5', 'OZONE', 'NO2', 'CO', 'siteid', 'latitude', 'longitude'])
    print(f.shape)

    f3 = f.dropna(subset=['PM2.5'], how='any').reset_index()
    nlocs, columns = f3.shape
    print(f3.shape)

    dt = f3.time[1].to_pydatetime()
    time_offset = round((dt - epoch).total_seconds())

    if args.fulllist:
        locationKeyList.append(("surfaceQualifier", "integer", ""))
        meta_keys.append("surfaceQualifier")

    ioda_data = {}         # The final outputs.
    data = {}              # Before assigning the output types into the above.
    for key in varDict.keys():
        data[key] = []
    for key in meta_keys:
        data[key] = []

    # Fill the temporary data arrays from input file column data
    data['stationIdentification'] = np.full(nlocs, f3.siteid, dtype='S20')
    data['dateTime'] = np.full(nlocs, np.int64(time_offset))
    data['latitude'] = np.array(f3['latitude'])
    data['longitude'] = np.array(f3['longitude'])
    data['stationElevation'] = np.array(f3['elevation'])
    data['height'] = np.array(f3['elevation'])
    for n in range(nlocs):
        data['height'][n] = data['height'][n] + 10.0   # 10 meters above stationElevation

    if args.fulllist:
        data['surfaceQualifier'] = np.array(f3['loc_type'], dtype=np.int32)

    for n, key in enumerate(varDict.keys()):
        if key in ['PM2.5', 'CO']:
            data[varDict[key][0]] = np.array(f3[key].fillna(float_missing_value))
        elif key in ['OZONE', 'NO2']:
            data[varDict[key][0]] = np.array((f3[key]/1000).fillna(float_missing_value))

    DimDict = {'Location': nlocs}

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
