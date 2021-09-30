#!/usr/bin/env python3
# read airnow data and convert to netcdf
import netCDF4 as nc
import numpy as np
import inspect, os, sys, argparse
import pandas as pd
from datetime import datetime
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
import meteo_utils
import ioda_conv_ncio as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict


def read_monitor_file(sitefile=None):

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


def add_data(infile, sitefile):
    df = pd.read_csv(infile, delimiter='|',
                     header=None,
                     error_bad_lines=False,
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

    monitor_df = read_monitor_file(sitefile)
    df = pd.merge(df, monitor_df, on='siteid')
    df.drop_duplicates(inplace=True)
    df = filter_bad_values(df)
    return long_to_wide(df)


if __name__ == '__main__':

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
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    args = parser.parse_args()
    print('infile=', args.input, args.sitefile)
    f = add_data(args.input, args.sitefile)

    f3 = f.dropna(subset=['OZONE', 'PM2.5'], how='all')
    nlocs, columns = f3.shape

    obsvars = {'pm25_tot': 'pm25_tot', 'o3': 'o3', }
    AttrData = {'converter': os.path.basename(__file__), }

    locationKeyList = [("latitude", "float"), ("longitude", "float"),
                       ("station_elevation", "float"), ("height", "float"), ("station_id", "string"),
                       ("datetime", "string")]

    writer = iconv.NcWriter(args.output, locationKeyList)

    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    units = {}
    units['pm25_tot'] = 'microgram/m3'
    units['o3'] = 'ppmV'

    for i in ['pm25_tot', 'o3']:
        varDict[i]['valKey'] = i, writer.OvalName()
        varDict[i]['errKey'] = i, writer.OerrName()
        varDict[i]['qcKey'] = i, writer.OqcName()

    d = np.empty([nlocs], 'S20')
    d[:] = f3.time[1].strftime('%Y-%m-%dT%H:%M:%SZ')
    loc_mdata['datetime'] = writer.FillNcVector(d, 'datetime')
    loc_mdata['latitude'] = np.array(f3['latitude'])
    loc_mdata['longitude'] = np.array(f3['longitude'])
    loc_mdata['height'] = np.full((nlocs), 10.)
    loc_mdata['station_elevation'] = np.full((nlocs), 10.)

    c = np.empty([nlocs], dtype=str)
    c[:] = np.array(f3.siteid)
    loc_mdata['station_id'] = writer.FillNcVector(c, 'string')

    outdata[varDict['pm25_tot']['valKey']] = np.array(f3['PM2.5'].fillna(nc.default_fillvals['f4']))
    outdata[varDict['o3']['valKey']] = np.array((f3['OZONE']/1000).fillna(nc.default_fillvals['f4']))
    for i in ['pm25_tot', 'o3']:
        outdata[varDict[i]['errKey']] = np.full((nlocs), 0.1)
        outdata[varDict[i]['qcKey']] = np.full((nlocs), 0)

    writer._nvars = 2
    writer._nlocs = nlocs
    writer.BuildNetcdf(outdata, loc_mdata, var_mdata, AttrData, units)
