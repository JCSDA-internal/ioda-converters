#!/usr/bin/env python3

# Description:
#        This code reads an AERONET AOD ASCII file downloaded from
#        from NASA website and writes AOD  at wavelengths
#        (340/380/440/500/675/870/1020/1640 nm) into IODA format.
#
# Usage:
#        python aeronet_aod2ioda.py -i aeronet_aod.dat 6 -o aeronet_aod.nc
#        -i: input AOD file path
#        -o: output file path

import numpy as np
import inspect, sys, os, argparse
import pandas as pd
from datetime import datetime, timedelta
from builtins import str
from numpy import NaN
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import meteo_utils
import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict


def dateparse(x):
    return datetime.strptime(x, '%d:%m:%Y %H:%M:%S')


def add_data(infile):
    df = pd.read_csv(infile,
                     engine='python',
                     header=None,
                     skiprows=6,
                     parse_dates={'time': [1, 2]},
                     date_parser=dateparse,
                     na_values=-999)
    header = pd.read_csv(infile, skiprows=5, header=None,
                         nrows=1).values.flatten()
    cols = ['time']
    for i in header:
        if "Date(" in i or 'Time(' in i:
            pass
        else:
            cols.append(i.lower())
    df.columns = cols
    df.rename(columns={
        'site_latitude(degrees)': 'latitude',
        'site_longitude(degrees)': 'longitude',
        'site_elevation(m)': 'elevation',
        'aeronet_site': 'siteid'
    },
        inplace=True)
    df.dropna(subset=['latitude', 'longitude'], inplace=True)
    df.dropna(axis=1, how='all', inplace=True)
    return df


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description=(
            'Reads AERONET AOD ASCII file downloaded from NASA website '
            ' and converts into IODA format')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of AERONET AOD input ASCII file",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of AERONET AOD IODA file",
        type=str, required=True)

    args = parser.parse_args()
    infile = args.input
    outfile = args.output

    # Read AERONET AOD from input file
    f3 = add_data(infile)

    # Define AOD wavelengths, channels and frequencies
    aod_wav = np.array([340., 380., 440., 500., 675, 870., 1020., 1640.], dtype=np.float32)
    aod_chan = np.array([1, 2, 3, 4, 5, 6, 7, 8], dtype=np.intc)
    speed_light = 2.99792458E8
    frequency = speed_light*1.0E9/aod_wav
    print('Output AERONET AOD at wavelengths/channels/frequencies: ')
    print(aod_wav)
    print(aod_chan)
    print(frequency)

    # Add obs data
    nlocs, columns = f3.shape
    nchans = len(aod_chan)
    if nlocs == 0:
        print('Zero AERONET AOD is available in file: ' + infile + ' and exit.')
        exit(0)

    locationKeyList = [("latitude", "float"), ("longitude", "float"), ("datetime", "string")]
    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    obsvars = {'aerosol_optical_depth': ['aod_340nm', 'aod_380nm',
                                         'aod_440nm', 'aod_675nm',
                                         'aod_500nm', 'aod_870nm',
                                         'aod_1020nm', 'aod_1640nm']}

    AttrData = {
        'converter': os.path.basename(__file__),
    }

    DimDict = {
    }

    VarDims = {
        'aerosol_optical_depth': ['nlocs', 'nchans'],
        'frequency': ['nchans'],
        'sensor_channel': ['nchans']
    }

    for key, value in obsvars.items():
        varDict[key]['valKey'] = key, iconv.OvalName()
        varAttrs[key, iconv.OvalName()]['_FillValue'] = -999.
        varAttrs[key, iconv.OvalName()]['coordinates'] = 'longitude latitude station_elevation'
        varAttrs[key, iconv.OvalName()]['units'] = '1'
        varDict[key]['errKey'] = key, iconv.OerrName()
        varAttrs[key, iconv.OerrName()]['_FillValue'] = -999.
        varAttrs[key, iconv.OerrName()]['units'] = '1'
        varAttrs[key, iconv.OerrName()]['coordinates'] = 'longitude latitude station_elevation'
        varDict[key]['qcKey'] = key, iconv.OqcName()
        varAttrs[key, iconv.OqcName()]['_FillValue'] = -999
        varAttrs[key, iconv.OqcName()]['coordinates'] = 'longitude latitude station_elevation'
        varAttrs[key, iconv.OqcName()]['units'] = 'unitless'

    for key, value in obsvars.items():
        outdata[varDict[key]['valKey']] = np.array(np.float32(f3[value].fillna(np.float32(-999.))))
        outdata[varDict[key]['qcKey']] = np.where(outdata[varDict[key]['valKey']] == np.float32(-999.),
                                                  1, 0)
        outdata[varDict[key]['errKey']] = np.where(outdata[varDict[key]['valKey']] == np.float32(-999.),
                                                   np.float32(-999.), np.float32(0.02))

    # Add metadata variables
    outdata[('latitude', 'MetaData')] = np.array(np.float32(f3['latitude']))
    outdata[('longitude', 'MetaData')] = np.array(np.float32(f3['longitude']))
    outdata[('station_elevation', 'MetaData')] = np.array(np.float32(f3['elevation']))
    varAttrs[('station_elevation', 'MetaData')]['units'] = 'm'
    outdata[('surface_type', 'MetaData')] = np.full((nlocs), 1)
    varAttrs[('surface_type', 'MetaData')]['units'] = 'unitless'

    c = np.empty([nlocs], dtype=object)
    c[:] = np.array(f3.siteid)
    outdata[('station_id', 'MetaData')] = c
    varAttrs[('station_id', 'MetaData')]['units'] = ''

    d = np.empty([nlocs], dtype=object)
    for i in range(nlocs):
        d[i] = f3.time[i].strftime('%Y-%m-%dT%H:%M:%SZ')
    outdata[('datetime', 'MetaData')] = d
    varAttrs[('datetime', 'MetaData')]['units'] = ''

    outdata[('frequency', 'MetaData')] = np.float32(frequency)
    varAttrs[('frequency', 'MetaData')]['units'] = 'Hz'
    outdata[('sensor_channel', 'MetaData')] = np.int32(aod_chan)
    varAttrs[('sensor_channel', 'MetaData')]['units'] = 'unitless'

    # Add global atrributes
    DimDict['nlocs'] = nlocs
    DimDict['nchans'] = aod_chan
    AttrData['nlocs'] = np.int32(DimDict['nlocs'])
    AttrData['nchans'] = np.int32(nchans)
    AttrData['observation_type'] = 'AOD'
    AttrData['sensor'] = 'aeronet'
    AttrData['surface_type'] = 'ocean=0,land=1,costal=2'

    # Setup the IODA writer
    writer = iconv.IodaWriter(outfile, locationKeyList, DimDict)

    # Write out IODA NC files
    writer.BuildIoda(outdata, VarDims, varAttrs, AttrData)
