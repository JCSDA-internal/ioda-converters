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
#
# Contact:
#        Bo Huang (bo.huang@noaa.gov) from CU/CIRES and NOAA/ESRL/GSL
#        (August 9, 2021)
#
# Acknowledgement:
#        Barry Baker from ARL for his initial preparation for this code.
#

import netCDF4 as nc
import numpy as np
import inspect, sys, os, argparse
import pandas as pd
from datetime import datetime, timedelta
from builtins import object, str
from numpy import NaN
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
import meteo_utils
import ioda_conv_ncio as iconv
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

    # Define AOD varname that match with those in f3 (match aod_wav and aod_chan)
    outcols = ['time', 'siteid', 'longitude', 'latitude', 'elevation',
               'aod_340nm', 'aod_380nm', 'aod_440nm', 'aod_500nm', 'aod_675nm',
               'aod_870nm', 'aod_1020nm', 'aod_1640nm']
    obsvars = {'aerosol_optical_depth_1': 'aod_340nm', 'aerosol_optical_depth_2': 'aod_380nm',
               'aerosol_optical_depth_3': 'aod_440nm', 'aerosol_optical_depth_4': 'aod_675nm',
               'aerosol_optical_depth_5': 'aod_500nm', 'aerosol_optical_depth_6': 'aod_870nm',
               'aerosol_optical_depth_7': 'aod_1020nm', 'aerosol_optical_depth_8': 'aod_1640nm'}

    nlocs, columns = f3.shape
    if nlocs == 0:
        print('Zero AERONET AOD is available in file: ' + infile + ' and exit.')
        exit(0)

    locationKeyList = [("latitude", "float"), ("longitude", "float"), ("datetime", "string")]
    writer = iconv.NcWriter(outfile, locationKeyList)
    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    loc_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    var_mdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    units = {}
    units['latitude'] = 'degree'
    units['longitude'] = 'degree'
    units['station_elevation'] = 'm'

    # Define varDict variables
    print('Define varDict variables')
    for key, value in obsvars.items():
        print(key, value)
        varDict[key]['valKey'] = key, writer.OvalName()
        varDict[key]['errKey'] = key, writer.OerrName()
        varDict[key]['qcKey'] = key, writer.OqcName()

    # Define loc_mdata
    print('Define loc_mdata')
    loc_mdata['latitude'] = np.array(f3['latitude'])
    loc_mdata['longitude'] = np.array(f3['longitude'])
    loc_mdata['station_elevation'] = np.array(f3['elevation'])
    loc_mdata['surface_type'] = np.full((nlocs), 1)

    c = np.empty([nlocs], dtype='S50')
    c[:] = np.array(f3.siteid)
    loc_mdata['station_id'] = writer.FillNcVector(c, 'string')

    # Define datetime
    d = np.empty([nlocs], 'S20')
    for i in range(nlocs):
        d[i] = f3.time[i].strftime('%Y-%m-%dT%H:%M:%SZ')
    loc_mdata['datetime'] = writer.FillNcVector(d, 'datetime')

    # Define var_mdata
    print('Define var_mdata')
    var_mdata['frequency'] = writer.FillNcVector(frequency, 'float')
    var_mdata['sensor_channel'] = writer.FillNcVector(aod_chan, 'integer')

    for key, value in obsvars.items():
        outdata[varDict[key]['valKey']] = np.array(f3[value].fillna(nc.default_fillvals['f4']))
        outdata[varDict[key]['qcKey']] = np.where(outdata[varDict[key]['valKey']] == nc.default_fillvals['f4'],
                                                  1, 0)
        outdata[varDict[key]['errKey']] = np.where(outdata[varDict[key]['valKey']] == nc.default_fillvals['f4'],
                                                   nc.default_fillvals['f4'], 0.02)

    # Define global atrributes
    print('Define global atrributes')
    AttrData = {'observation_type': 'Aod',
                'sensor': "aeronet",
                'surface_type': 'ocean=0,land=1,costal=2'}

    # Write out IODA V1 NC files
    print('Write into IODA format file: ' + outfile)
    writer._nvars = len(aod_wav)
    writer._nlocs = nlocs
    writer.BuildNetcdf(outdata, loc_mdata, var_mdata, AttrData, units)
