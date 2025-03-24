#!/usr/bin/env python3

# Description:
#        This code reads an AERONET AOD ASCII file downloaded from
#        from NASA website and writes AOD  at wavelengths
#        (340/380/440/500/675/870/1020/1640 nm) into IODA format.
#        AERONET input is downloaded with
#        wget --no-check-certificate -q -O $aeronet_aod \
#             "https://aeronet.gsfc.nasa.gov/cgi-bin/print_web_data_v3?year=2000&month=6&day=1&hour=9&year2=2000&month2=6&day2=1&hour2=15&AOD20=1&AVG=10&if_no_html=1"
#
# Usage:
#        python aeronet_aod2ioda.py -i aeronet_aod.dat 6 -o aeronet_aod.nc
#        -i: input AOD file path
#        -o: output file path

import numpy as np
import os, argparse
import pandas as pd
from datetime import datetime
from builtins import str

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import iso8601_string, epoch

float_missing_value = iconv.get_default_fill_val(np.float32)
double_missing_value = iconv.get_default_fill_val(np.float64)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)
string_missing_value = iconv.get_default_fill_val(np.str_)

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
    return df


if __name__ == '__main__':
    # Get the group names we use the most.
    metaDataName = iconv.MetaDataName()
    obsValName = iconv.OvalName()
    obsErrName = iconv.OerrName()
    qcName = iconv.OqcName()

    locationKeyList = [
        ("latitude", "float", "degrees_north"),
        ("longitude", "float", "degrees_east"),
        ("dateTime", "long", iso8601_string),
        ("stationElevation", "float", "m"),
        ("stationIdentification", "string", None),
        ("sensorCentralFrequency", "float", "Hz"),
    ]

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

    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    # Setup MetaData _FillValue and units
    meta_keys = [m_item[0] for m_item in locationKeyList]
    for key in meta_keys:
        dtypestr = locationKeyList[meta_keys.index(key)][1]
        if locationKeyList[meta_keys.index(key)][2]:
            varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]

    obsvars = {'aerosolOpticalDepth': ['aod_340nm', 'aod_380nm',
                                       'aod_440nm', 'aod_675nm',
                                       'aod_500nm', 'aod_870nm',
                                       'aod_1020nm', 'aod_1640nm']}

    # A dictionary of global attributes.  More filled in further down.
    AttrData = {}
    AttrData['ioda_object_type'] = 'AOD'
    AttrData['sensor'] = 'aeronet'

    # A dictionary of variable dimensions.
    DimDict = {}

    # A dictionary of variable names and their dimensions.
    VarDims = {
        'aerosolOpticalDepth': ["Location", "Channel"],
        'sensorCentralFrequency': ['Channel'],
        'sensorChannelNumber': ['Channel']
    }

    for key, value in obsvars.items():
        varDict[key]['valKey'] = key, obsValName
        varDict[key]['errKey'] = key, obsErrName
        varDict[key]['qcKey'] = key, qcName
        varAttrs[key, obsValName]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[key, obsErrName]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[key, qcName]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[key, obsValName]['_FillValue'] = float_missing_value
        varAttrs[key, obsErrName]['_FillValue'] = float_missing_value
        varAttrs[key, qcName]['_FillValue'] = int_missing_value
        varAttrs[key, obsValName]['units'] = '1'
        varAttrs[key, obsErrName]['units'] = '1'

    for key, value in obsvars.items():
        outdata[varDict[key]['valKey']] = np.array(np.float32(f3[value].fillna(float_missing_value)))
        outdata[varDict[key]['qcKey']] = np.int32(np.where(outdata[varDict[key]['valKey']] == float_missing_value,
                                                           1, 0))
        outdata[varDict[key]['errKey']] = np.float32(np.where(outdata[varDict[key]['valKey']] == float_missing_value,
                                                              float_missing_value, np.float32(0.02)))

    # Add metadata variables
    outdata[('latitude', metaDataName)] = np.array(np.float32(f3['latitude']))
    outdata[('longitude', metaDataName)] = np.array(np.float32(f3['longitude']))
    outdata[('stationElevation', metaDataName)] = np.array(np.float32(f3['elevation']))

    c = np.empty([nlocs], dtype=object)
    c[:] = np.array(f3.siteid)
    outdata[('stationIdentification', metaDataName)] = c

    f3['time'] = f3['time'].dt.tz_localize('UTC')
    f3['time'] = f3['time'].dt.to_pydatetime()
    time_offset = round((f3['time'] - epoch).dt.total_seconds())
    outdata[('dateTime', metaDataName)] = np.int64(time_offset)

    outdata[('sensorCentralFrequency', metaDataName)] = np.float32(frequency)
    outdata[('sensorChannelNumber', metaDataName)] = np.int32(aod_chan)

    # Add global atrributes
    DimDict['Location'] = nlocs
    DimDict['Channel'] = aod_chan

    # Setup the IODA writer
    writer = iconv.IodaWriter(outfile, locationKeyList, DimDict)

    # Write out IODA NC files
    writer.BuildIoda(outdata, VarDims, varAttrs, AttrData)
