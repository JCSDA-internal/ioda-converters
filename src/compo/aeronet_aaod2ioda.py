#!/usr/bin/env python3

# Description:
#        This code reads ASCII files of AERONET inversion products
#        downloaded from from NASA websit
#        (https://aeronet.gsfc.nasa.gov/print_web_data_help_v3_inv_new.html)
#        and converts to IODA format.
#        Inversion products here include conicident AOT data with almucantar
#        retrieval (CAD), AOD aborption (TAB) for inversion types of ALM15 or ALM20
#        at wavelengths of 440/675/870/1020nm)
#
# Usage:
#        python aeronet_aaod2ioda.py -c 'testinput/aeronet_cad.dat'
#                                    -t 'testinput/aeronet_tab.dat'
#                                    -o aeronet_aaod.nc
#        -c: input file of AERONET inversion conicident AOT data with
#            almucantar retrieval (CAD)
#        -t: input file of AERONET inversion AOD aborption (TAB)
#        -o: output IODA file

import numpy as np
import netCDF4 as nc
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
                     skiprows=7,
                     parse_dates={'time': [1, 2]},
                     date_parser=dateparse,
                     na_values=-999)
    header = pd.read_csv(infile, skiprows=6, header=None,
                         nrows=1).values.flatten()

    cols = ['time']
    for i in header:
        if "Date(" in i or 'Time(' in i:
            if "Last_Processing_Date(" in i or "Last_Processing_Time(" in i:
                cols.append(i.lower())
            else:
                pass
        else:
            cols.append(i.lower())
    df.columns = cols
    df.index = df.time
    df.rename(columns={
        'latitude(degrees)': 'latitude',
        'longitude(degrees)': 'longitude',
        'elevation(m)': 'elevation',
        'aeronet_site': 'siteid'
    },
        inplace=True)
    return df


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description=(
            "Reads AERONET inversion files downloaded from NASA website "
            " and converts into IODA format")
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-c', '--incad',
        help="input file of AERONET inversion conicident AOT data "
             " with almucantar retrieval (CAD)",
        type=str, required=True)
    required.add_argument(
        '-t', '--intab',
        help="input file of AERONET inversion AOD aborption (TAB)",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path and filename of AERONET inversion IODA file",
        type=str, required=True)

    args = parser.parse_args()
    incad = args.incad
    intab = args.intab
    outfile = args.output

    # Read and extract online AERONET inversion data
    print('Read and extract AERONET inversion data: CAD, TAB')
    f3_cad_all = add_data(incad)
    f3_tab_all = add_data(intab)
    f3_cad = f3_cad_all[['time', 'siteid', 'longitude', 'latitude', 'elevation',
                         'if_retrieval_is_l2(without_l2_0.4_aod_440_threshold)',
                         'if_aod_is_l2', 'inversion_data_quality_level',
                         'aod_coincident_input[440nm]', 'aod_coincident_input[675nm]',
                         'aod_coincident_input[870nm]', 'aod_coincident_input[1020nm]']]
    f3_tab = f3_tab_all[['absorption_aod[440nm]', 'absorption_aod[675nm]',
                         'absorption_aod[870nm]', 'absorption_aod[1020nm]']]
    f3 = pd.concat([f3_cad, f3_tab], axis=1, join='inner')

    # Define wavelengths, channels and frequencies of AERONET inversion data
    aeronetinv_wav = np.array([440., 675, 870., 1020.], dtype=np.float32)
    aeronetinv_chan = np.array([3, 5, 6, 7], dtype=np.intc)
    speed_light = 2.99792458E8
    frequency = speed_light*1.0E9/aeronetinv_wav
    print('Output AERONET inverion data at wavelengths/channels/frequencies: ')
    print(aeronetinv_wav)
    print(aeronetinv_chan)
    print(frequency)

    long_missing_value = nc.default_fillvals['i8']

    nlocs, columns = f3.shape
    nchans = len(aeronetinv_chan)
    if nlocs == 0:
        print('No AERONET inversion data available in input files')
        exit(0)

    locationKeyList = [("latitude", "float"), ("longitude", "float"), ("datetime", "string")]
    varDict = defaultdict(lambda: defaultdict(dict))
    outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    obsvars = {'aerosolOpticalDepth': ['aod_coincident_input[440nm]', 'aod_coincident_input[675nm]',
                                       'aod_coincident_input[870nm]', 'aod_coincident_input[1020nm]'],
               'absorptionAerosolOpticalDepth': ['absorption_aod[440nm]', 'absorption_aod[675nm]',
                                                 'absorption_aod[870nm]', 'absorption_aod[1020nm]']}

    # A dictionary of global attributes.  More filled in further down.
    AttrData = {}
    AttrData['ioda_object_type'] = 'absorptionAOD'
    AttrData['sensor'] = 'aeronet'

    # A dictionary of variable dimensions.
    DimDict = {}

    # A dictionary of variable names and their dimensions.
    VarDims = {
        'aerosolOpticalDepth': ['Location', 'Channel'],
        'absorptionAerosolOpticalDepth': ['Location', 'Channel'],
        'sensorCentralFrequency': ['Channel'],
        'sensorChannelNumber': ['Channel']
    }

    # Get the group names we use the most.
    metaDataName = iconv.MetaDataName()
    obsValName = iconv.OvalName()
    obsErrName = iconv.OerrName()
    qcName = iconv.OqcName()

    # Define varDict variables
    for key, value in obsvars.items():
        varDict[key]['valKey'] = key, obsValName
        varDict[key]['errKey'] = key, obsErrName
        varDict[key]['qcKey'] = key, qcName
        varAttrs[key, obsValName]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[key, obsErrName]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[key, qcName]['coordinates'] = 'longitude latitude stationElevation'
        varAttrs[key, obsValName]['_FillValue'] = -9999.
        varAttrs[key, obsErrName]['_FillValue'] = -9999.
        varAttrs[key, qcName]['_FillValue'] = -9999
        varAttrs[key, obsValName]['units'] = '1'
        varAttrs[key, obsErrName]['units'] = '1'

    for key, value in obsvars.items():
        outdata[varDict[key]['valKey']] = np.array(np.float32(f3[value].fillna(np.float32(-9999.))))
        outdata[varDict[key]['qcKey']] = np.where(outdata[varDict[key]['valKey']] == np.float32(-9999.),
                                                  1, 0)
        if key in ["aerosolOpticalDepth"]:
            outdata[varDict[key]['errKey']] = np.where(outdata[varDict[key]['valKey']] == np.float32(-9999.),
                                                       np.float32(-9999.), np.float32(0.02))
        else:
            outdata[varDict[key]['errKey']] = np.full((nlocs, nchans), np.float32(-9999.))

    outdata[('latitude', metaDataName)] = np.array(np.float32(f3['latitude']))
    outdata[('longitude', metaDataName)] = np.array(np.float32(f3['longitude']))
    outdata[('stationElevation', metaDataName)] = np.array(np.float32(f3['elevation']))
    varAttrs[('stationElevation', metaDataName)]['units'] = 'm'

    # Whether Coincident_AOD440nm in aeronet_cad.txt reaches Level 2.0 (0: yes, 1: no)
    qcL2Aod = np.where(f3['if_aod_is_l2'] == 1, 0, 1)

    # aaod inversion type: 0 for ALM20 and 1 for ALM15
    qcL2Aaod = np.where(f3['inversion_data_quality_level'] == 'lev20', 0, 1)

    # Whether aaod reaches Level 2.0 without the threshold of aod440 >= 0.4 (0: yes, 1: no)
    qcL2Aaod2 = np.where(f3['if_retrieval_is_l2(without_l2_0.4_aod_440_threshold)'] == 1, 0, 1)

    qcAll = np.full(len(qcL2Aod), long_missing_value, dtype=np.int32)
    for i in range(len(qcL2Aod)):
        qc1 = qcL2Aod[i]
        qc2 = qcL2Aaod[i]
        qc3 = qcL2Aaod2[i]
        if qc1 == 0 and qc2 == 0 and qc3 == 0:
            # Both AOD and AAOD w/and w/o aod440>0.4 threshold reach L2.
            qcAll[i] = 0
        elif qc1 == 0 and qc2 == 0 and qc3 == 1:
            # Both AOD and AAOD w/ aod440>0.4 threshold reaches L2,
            # but AAOD w/o aod440>0.4 threshold does not.
            qcAll[i] = 1
        elif qc1 == 0 and qc2 == 1 and qc3 == 0:
            # Both AOD and AAOD w/o aod440>0.4 threshold reaches L2,
            # but AAOD w/ aod440>0.4 threshold does not.
            qcAll[i] = 2
        elif qc1 == 0 and qc2 == 1 and qc3 == 1:
            # Only AOD reaches L2.
            qcAll[i] = 3
        elif qc1 == 1 and qc2 == 0 and qc3 == 0:
            # AAOD w/ and w/o aod440>0.4 threshold reaches L2,
            # but AOD does not.
            qcAll[i] = 4
        elif qc1 == 1 and qc2 == 0 and qc3 == 1:
            # AAOD w/ aod440>0.4 threshold reaches L2,
            # but AOD and AAOD w/o aod440>0.4 threshold do not.
            qcAll[i] = 5
        elif qc1 == 1 and qc2 == 1 and qc3 == 0:
            # AAOD w/o  aod440>0.4 threshold reaches L2,
            # but AOD and AAOD w/ aod440>0.4 threshold do not.
            qcAll[i] = 6
        elif qc1 == 1 and qc2 == 1 and qc3 == 1:
            # Neither AOD or AAOD w/ and w/o aod440>0.4 threshold reaches L2.
            qcAll[i] = 7

    outdata[('qualityFlags', metaDataName)] = qcAll
    c = np.empty([nlocs], dtype=object)
    c[:] = np.array(f3.siteid)
    outdata[('stationIdentification', metaDataName)] = c

    d = np.empty([nlocs], dtype=object)
    for i in range(nlocs):
        d[i] = f3.time[i].strftime('%Y-%m-%dT%H:%M:%SZ')
    outdata[('dateTime', metaDataName)] = d

    outdata[('sensorCentralFrequency', metaDataName)] = np.float32(frequency)
    varAttrs[('sensorCentralFrequency', metaDataName)]['units'] = 'Hz'
    outdata[('sensorChannelNumber', metaDataName)] = np.int32(aeronetinv_chan)

    # Add global atrributes
    DimDict['Location'] = nlocs
    DimDict['Channel'] = aeronetinv_chan

    # Setup the IODA writer
    writer = iconv.IodaWriter(outfile, locationKeyList, DimDict)

    # Write out IODA NC files
    writer.BuildIoda(outdata, VarDims, varAttrs, AttrData)
