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
#
# Contact:
#        Bo Huang (bo.huang@noaa.gov) from CU/CIRES and NOAA/ESRL/GSL
#        (September 28, 2021)
#
# Acknowledgement:
#        Barry Baker from ARL for his initial preparation for this code.


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

    nlocs, columns = f3.shape
    if nlocs == 0:
        print('No AERONET inversion data available in input files')
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

    # Define AERONET inversion varname that match with those in f3
    # (e.g., match aaeronetinv_wav and aeronetinv_chan)
    obsvars = {'aerosol_optical_depth_3': 'aod_coincident_input[440nm]',
               'aerosol_optical_depth_5': 'aod_coincident_input[675nm]',
               'aerosol_optical_depth_6': 'aod_coincident_input[870nm]',
               'aerosol_optical_depth_7': 'aod_coincident_input[1020nm]',
               'absorption_aerosol_optical_depth_3': 'absorption_aod[440nm]',
               'absorption_aerosol_optical_depth_5': 'absorption_aod[675nm]',
               'absorption_aerosol_optical_depth_6': 'absorption_aod[870nm]',
               'absorption_aerosol_optical_depth_7': 'absorption_aod[1020nm]'}

    # Define varDict variables
    for key, value in obsvars.items():
        varDict[key]['valKey'] = key, writer.OvalName()
        varDict[key]['errKey'] = key, writer.OerrName()
        varDict[key]['qcKey'] = key, writer.OqcName()

    # Define loc_mdata
    loc_mdata['latitude'] = np.array(f3['latitude'])
    loc_mdata['longitude'] = np.array(f3['longitude'])
    loc_mdata['station_elevation'] = np.array(f3['elevation'])
    loc_mdata['surface_type'] = np.full((nlocs), 1)

    # Whether aaod reaches Level 2.0 without the threshold of aod440 >= 0.4 (0: yes, 1: no)
    loc_mdata['aaod_l2_qc_without_aod440_le_0.4_threshold'] = np.where(f3['if_retrieval_is_l2(without_l2_0.4_aod_440_threshold)'] == 1, 0, 1)

    # Whether Coincident_AOD440nm in aeronet_cad.txt reaches Level 2.0 (0: yes, 1: no)
    loc_mdata['aod_l2_qc'] = np.where(f3['if_aod_is_l2'] == 1, 0, 1)

    # aaod inversion type: 0 for ALM20 and 1 for ALM15
    loc_mdata['aaod_l2_qc'] = np.where(f3['inversion_data_quality_level'] == 'lev20', 0, 1)

    c = np.empty([nlocs], dtype='S50')
    c[:] = np.array(f3.siteid)
    loc_mdata['station_id'] = writer.FillNcVector(c, 'string')

    # Define datetime
    d = np.empty([nlocs], 'S20')
    for i in range(nlocs):
        d[i] = f3.time[i].strftime('%Y-%m-%dT%H:%M:%SZ')
    loc_mdata['datetime'] = writer.FillNcVector(d, 'datetime')

    # Define var_mdata
    var_mdata['frequency'] = writer.FillNcVector(frequency, 'float')
    var_mdata['sensor_channel'] = writer.FillNcVector(aeronetinv_chan, 'integer')

    for key, value in obsvars.items():
        outdata[varDict[key]['valKey']] = np.array(f3[value].fillna(nc.default_fillvals['f4']))
        outdata[varDict[key]['qcKey']] = np.where(outdata[varDict[key]['valKey']] == nc.default_fillvals['f4'],
                                                  1, 0)
        if key in ["aerosol_optical_depth_3", "aerosol_optical_depth_5", "aerosol_optical_depth_6", "aerosol_optical_depth_7"]:
            outdata[varDict[key]['errKey']] = np.where(outdata[varDict[key]['valKey']] == nc.default_fillvals['f4'],
                                                       nc.default_fillvals['f4'], 0.02)
        else:
            outdata[varDict[key]['errKey']] = np.array(nc.default_fillvals['f4'], dtype=np.float32)

    # Define global atrributes
    AttrData = {'observation_type': 'AERONET AAOD',
                'sensor': "aeronet",
                'surface_type': 'ocean=0, land=1, costal=2'}

    # Write out IODA V1 NC files
    writer._nvars = len(aeronetinv_wav)
    writer._nlocs = nlocs
    writer.BuildNetcdf(outdata, loc_mdata, var_mdata, AttrData, units)
