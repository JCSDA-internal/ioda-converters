#!/usr/bin/env python3

#
# (C) Copyright 2019-2023 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import argparse
from datetime import datetime, timedelta
import dateutil.parser
from concurrent.futures import ProcessPoolExecutor
import numpy as np
import os
from pathlib import Path
from itertools import repeat
import netCDF4 as nc

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

import h5py

# globals
ioda_int_type = 'int32'
ioda_float_type = 'float'
ioda_float32_type = np.float32
ioda_byte_type    ='bytes'
float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
long_missing_value = nc.default_fillvals['i8']

epoch = datetime.fromisoformat('1970-01-01T00:00:00')
#thistime = datetime.fromisoformat('1970-01-01T01:00:00')
#print(epoch)
#time_offset = round((thistime-epoch).total_seconds())
#print(time_offset)
#sys.exit()

locationKeyList = [
    ("latitude", 'float'),
    ("longitude", 'float'),
    ("datetime", "np.int64")
]

def main(args):

    args.date = datetime.strptime(args.date, '%Y%m%d%H')
    qc = args.qualitycontrol

    # read / process files in parallel
    pool_input_01 = args.input
    pool_input_02 = np.arange(len(args.input))+args.recordnumber
    pool_inputs = [[i, j] for i, j in zip(pool_input_01, pool_input_02)]
    obs_data = {}
    # create a thread pool -- multi-threading
    # normal serial loop
    record_number = args.recordnumber
    for input_file in args.input:
        file_obs_data = read_input([input_file, record_number],add_qc=qc)
        timeoff =  file_obs_data[('dateTime', 'MetaData')][0] - round((args.date-epoch).total_seconds())

        if not file_obs_data: 
            print("INFO: non-nominal file skipping")
            continue
        if timeoff  < -args.window*3600 or timeoff >= args.window*3600 :
           print("INFO: outside time window file skipping")
           continue 

        record_number += 1
        if obs_data:
            concat_obs_dict(obs_data, file_obs_data)
        else:
            obs_data = file_obs_data

    if len(obs_data) == 0:
        print('ERROR: no occultations to write out')
        sys.exit()

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs = {} 
    GlobalAttrs['datetimeReference'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    date_time_int32 = np.array(int(args.date.strftime("%Y%m%d%H")), dtype='int32')
    GlobalAttrs['converter'] = os.path.basename(__file__)
    GlobalAttrs['source data'] = 'https://registry.opendata.aws/gnss-ro-opendata'
 
   # pass parameters to the IODA writer
    VarDims = {
        'bendingAngle': ['Location'],
        'atmosphericRefractivity': ['Location']
    }

    # write them out
    nlocs = obs_data[('bendingAngle', 'ObsValue')].shape[0]
    DimDict = {'Location': nlocs}
    meta_data_types = def_meta_types()

    for k, v in meta_data_types.items():
        locationKeyList.append((k, v)) 

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('dateTime', 'MetaData')]['longname'] = 'seconds since 1970-01-01T00:00:00Z'
    VarAttrs[('latitude',  'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('latitude', 'MetaData')]['units'] = 'degree_north'
    VarAttrs[('longitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', 'MetaData')]['units'] = 'degree_east'

    VarAttrs[('geoidUndulation', 'MetaData')]['units'] = 'm'
    VarAttrs[('geoidUndulation', 'MetaData')]['longname'] = 'undulation height above WGS-84 ellipsoid'
    VarAttrs[('earthRadiusCurvature', 'MetaData')]['units'] = 'm'
    VarAttrs[('earthRadiusCurvature', 'MetaData')]['longname'] = 'Earth’s local radius of curvature' 

    VarAttrs[('sensorAzimuthAngle', 'MetaData')]['units'] = 'degree'
    VarAttrs[('sensorAzimuthAngle', 'MetaData')]['longname'] = 'The direction of the occultation ray, transmitter to receiver,\
             at the occultation tangent point,measured eastward from north'

    VarAttrs[('sequenceNumber', 'MetaData')]['longname'] = 'GNSS RO profile identifier'

    VarAttrs[('satelliteIdentifier', 'MetaData')]['longname'] = 'Low Earth Orbit satellite identifier, e.g., COSMIC2=750-755'
    VarAttrs[('satelliteTransmitterId', 'MetaData')]['longname'] = 'GNSS satellite transmitter identifier'
    VarAttrs[('satelliteAscendingFlag', 'MetaData')]['flag_values'] = '0,1'
    VarAttrs[('satelliteAscendingFlag', 'MetaData')]['flag_meanings'] = 'descending/ascending'
    VarAttrs[('satelliteAscendingFlag', 'MetaData')]['longname'] = 'the original occultation ascending/descending flag'

    VarAttrs[('dataProviderOrigin', 'MetaData')]['longname'] = 'originally data processing_center,e.g., 60=UCAR'
    VarAttrs[('dataProviderOrigin', 'MetaData')]['name_in_earlier_version'] = 'process_center'
    VarAttrs[('satelliteConstellationRO', 'MetaData')]['longname'] = 'GNSS satellite classification, e.g., 401=GPS'

    VarAttrs[('impactHeightRO', 'MetaData')]['units'] = 'meter'
    VarAttrs[('impactHeightRO', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('impactHeightRO', 'MetaData')]['valid_range'] = '0-200000'
    VarAttrs[('impactHeightRO', 'MetaData')]['longname'] = 'distance from mean sea level'
    VarAttrs[('impactParameterRO', 'MetaData')]['units'] = 'meter'
    VarAttrs[('impactParameterRO', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('impactParameterRO', 'MetaData')]['valid_range'] = '6200000-6600000'
    VarAttrs[('impactParameterRO', 'MetaData')]['longname'] = 'centre of curvature'

    VarAttrs[('bendingAngle', 'ObsValue')]['units'] = 'Radians'
    VarAttrs[('bendingAngle', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('bendingAngle', 'ObsValue')]['valid_range'] = '0-0.008'
    VarAttrs[('bendingAngle', 'ObsValue')]['longname'] = 'Bending Angle'

    VarAttrs[('height',  'MetaData')]['units'] = 'meter'
    VarAttrs[('height',  'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('height',  'MetaData')]['longname'] = 'Geometric altitude'

    VarAttrs[('atmosphericRefractivity', 'ObsValue')]['units'] = 'N'
    VarAttrs[('atmosphericRefractivity', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('atmosphericRefractivity', 'ObsValue')]['valid_range'] = '0-500'
    VarAttrs[('atmosphericRefractivity', 'ObsValue')]['longname'] = 'Atmospheric refractivity'

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)
    sys.exit()


def read_input(input_file_and_record, add_qc):
    """
    Reads/converts input file(s)

    Arguments:

        input_args: an input filename or names
            input_file: The name of file to read

    Returns:

        A dictionary holding the variables (obs_data) needed by the IODA writer
    """

    input_file = input_file_and_record[0]
    record_number = input_file_and_record[1]
    print("Reading: %s" % input_file)

    f = h5py.File(input_file, 'r')
    obs_data = get_opendata(f, add_qc, record_number=record_number)
    f.close()
    return obs_data

# assign satellite identifier  
def get_WMO_satellite_ID(filename):
    os.path.basename(filename)
    if 'kompsat' in filename:
        WMO_sat_ID = 825
    elif 'metopa' in filename:
        WMO_sat_ID = 4
    elif 'metopb' in filename:
        WMO_sat_ID = 3
    elif 'metopc' in filename:
        WMO_sat_ID = 5
    elif 'cosmic2e1' in filename:
        WMO_sat_ID = 750
    elif 'cosmic2e2' in filename:
        WMO_sat_ID = 751
    elif 'cosmic2e3' in filename:
        WMO_sat_ID = 752
    elif 'cosmic2e4' in filename:
        WMO_sat_ID = 753
    elif 'cosmic2e5' in filename:
        WMO_sat_ID = 754
    elif 'cosmic2e6' in filename:
        WMO_sat_ID = 755
    elif 'cosmic1e1' in filename:
        WMO_sat_ID = 740
    elif 'cosmic1e2' in filename:
        WMO_sat_ID = 741
    elif 'cosmic1e3' in filename:
        WMO_sat_ID = 742
    elif 'cosmic1e4' in filename:
        WMO_sat_ID = 743
    elif 'cosmic1e5' in filename:
        WMO_sat_ID = 744
    elif 'cosmic1e6' in filename:
        WMO_sat_ID = 745
    else:
        print('unknown satellite id')
        sys.exit() 
    return WMO_sat_ID

# assign processing center originate
def get_WMO_processcenter_ID(pid):
    if pid.lower() == 'ucar':
       WMO_processcenter_ID=60 
    elif pid.lower() == 'dmi':
       WMO_processcenter_ID=94
    elif pid.lower() == 'gfz' or pid.lower() == 'dwd':
       WMO_processcenter_ID=78
    elif pid.lower() == 'romsaf' or pid.lower() == 'eumetsat':
       WMO_processcenter_ID=254
    else:
        print('unknown process center id')
        sys.exit() 
    return WMO_processcenter_ID

# assign GNSS satellite constellation
def get_GNSS_satellite_class_ID(pid):
    if pid.upper() == 'G':
       satellite_class_ID=401
    elif pid.upper() == 'R':
       satellite_class_ID=402
    else:
        print('unknown  GNSS satellite class')
        sys.exit()
    return satellite_class_ID

def get_opendata(f, add_qc, record_number=1):

    profile_meta_data = get_meta_opendata(f)
    obs_data={}
    lats = np.array(f['latitude'], dtype=ioda_float_type)
    lons = np.array(f['longitude'], dtype=ioda_float_type)
    local_nlocs = len(lats)
    azim = np.array(f['orientation'], dtype=ioda_float_type)
    impact = np.array(f['impactParameter'], dtype=ioda_float_type)
    altitu = np.array(f['altitude'], dtype=ioda_float_type)
    setting = np.array(f['setting'], dtype=ioda_int_type)
    bang = np.array(f['bendingAngle'], dtype=ioda_float_type)
    ref = np.array(f['refractivity'], dtype=ioda_float_type)
    obs_data[('latitude', 'MetaData')] = np.float32(assign_values(lats))
    obs_data[('longitude', 'MetaData')] = np.float32(assign_values(lons))
    obs_data[('height', 'MetaData')] = np.float32(assign_values(altitu))
    obs_data[('impactParameterRO', "MetaData")] = np.float32(assign_values(impact))
    obs_data[('sensorAzimuthAngle', "MetaData")] = np.float32(assign_values(azim))
    obs_data[('bendingAngle', "ObsValue")] = np.float32(assign_values(bang))
    obs_data[('atmosphericRefractivity', "ObsValue")] = np.float32(assign_values(ref))

#   switch ascending/descending values following IODA defination
    if setting == 1:
        obs_data[('satelliteAscendingFlag', "MetaData")] = np.array(np.repeat(0, local_nlocs), dtype=ioda_int_type)
    elif setting == 0: 
        obs_data[('satelliteAscendingFlag', "MetaData")] = np.array(np.repeat(1, local_nlocs), dtype=ioda_int_type)
    else:
        obs_data[('satelliteAscendingFlag', "MetaData")] = np.array(np.repeat(int_missing_value, local_nlocs), dtype=ioda_int_type)


    meta_data_types = def_meta_types()

    for k, v in profile_meta_data.items():
        if isinstance(v, np.int64):
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, local_nlocs), dtype=np.int64)
        elif isinstance(v, int):
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, local_nlocs), dtype=ioda_int_type)
        elif v.dtype.kind == 'f':
             obs_data[(k, 'MetaData')] = np.array(np.repeat(v, local_nlocs), dtype=ioda_float32_type)
        else:
            print('do not know type')
            sys.exit() 

    # Compute impact height
    obs_data[('impactHeightRO', 'MetaData')] = \
        obs_data[('impactParameterRO', 'MetaData')] - \
        obs_data[('geoidUndulation', 'MetaData')] - \
        obs_data[('earthRadiusCurvature', 'MetaData')]

    # set record number (multi file procesing will change this)
    if record_number is None:
        nrec = 1
    else:
        nrec = record_number
    obs_data[('sequenceNumber', 'MetaData')] = np.array(np.repeat(nrec, local_nlocs), dtype=ioda_int_type)

    if add_qc:
        good = quality_control(profile_meta_data, altitu, lats, lons)
        if len(lats[good]) == 0: 
            return{}
            # exit if entire profile is missing
        for k in obs_data.keys():
            obs_data[k] = obs_data[k][good]
    return obs_data


def get_meta_opendata(f):

    # get some of the global attributes that we are interested in
    meta_data_keys = def_meta_opendata()

    # these are the MetaData we are interested in
    profile_meta_data = {}
    for k, v in meta_data_keys.items():
        profile_meta_data[k] = np.array(f[v],dtype=ioda_float_type)

    year = f.attrs['year'][0] 
    month = f.attrs['month'][0]
    day = f.attrs['day'][0]
    hour = f.attrs['hour'][0]
    minute = f.attrs['minute'][0]
    second = f.attrs['second'][0]

    dtg = ("%4i-%.2i-%.2iT%.2i:%.2i:%.2iZ" % (year, month, day, hour, minute, second))
    this_datetime = datetime.strptime(dtg, "%Y-%m-%dT%H:%M:%SZ")
    time_offset = round((this_datetime - epoch).total_seconds())
    profile_meta_data['dateTime'] = np.int64(time_offset)
    WMO_sat_ID = get_WMO_satellite_ID(f.filename)
    profile_meta_data['satelliteIdentifier'] = WMO_sat_ID
    refGnss = int(f.attrs['occGnss'].decode()[1-2])
    profile_meta_data['satelliteTransmitterId'] = refGnss
    pcenter = get_WMO_processcenter_ID(f.attrs['processing_center'].decode())
    profile_meta_data['dataProviderOrigin'] = pcenter
    satGnssclass = get_GNSS_satellite_class_ID(f.attrs['occGnss'].decode()[0])
    profile_meta_data['satelliteConstellationRO'] = satGnssclass
    return profile_meta_data
   

def quality_control(profile_meta_data, heights, lats, lons):
    print('Performing QC Checks')

    good = (heights > 0.) & (heights < 100000.) & (abs(lats) <= 90.) & (abs(lons) <= 360.)
    # bad radius or large geoid undulation
    if (profile_meta_data['earthRadiusCurvature'] > 6450000.) or (profile_meta_data['earthRadiusCurvature'] < 6250000.) or \
       (abs(profile_meta_data['geoidUndulation']) > 200): 
        good = []
    return good


def def_meta_opendata():

    meta_data_keys = {
        "geoidUndulation": 'undulation',
        "earthRadiusCurvature": 'radiusOfCurvature',
    }
    return meta_data_keys

def def_meta_types():

    meta_data_types = {
        "geoidUndulation": 'float',
        "earthRadiusCurvature": 'float',
    }
    return meta_data_types


def get_normalized_bit(value, bit_index):
    return(value >> bit_index) & 1


def assign_values(data):
    if data.dtype == float:
        data[np.abs(data) >= np.abs(float_missing_value)] = float_missing_value
        return np.array(data, dtype=ioda_float_type)
    elif data.dtype == int:
        data[np.abs(data) >= np.abs(int_missing_value)] = int_missing_value
        return np.array(data, dtype=ioda_int_type)


def concat_obs_dict(obs_data, append_obs_data):
    # For now we are assuming that the obs_data dictionary has the "golden" list
    # of variables. If one is missing from append_obs_data, the obs_data variable
    # will be extended using fill values.
    #
    # Use the first key in the append_obs_data dictionary to determine how
    # long to make the fill value vector.
    append_keys = list(append_obs_data.keys())
    append_length = len(append_obs_data[append_keys[0]])
    for gv_key in obs_data.keys():
        if gv_key in append_keys:
            obs_data[gv_key] = np.append(obs_data[gv_key], append_obs_data[gv_key])
        else:
            if obs_data[gv_key].dtype == float:
                fill_data = np.repeat(float_missing_value, append_length, dtype=ioda_float_type)
            elif obs_data[gv_key].dtype == int:
                fill_data = np.repeat(int_missing_value, append_length, dtype=ioda_int_type)
            elif obs_data[gv_key].dtype == np.int64:
                fill_data = np.repeat(long_missing_value, append_length, dtype=np.int64)
            elif obs_data[gv_key].dtype == object:
                # string type, extend with empty strings
                fill_data = np.repeat("", append_length, dtype=object)
            obs_data[gv_key] = np.append(obs_data[gv_key], fill_data)


if __name__ == "__main__":

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the GNSS-RO data in netcdf format from AWS GNSSRO open data'
            'avaialble at https://registry.opendata.aws/gnss-ro-opendata/'
            'convert into IODA formatted output files. '
            'Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of NETCDF GNSS-RO observation input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="full path and name of IODA output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             '(default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '-r', '--recordnumber',
        help=' optional record number to associate with profile ',
        type=int, default=1)

    optional.add_argument(
        '-q', '--qualitycontrol',
        help='turn on quality control georeality checks',
        default=False, action='store_true', required=False)

    optional.add_argument(
        '-w', '--window',
        default=3, required=False)

    args = parser.parse_args()
    main(args)
