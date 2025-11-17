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
import h5py
import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import concat_obs_dict

# globals
ioda_int_type = 'int32'
ioda_float_type = 'float'
ioda_float32_type = np.float32
float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
long_missing_value = nc.default_fillvals['i8']

epoch = datetime.fromisoformat('1970-01-01T00:00:00')

locationKeyList = [
    ("latitude", 'float'),
    ("longitude", 'float'),
    ("dateTime", 'np.int64')
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
        file_obs_data = read_input([input_file, record_number], add_qc=qc)
        timeoff = file_obs_data[('dateTime', 'MetaData')][0] - round((args.date-epoch).total_seconds())

        if not file_obs_data:
            print("INFO: non-nominal file skipping")
            continue
        if timeoff < -args.window*3600 or timeoff >= args.window*3600:
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
    GlobalAttrs['totalProfiles'] = str(record_number-1)
    GlobalAttrs['datetimeReference'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    date_time_int32 = np.array(int(args.date.strftime("%Y%m%d%H")), dtype='int32')
    GlobalAttrs['converter'] = os.path.basename(__file__)
    GlobalAttrs['sourceData'] = 'https://registry.opendata.aws/gnss-ro-opendata'

    # pass parameters to the IODA writer
    VarDims = {
        'bendingAngle': ['Location']
    }

    # write them out
    nlocs = np.int32(obs_data[('bendingAngle', 'ObsValue')].shape[0])
    DimDict = {'Location': nlocs}
    meta_data_types = def_meta_types()

    for k, v in meta_data_types.items():
        locationKeyList.append((k, v))

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('dateTime', 'MetaData')]['longname'] = 'seconds since 1970-01-01T00:00:00Z'
    VarAttrs[('dateTime', 'MetaData')]['units'] = 'seconds since 1970-01-01T00:00:00Z'
    VarAttrs[('latitude', 'MetaData')]['_FillValue'] = float_missing_value
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
    VarAttrs[('sequenceNumber', 'MetaData')]['units'] = '1'

    VarAttrs[('satelliteIdentifier', 'MetaData')]['longname'] = 'Low Earth Orbit satellite identifier, e.g., COSMIC2=750-755'
    VarAttrs[('satelliteIdentifier', 'MetaData')]['units'] = '1'

    VarAttrs[('satelliteTransmitterId', 'MetaData')]['longname'] = 'GNSS satellite transmitter identifier'
    VarAttrs[('satelliteTransmitterId', 'MetaData')]['units'] = '1'

    VarAttrs[('satelliteAscendingFlag', 'MetaData')]['flag_values'] = '0,1'
    VarAttrs[('satelliteAscendingFlag', 'MetaData')]['flag_meanings'] = 'descending/ascending'
    VarAttrs[('satelliteAscendingFlag', 'MetaData')]['longname'] = 'the original occultation ascending/descending flag'
    VarAttrs[('satelliteAscendingFlag', 'MetaData')]['units'] = '1'

    VarAttrs[('dataProviderOrigin', 'MetaData')]['longname'] = 'originally data processing_center,e.g., 60=UCAR'
    VarAttrs[('dataProviderOrigin', 'MetaData')]['name_in_earlier_version'] = 'process_center'
    VarAttrs[('dataProviderOrigin', 'MetaData')]['units'] = '1'

    VarAttrs[('satelliteConstellationRO', 'MetaData')]['longname'] = 'GNSS satellite classification, e.g., 401=GPS'
    VarAttrs[('satelliteConstellationRO', 'MetaData')]['units'] = '1'

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
def get_WMO_satellite_ID(leo):
    if 'kompsat' in leo:
        WMO_sat_ID = 825
    elif 'metopa' in leo:
        WMO_sat_ID = 4
    elif 'metopb' in leo:
        WMO_sat_ID = 3
    elif 'metopc' in leo:
        WMO_sat_ID = 5
    elif 'cosmic2e1' in leo:
        WMO_sat_ID = 750
    elif 'cosmic2e2' in leo:
        WMO_sat_ID = 751
    elif 'cosmic2e3' in leo:
        WMO_sat_ID = 752
    elif 'cosmic2e4' in leo:
        WMO_sat_ID = 753
    elif 'cosmic2e5' in leo:
        WMO_sat_ID = 754
    elif 'cosmic2e6' in leo:
        WMO_sat_ID = 755
    elif 'cosmic1e1' in leo:
        WMO_sat_ID = 740
    elif 'cosmic1e2' in leo:
        WMO_sat_ID = 741
    elif 'cosmic1e3' in leo:
        WMO_sat_ID = 742
    elif 'cosmic1e4' in leo:
        WMO_sat_ID = 743
    elif 'cosmic1e5' in leo:
        WMO_sat_ID = 744
    elif 'cosmic1e6' in leo:
        WMO_sat_ID = 745
    else:
        print('unknown satellite id')
        sys.exit()
    return WMO_sat_ID


# assign processing center originate
def get_WMO_processcenter_ID(pid):
    if pid.lower() == 'ucar':
        WMO_processcenter_ID = 60
    elif pid.lower() == 'dmi':
        WMO_processcenter_ID = 94
    elif pid.lower() == 'gfz' or pid.lower() == 'dwd':
        WMO_processcenter_ID = 78
    elif pid.lower() == 'romsaf' or pid.lower() == 'eumetsat':
        WMO_processcenter_ID = 254
    else:
        print('unknown process center id')
        sys.exit()
    return WMO_processcenter_ID


# assign GNSS satellite constellation
def get_GNSS_satellite_class_ID(pid):
    if pid.upper() == 'G':
        satellite_class_ID = 401
    elif pid.upper() == 'R':
        satellite_class_ID = 402
    else:
        print('unknown  GNSS satellite class')
        sys.exit()
    return satellite_class_ID


def get_opendata(f, add_qc, record_number=1):

    profile_meta_data = get_meta_opendata(f)
    obs_data = {}

    if f['impactParameter'][0] > f['impactParameter'][-1]:
        lats_full = np.array(f['latitude'], dtype=ioda_float_type)[::-1]
        lons_full = np.array(f['longitude'], dtype=ioda_float_type)[::-1]
        azim_full = np.array(f['orientation'], dtype=ioda_float_type)[::-1]
        impact_full = np.array(f['impactParameter'], dtype=ioda_float_type)[::-1]
        bang_full = np.array(f['bendingAngle'], dtype=ioda_float_type)[::-1]

    else:
        lats_full = np.array(f['latitude'], dtype=ioda_float_type)
        lons_full = np.array(f['longitude'], dtype=ioda_float_type)
        azim_full = np.array(f['orientation'], dtype=ioda_float_type)
        impact_full = np.array(f['impactParameter'], dtype=ioda_float_type)
        bang_full = np.array(f['bendingAngle'], dtype=ioda_float_type)

    setting = np.array(f['setting'], dtype=ioda_int_type)

    # Compute impact height
    impact_height_full = \
        f['impactParameter'] - \
        profile_meta_data['geoidUndulation'] - \
        profile_meta_data['earthRadiusCurvature']

    impact_height_full = impact_height_full[::-1]
    used = np.zeros(len(impact_height_full))

    impact_height = []
    impact = []
    lats = []
    lons = []
    azim = []
    bang = []

    interval = 120
    layer_height = impact_height_full[0] + interval
    step = 180/(40000 - impact_height_full[0])
    ind = 0
    while layer_height <= 40000:
        diffs = abs(impact_height_full - layer_height)
        min_loc = np.nanargmin(diffs)
        if used[min_loc] == 1:
            min_loc += 1

        impact_height.append(impact_height_full[min_loc])
        impact.append(impact_full[min_loc])
        bang.append(bang_full[min_loc])
        lats.append(lats_full[min_loc])
        lons.append(lons_full[min_loc])
        azim.append(azim_full[min_loc])

        used[:min_loc] = 1
        ind += 1
        interval += ind*step
        layer_height = impact_height[ind-1] + interval

    while layer_height <= 60000:
        diffs = abs(impact_height_full - layer_height)
        min_loc = np.nanargmin(diffs)
        if used[min_loc] == 1:
            min_loc += 1

        impact_height.append(impact_height_full[min_loc])
        impact.append(impact_full[min_loc])
        bang.append(bang_full[min_loc])
        lats.append(lats_full[min_loc])
        lons.append(lons_full[min_loc])
        azim.append(azim_full[min_loc])

        used[:min_loc] = 1
        ind += 1
        layer_height = impact_height[ind-1] + interval

    obs_data[('impactHeightRO', 'MetaData')] = np.float32(assign_values(np.asarray(impact_height, dtype=ioda_float_type)))
    obs_data[('latitude', 'MetaData')] = np.float32(assign_values(np.asarray(lats, dtype=ioda_float_type)))
    obs_data[('longitude', 'MetaData')] = np.float32(assign_values(np.asarray(lons, dtype=ioda_float_type)))
    obs_data[('impactParameterRO', "MetaData")] = np.float32(assign_values(np.asarray(impact, dtype=ioda_float_type)))
    obs_data[('sensorAzimuthAngle', "MetaData")] = np.float32(assign_values(np.asarray(azim, dtype=ioda_float_type)))
    obs_data[('bendingAngle', "ObsValue")] = np.float32(assign_values(np.asarray(bang, dtype=ioda_float_type)))

    local_nlocs = len(lats)
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

    # set record number (multi file procesing will change this)
    if record_number is None:
        nrec = 1
    else:
        nrec = record_number
    obs_data[('sequenceNumber', 'MetaData')] = np.array(np.repeat(nrec, local_nlocs), dtype=ioda_int_type)

    if add_qc:
        good = quality_control(profile_meta_data, np.array(impact_height, float), np.array(lats, float), np.array(lons, float))
        if len(good) == 0:
            return {}
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
        profile_meta_data[k] = np.array(f[v], dtype=ioda_float_type)

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
    WMO_sat_ID = get_WMO_satellite_ID(f.attrs['leo'].decode())
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
    return (value >> bit_index) & 1


def assign_values(data):
    if data.dtype == float:
        data[np.abs(data) >= np.abs(float_missing_value)] = float_missing_value
        return np.array(data, dtype=ioda_float_type)
    elif data.dtype == int:
        data[np.abs(data) >= np.abs(int_missing_value)] = int_missing_value
        return np.array(data, dtype=ioda_int_type)


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
