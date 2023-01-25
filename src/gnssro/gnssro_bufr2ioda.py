#!/usr/bin/env python3

#
# (C) Copyright 2019-2021 UCAR
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
from itertools import repeat
import netCDF4 as nc

import lib_python.ioda_conv_engines as iconv
from lib_python.orddicts import DefaultOrderedDict

from eccodes import *

# globals
ioda_float_type = 'float32'
ioda_int_type = 'int32'
float_missing_value = nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'

iso8601_string = 'seconds since 1970-01-01T00:00:00Z'
epoch = datetime.fromisoformat(iso8601_string[14:-1])

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]


def main(args):

    dtg = datetime.strptime(args.date, '%Y%m%d%H')
    qc = args.qualitycontrol

    # read / process files in parallel
    pool_input_01 = args.input
    pool_input_02 = np.arange(len(args.input))+args.recordnumber
    pool_inputs = [[i, j] for i, j in zip(pool_input_01, pool_input_02)]
    obs_data = {}
    # create a thread pool
    with ProcessPoolExecutor(max_workers=args.threads) as executor:
        for file_obs_data in executor.map(read_input, pool_inputs, repeat(qc)):
            if not file_obs_data:
                print(f"INFO: non-nominal file skipping")
                continue
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
    GlobalAttrs['date_time_string'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    date_time_int32 = np.array(int(dtg.strftime("%Y%m%d%H")), dtype='int32')
    GlobalAttrs['date_time'] = date_time_int32.item()
    GlobalAttrs['converter'] = os.path.basename(__file__)

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
    VarAttrs[('bendingAngle', 'ObsValue')]['units'] = 'Radians'
    VarAttrs[('bendingAngle', 'ObsError')]['units'] = 'Radians'
    VarAttrs[('atmosphericRefractivity', 'ObsValue')]['units'] = 'N units'
    VarAttrs[('atmosphericRefractivity', 'ObsError')]['units'] = 'N units'
    VarAttrs[('height', 'MetaData')]['units'] = 'm'
    VarAttrs[('latitude', 'MetaData')]['units'] = 'degree_north'
    VarAttrs[('longitude', 'MetaData')]['units'] = 'degree_east'
    VarAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string
    VarAttrs[('sensorAzimuthAngle', 'MetaData')]['units'] = 'degree'
    VarAttrs[('geoidUndulation', 'MetaData')]['units'] = 'm'
    VarAttrs[('earthRadiusCurvature', 'MetaData')]['units'] = 'm'

    VarAttrs[('bendingAngle', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('bendingAngle', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('bendingAngle', 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[('atmosphericRefractivity', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('atmosphericRefractivity', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('atmosphericRefractivity', 'PreQC')]['_FillValue'] = int_missing_value

    VarAttrs[('latitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('height', 'MetaData')]['_FillValue'] = float_missing_value

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


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
    f = open(input_file, 'rb')
    bufr = codes_bufr_new_from_file(f)
    codes_set(bufr, 'unpack', 1)

    profile_meta_data = get_meta_data(bufr)

    obs_data = get_obs_data(bufr, profile_meta_data, add_qc, record_number=record_number)

    return obs_data


def get_meta_data(bufr):

    # get some of the global attributes that we are interested in
    meta_data_keys = def_meta_data()

    # these are the MetaData we are interested in
    profile_meta_data = {}
    for k, v in meta_data_keys.items():
        profile_meta_data[k] = codes_get(bufr, v)

    # do the hokey time structure to time structure
    year = codes_get(bufr, 'year')
    month = codes_get(bufr, 'month')
    day = codes_get(bufr, 'day')
    hour = codes_get(bufr, 'hour')
    minute = codes_get(bufr, 'minute')
    second = codes_get(bufr, 'second')  # non-integer value
    second = round(second)

    # get string date, translate to a datetime object, then offset from epoch
    dtg = ("%4i-%.2i-%.2iT%.2i:%.2i:%.2iZ" % (year, month, day, hour, minute, second))
    this_datetime = datetime.strptime(dtg, "%Y-%m-%dT%H:%M:%SZ")
    time_offset = round((this_datetime - epoch).total_seconds())
    profile_meta_data['dateTime'] = np.int64(time_offset)

    return profile_meta_data


def get_obs_data(bufr, profile_meta_data, add_qc, record_number=None):

    # allocate space for output depending on which variables are to be saved
    obs_data = {}

    # replication factors for the datasets, bending angle, refractivity and derived profiles
    krepfac = codes_get_array(bufr, 'extendedDelayedDescriptorReplicationFactor')
    # array([247, 247, 200])

    drepfac = codes_get_array(bufr, 'delayedDescriptorReplicationFactor')
    # L1, L2, combined -- only care about combined
    if drepfac[0] == 1:
        offset = 0
    elif drepfac[0] == 3:
        offset = 2
    else:
        raise NotImplementedError(f"expect repeat factor to be either 3 (L1, L2, and combined) or 1 (combined): {drepfac[0]}")

    # sequence is either 1 or 3 *(freq,impact,bendang,first-ord stat, bendang error, first-ord sat)
    #  note the label bendingAngle is used for both the value and its error !!!
    # get the bending angle
    lats = codes_get_array(bufr, 'latitude')[1:]                     # geolocation -- first value is the average
    lons = codes_get_array(bufr, 'longitude')[1:]
    impact = codes_get_array(bufr, 'impactParameter')[offset::drepfac[0]]
    bang = codes_get_array(bufr, 'bendingAngle')[offset*2::drepfac[0]*2]
    bang_err = codes_get_array(bufr, 'bendingAngle')[offset*2+1::drepfac[0]*2]
    bang_conf = codes_get_array(bufr, 'percentConfidence')[1:krepfac[0]+1]
    # len (bang) Out[19]: 1482  (krepfac * 6) -or- (krepfac * drepfac * 2 )`

    # bits are in reverse order according to WMO GNSSRO bufr documentation
    # ! Bit 1=Non-nominal quality
    # ! Bit 3=Rising Occulation (1=rising; 0=setting)
    # ! Bit 4=Excess Phase non-nominal
    # ! Bit 5=Bending Angle non-nominal
    i_non_nominal = get_normalized_bit(profile_meta_data['qualityFlags'], bit_index=16-1)
    i_phase_non_nominal = get_normalized_bit(profile_meta_data['qualityFlags'], bit_index=16-4)
    i_bang_non_nominal = get_normalized_bit(profile_meta_data['qualityFlags'], bit_index=16-5)
    iasc = get_normalized_bit(profile_meta_data['qualityFlags'], bit_index=16-3)
    # add rising/setting (ascending/descending) bit
    obs_data[('satelliteAscendingFlag', 'MetaData')] = np.array(np.repeat(iasc, krepfac[0]), dtype=ioda_int_type)

    # print( " ... RO QC flags: %i  %i  %i  %i" % (i_non_nominal, i_phase_non_nominal, i_bang_non_nominal, iasc) )

    # exit if non-nominal profile
    if i_non_nominal != 0 or i_phase_non_nominal != 0 or i_bang_non_nominal != 0:
        return {}

    # value, ob_error, qc
    obs_data[('bendingAngle', "ObsValue")] = assign_values(bang)
    obs_data[('bendingAngle', "ObsError")] = assign_values(bang_err)
    obs_data[('bendingAngle', "PreQC")] = np.full(krepfac[0], 0, dtype=ioda_int_type)

    # (geometric) height is read as integer but expected as float in output
    height = codes_get_array(bufr, 'height', ktype=float)

    # get the refractivity
    refrac = codes_get_array(bufr, 'atmosphericRefractivity')[0::2]
    refrac_err = codes_get_array(bufr, 'atmosphericRefractivity')[1::2]
    refrac_conf = codes_get_array(bufr, 'percentConfidence')[sum(krepfac[:1])+1:sum(krepfac[:2])+1]

    # value, ob_error, qc
    obs_data[('atmosphericRefractivity', "ObsValue")] = assign_values(refrac)
    obs_data[('atmosphericRefractivity', "ObsError")] = assign_values(refrac_err)
    obs_data[('atmosphericRefractivity', "PreQC")] = np.full(krepfac[0], 0, dtype=ioda_int_type)

    meta_data_types = def_meta_types()

    obs_data[('latitude', 'MetaData')] = assign_values(lats)
    obs_data[('longitude', 'MetaData')] = assign_values(lons)
    obs_data[('impactParameterRO', 'MetaData')] = assign_values(impact)
    obs_data[('height', 'MetaData')] = assign_values(height)
    for k, v in profile_meta_data.items():
        if type(v) is np.int64:
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, krepfac[0]), dtype=np.int64)
        elif type(v) is int:
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, krepfac[0]), dtype=ioda_int_type)
        elif type(v) is float:
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, krepfac[0]), dtype=ioda_float_type)
        else:  # something else (what do we do with it)
            print(f"Found neither float nor in, type={type(v)}; skipping")

    # set record number (multi file procesing will change this)
    if record_number is None:
        nrec = 1
    else:
        nrec = record_number
    obs_data[('sequenceNumber', 'MetaData')] = np.array(np.repeat(nrec, krepfac[0]), dtype=ioda_int_type)

    # get derived profiles
    geop = codes_get_array(bufr, 'geopotentialHeight')[:-1]
    pres = codes_get_array(bufr, 'nonCoordinatePressure')[0:-2:2]
    temp = codes_get_array(bufr, 'airTemperature')[0::2]
    spchum = codes_get_array(bufr, 'specificHumidity')[0::2]
    prof_conf = codes_get_array(bufr, 'percentConfidence')[sum(krepfac[:2])+1:sum(krepfac)+1]

    # Compute impact height
    obs_data[('impactHeightRO', 'MetaData')] = \
        obs_data[('impactParameterRO', 'MetaData')] - \
        obs_data[('geoidUndulation', 'MetaData')] - \
        obs_data[('earthRadiusCurvature', 'MetaData')]

    if add_qc:
        good = quality_control(profile_meta_data, height, lats, lons)
        if len(lats[good]) == 0:
            # exit if entire profile is missing
            return{}
        for k in obs_data.keys():
            obs_data[k] = obs_data[k][good]

    return obs_data


def quality_control(profile_meta_data, heights, lats, lons):
    try:
        good = (heights > 0.) & (heights < 100000.) & (abs(lats) <= 90.) & (abs(lons) <= 360.)
    except ValueError:
        print(f" quality control on impact_height and lat/lon did not pass")
        print(f" maybe length of vectors not consistent: {len(heights)}, {len(lats)}, {len(lons)}")
        return []

    # bad radius or
    # large geoid undulation
    if (profile_meta_data['earthRadiusCurvature'] > 6450000.) or (profile_meta_data['earthRadiusCurvature'] < 6250000.) or \
       (abs(profile_meta_data['geoidUndulation']) > 200):
        good = []
        # bad profile
    return good


def def_meta_data():

    meta_data_keys = {
        "qualityFlags": 'radioOccultationDataQualityFlags',
        "geoidUndulation": 'geoidUndulation',
        "sensorAzimuthAngle": 'bearingOrAzimuth',
        # "timeIncrement": 'timeIncrement',
        "earthRadiusCurvature": 'earthLocalRadiusOfCurvature',
        "satelliteIdentifier": 'satelliteIdentifier',
        "satelliteInstrument": 'satelliteInstruments',
        "dataProviderOrigin": 'centre',
        "satelliteTransmitterId": 'platformTransmitterIdNumber',
        "satelliteConstellationRO": 'satelliteClassification',
    }

    return meta_data_keys


def def_meta_types():

    meta_data_types = {
        "latitude": "float",
        "longitude": "float",
        "dateTime": "long",
        'impactParameterRO': 'float',
        'impactHeightRO': 'float',
        'height': 'float',
        "qualityFlags": 'integer',
        "geoidUndulation": 'float',
        "earthRadiusCurvature": 'float',
        "satelliteIdentifier": 'integer',
        "satelliteInstrument": 'integer',
        "dataProviderOrigin": 'string',
        "satelliteTransmitterId": 'integer',
        "satelliteConstellationRO": 'integer',
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
            elif obs_data[gv_key].dtype == np.int64:
                fill_data = np.repeat(long_missing_value, append_length, dtype=np.int64)
            elif obs_data[gv_key].dtype == int:
                fill_data = np.repeat(int_missing_value, append_length, dtype=ioda_int_type)
            elif obs_data[gv_key].dtype == object:
                # string type, extend with string missing value
                fill_data = np.repeat(string_missing_value, append_length, dtype=object)
            obs_data[gv_key] = np.append(obs_data[gv_key], fill_data)


if __name__ == "__main__":

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the GNSS-RO data from BUFR file'
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of BUFR GNSS-RO observation input file(s)",
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

    args = parser.parse_args()
    main(args)
