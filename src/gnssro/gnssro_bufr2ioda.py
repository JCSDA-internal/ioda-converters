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
import netCDF4 as nc
from datetime import datetime, timedelta
import dateutil.parser
from concurrent.futures import ProcessPoolExecutor
import numpy as np
import os
from pathlib import Path
from itertools import repeat

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

from eccodes import *

# globals
ioda_float_type = 'float32'
ioda_int_type = 'int32'
float_missing_value = -1.0e+37
int_missing_value = -2147483647

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]


def main(args):

    args.date = datetime.strptime(args.date, '%Y%m%d%H')
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
                print("INFO: non-nominal file skipping")
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
    GlobalAttrs['date_time_string'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    date_time_int32 = np.array(int(args.date.strftime("%Y%m%d%H")), dtype='int32')
    GlobalAttrs['date_time'] = date_time_int32.item()
    GlobalAttrs['converter'] = os.path.basename(__file__)

    # pass parameters to the IODA writer
    VarDims = {
        'bending_angle': ['nlocs'],
        'refractivity': ['nlocs']
    }

    # write them out
    nlocs = obs_data[('bending_angle', 'ObsValue')].shape[0]
    DimDict = {'nlocs': nlocs}
    meta_data_types = def_meta_types()
    for k, v in meta_data_types.items():
        locationKeyList.append((k, v))
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('bending_angle', 'ObsValue')]['units'] = 'Radians'
    VarAttrs[('bending_angle', 'ObsError')]['units'] = 'Radians'
    VarAttrs[('bending_angle', 'PreQC')]['units'] = 'unitless'
    VarAttrs[('refractivity', 'ObsValue')]['units'] = 'N'
    VarAttrs[('refractivity', 'ObsError')]['units'] = 'N'
    VarAttrs[('refractivity', 'PreQC')]['units'] = 'unitless'

    VarAttrs[('bending_angle', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('bending_angle', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('bending_angle', 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[('refractivity', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('refractivity', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('refractivity', 'PreQC')]['_FillValue'] = int_missing_value

    VarAttrs[('latitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('altitude', 'MetaData')]['_FillValue'] = float_missing_value

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

    # should really add seconds
    dtg = ("%4i-%.2i-%.2iT%.2i:%.2i:00Z" % (year, month, day, hour, minute))
    profile_meta_data['datetime'] = datetime.strptime(dtg, "%Y-%m-%dT%H:%M:%SZ")

    return profile_meta_data


def get_obs_data(bufr, profile_meta_data, add_qc, record_number=None):

    # allocate space for output depending on which variables are to be saved
    obs_data = {}

    # replication factors for the datasets, bending angle, refractivity and derived profiles
    krepfac = codes_get_array(bufr, 'extendedDelayedDescriptorReplicationFactor')
    # array([247, 247, 200])

    drepfac = codes_get_array(bufr, 'delayedDescriptorReplicationFactor')
    # len(drepfac) Out[13]: 247   # ALL values all 3
    # sequence is 3 *(freq,impact,bendang,first-ord stat, bendang error, first-ord sat)
    #  note the label bendingAngle is used for both the value and its error !!!

    # get the bending angle
    lats = codes_get_array(bufr, 'latitude')[1:]                     # geolocation -- first value is the average
    lons = codes_get_array(bufr, 'longitude')[1:]
    bang = codes_get_array(bufr, 'bendingAngle')[4::drepfac[0]*2]    # L1, L2, combined -- only care about combined
    bang_err = codes_get_array(bufr, 'bendingAngle')[5::drepfac[0]*2]
    impact = codes_get_array(bufr, 'impactParameter')[2::drepfac[0]]
    bang_conf = codes_get_array(bufr, 'percentConfidence')[1:krepfac[0]+1]
    # len (bang) Out[19]: 1482  (krepfac * 6) -or- (krepfac * drepfac * 2 )`

    # bits are in reverse order according to WMO GNSSRO bufr documentation
    # ! Bit 1=Non-nominal quality
    # ! Bit 3=Rising Occulation (1=rising; 0=setting)
    # ! Bit 4=Excess Phase non-nominal
    # ! Bit 5=Bending Angle non-nominal
    i_non_nominal = get_normalized_bit(profile_meta_data['qualityFlag'], bit_index=16-1)
    i_phase_non_nominal = get_normalized_bit(profile_meta_data['qualityFlag'], bit_index=16-4)
    i_bang_non_nominal = get_normalized_bit(profile_meta_data['qualityFlag'], bit_index=16-5)
    iasc = get_normalized_bit(profile_meta_data['qualityFlag'], bit_index=16-3)
    # add rising/setting (ascending/descending) bit
    obs_data[('ascending_flag', 'MetaData')] = np.array(np.repeat(iasc, krepfac[0]), dtype=ioda_int_type)

    # print( " ... RO QC flags: %i  %i  %i  %i" % (i_non_nominal, i_phase_non_nominal, i_bang_non_nominal, iasc) )

    # exit if non-nominal profile
    if i_non_nominal != 0 or i_phase_non_nominal != 0 or i_bang_non_nominal != 0:
        return {}

    # value, ob_error, qc
    obs_data[('bending_angle', "ObsValue")] = assign_values(bang)
    obs_data[('bending_angle', "ObsError")] = assign_values(bang_err)
    obs_data[('bending_angle', "PreQC")] = np.full(krepfac[0], 0, dtype=ioda_int_type)

    # (geometric) height is read as integer but expected as float in output
    height = codes_get_array(bufr, 'height', ktype=float)

    # get the refractivity
    refrac = codes_get_array(bufr, 'atmosphericRefractivity')[0::2]
    refrac_err = codes_get_array(bufr, 'atmosphericRefractivity')[1::2]
    refrac_conf = codes_get_array(bufr, 'percentConfidence')[sum(krepfac[:1])+1:sum(krepfac[:2])+1]

    # value, ob_error, qc
    obs_data[('refractivity', "ObsValue")] = assign_values(refrac)
    obs_data[('refractivity', "ObsError")] = assign_values(refrac_err)
    obs_data[('refractivity', "PreQC")] = np.full(krepfac[0], 0, dtype=ioda_int_type)

    meta_data_types = def_meta_types()

    obs_data[('latitude', 'MetaData')] = assign_values(lats)
    obs_data[('longitude', 'MetaData')] = assign_values(lons)
    obs_data[('impact_parameter', 'MetaData')] = assign_values(impact)
    obs_data[('altitude', 'MetaData')] = assign_values(height)
    for k, v in profile_meta_data.items():
        if type(v) is int:
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, krepfac[0]), dtype=ioda_int_type)
        elif type(v) is float:
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, krepfac[0]), dtype=ioda_float_type)
        else:  # something else (datetime for instance)
            string_array = np.repeat(v.strftime("%Y-%m-%dT%H:%M:%SZ"), krepfac[0])
            obs_data[(k, 'MetaData')] = string_array.astype(object)

    # set record number (multi file procesing will change this)
    if record_number is None:
        nrec = 1
    else:
        nrec = record_number
    obs_data[('record_number', 'MetaData')] = np.array(np.repeat(nrec, krepfac[0]), dtype=ioda_int_type)

    # get derived profiles
    geop = codes_get_array(bufr, 'geopotentialHeight')[:-1]
    pres = codes_get_array(bufr, 'nonCoordinatePressure')[0:-2:2]
    temp = codes_get_array(bufr, 'airTemperature')[0::2]
    spchum = codes_get_array(bufr, 'specificHumidity')[0::2]
    prof_conf = codes_get_array(bufr, 'percentConfidence')[sum(krepfac[:2])+1:sum(krepfac)+1]

    # Compute impact height
    obs_data[('impact_height', 'MetaData')] = \
        obs_data[('impact_parameter', 'MetaData')] - \
        obs_data[('geoid_height_above_reference_ellipsoid', 'MetaData')] - \
        obs_data[('earth_radius_of_curvature', 'MetaData')]

    if add_qc:
        good = quality_control(profile_meta_data, height, lats, lons)
        if len(lats[good]) == 0:
            return{}
            # exit if entire profile is missing
        for k in obs_data.keys():
            obs_data[k] = obs_data[k][good]

    return obs_data


def quality_control(profile_meta_data, heights, lats, lons):
    print('Performing QC Checks')

    good = (heights > 0.) & (heights < 100000.) & (abs(lats) < 90.) & (abs(lons) < 360.)

    # bad radius or
    # large geoid undulation
    if (profile_meta_data['earth_radius_of_curvature'] > 6450000.) or (profile_meta_data['earth_radius_of_curvature'] < 6250000.) or \
       (abs(profile_meta_data['geoid_height_above_reference_ellipsoid']) > 200):
        good = []
        # bad profile
    return good


def def_meta_data():

    meta_data_keys = {
        "qualityFlag": 'radioOccultationDataQualityFlags',
        "geoid_height_above_reference_ellipsoid": 'geoidUndulation',
        "sensor_azimuth_angle": 'bearingOrAzimuth',
        "time": 'timeIncrement',
        "earth_radius_of_curvature": 'earthLocalRadiusOfCurvature',
        "occulting_sat_id": 'satelliteIdentifier',
        "occulting_sat_is": 'satelliteInstruments',
        "process_center": 'centre',
        "reference_sat_id": 'platformTransmitterIdNumber',
        "gnss_sat_class": 'satelliteClassification',
    }

    return meta_data_keys


def def_meta_types():

    meta_data_types = {
        "latitude": "float",
        "longitude": "float",
        "datetime": "string",
        'impact_parameter': 'float',
        'impact_height': 'float',
        'height': 'float',
        "qualityFlag": 'integer',
        "geoid_height_above_reference_ellipsoid": 'float',
        "earth_radius_of_curvature": 'float',
        "occulting_sat_id": 'integer',
        "occulting_sat_is": 'integer',
        "process_center": 'string',
        "reference_sat_id": 'integer',
        "gnss_sat_class": 'integer',
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
            elif obs_data[gv_key].dtype == object:
                # string type, extend with empty strings
                fill_data = np.repeat("", append_length, dtype=object)
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
