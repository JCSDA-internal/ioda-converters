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
from multiprocessing import Pool
import numpy as np
import os
from pathlib import Path

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

GlobalAttrs = {
}


def main(args):

    args.date = datetime.strptime(args.date, '%Y%m%d%H')

    # Setup the configuration that is passed to each worker process
    # Note: Pool.map creates separate processes, and can only take iterable
    # objects. Rather than using global variables, embed them into
    # the iterable object together with argument array passed in (args.input)
    global_config = {}
    global_config['date'] = args.date
    global_config['oval_name'] = 'ObsValue'
    global_config['oerr_name'] = 'ObsError'
    global_config['opqc_name'] = 'PreQC'

    # read / process files in parallel
#   pool_inputs = [(i) for i in args.input]
#   pool = Pool(args.threads)
#   obs = pool.map(read_input, pool_inputs)

    # concatenate the data from the files
#   obs_data = obs[0]

    obs_data = read_input(args.input)

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    attr_data = {}
    attr_data['date_time_string'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    attr_data['converter'] = os.path.basename(__file__)

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

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def read_input(input_args):
    """
    Reads/converts input file(s)

    Arguments:

        input_args: an input filename or names
            input_file: The name of file to read

    Returns:

        A dictionary holding the variables (obs_data) needed by the IODA writer
    """
    input_file = input_args[0]

    print("Reading ", input_file)
    f = open(input_file, 'rb')
    bufr = codes_bufr_new_from_file(f)
    codes_set(bufr, 'unpack', 1)

#   attr_data  = get_meta_data(bufr)
    profile_meta_data = get_meta_data(bufr)

    obs_data = get_obs_data(bufr, profile_meta_data)

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

    # thought this string conversion was gone ???
    # should really add seconds
    dtg = ("%4i-%.2i-%.2iT%.2i:%.2i:00Z" % (year, month, day, hour, minute))
#   profile_meta_data['datetime'] = dtg
    profile_meta_data['datetime'] = datetime.strptime(dtg, "%Y-%m-%dT%H:%M:%SZ")

    # these are derived but also single per profile
    # profile_meta_data["ascending_flag"] = bit decomposition of ro_quality_word
    # profile_meta_data["record_number"]  = # do not know how this works (one per profile or occultation point)

    return profile_meta_data


def get_obs_data(bufr, profile_meta_data):

    # allocate space for output depending on which variables are to be saved
    obs_data = {}

    # replication factors for the datasets, bending angle, refractivity and derived profiles
    krepfac = codes_get_array(bufr, 'extendedDelayedDescriptorReplicationFactor')
    # array([247, 247, 200])

    drepfac = codes_get_array(bufr, 'delayedDescriptorReplicationFactor')
    # len(drepfac) Out[13]: 247   # ALL values all 3
    # sequence is 3 * (freq,impact,bendang,first-ord stat, bendang error, first-ord sat)
    #  note the label bendingAngle is used for both the value and its error !!!

    # get the bending angle
    lats = codes_get_array(bufr, 'latitude')[1:]                     # geolocation -- first value is the average
    lons = codes_get_array(bufr, 'longitude')[1:]
    bang = codes_get_array(bufr, 'bendingAngle')[4::drepfac[0]*2]    # L1, L2, combined -- only care about combined
    bang_err = codes_get_array(bufr, 'bendingAngle')[5::drepfac[0]*2]
    impact = codes_get_array(bufr, 'impactParameter')[2::drepfac[0]]
    bang_conf = codes_get_array(bufr, 'percentConfidence')[1:krepfac[0]+1]
    # len(bang) Out[19]: 1482   (krepfac * 6) -or- (krepfac * drepfac * 2 )`

    # value, ob_error, qc
    obs_data[('bending_angle', "ObsValue")] = assign_values(bang)
    obs_data[('bending_angle', "ObsError")] = assign_values(bang_err)
    obs_data[('bending_angle', "PreQC")] = np.full(krepfac[0], 0, dtype=ioda_int_type)

    # get the refractivity
    height = codes_get_array(bufr, 'height')
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
    obs_data[('height', 'MetaData')] = assign_values(height)
    for k, v in profile_meta_data.items():
        if type(v) is int:
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, krepfac[0]), dtype=ioda_int_type)
        elif type(v) is float:
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, krepfac[0]), dtype=ioda_float_type)
        else:  # something else (datetime for instance)
            # obs_data[(k, meta_data_types[k])] = np.repeat(v, krepfac[0])
            # do we need to do this?
            string_array = np.repeat(v.strftime("%Y-%m-%dT%H:%M:%SZ"), krepfac[0])
            obs_data[(k, 'MetaData')] = string_array.astype(object)

    # get derived profiles
    geop = codes_get_array(bufr, 'geopotentialHeight')[:-1]
    pres = codes_get_array(bufr, 'nonCoordinatePressure')[0:-2:2]
    temp = codes_get_array(bufr, 'airTemperature')[0::2]
    spchum = codes_get_array(bufr, 'specificHumidity')[0::2]
    prof_conf = codes_get_array(bufr, 'percentConfidence')[sum(krepfac[:2])+1:sum(krepfac)+1]

    return obs_data


def def_meta_data():

    meta_data_keys = {
        "qualityFlag": 'radioOccultationDataQualityFlags',
        "geoid_height_above_reference_ellipsoid": 'geoidUndulation',
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
        help="path of IODA output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--threads',
        help='multiple threads can be used to load input files in parallel.'
             ' (default: %(default)s)',
        type=int, default=1)

    args = parser.parse_args()

    main(args)
