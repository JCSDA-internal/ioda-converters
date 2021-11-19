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
import numpy as np
from multiprocessing import Pool
import os
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

from eccodes import *

output_var_names = [
    "bending_angle",
    "refractivity",
]

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]
GlobalAttrs = {
}

DimDict = {
}


def read_input(input_args):
    """
    Reads/converts a single input file, performing optional thinning also.

    Arguments:

        input_args: an input filename
            input_file: The name of file to read

    Returns:

        A tuple of (obs_data, loc_data, attr_data) needed by the IODA writer.
    """
    input_file = input_args[0]

    print("Reading ", input_file)
    f = open(input_file,'rb')
    bufr = codes_bufr_new_from_file(f)
    codes_set(bufr, 'unpack', 1)

    qc_data = get_meta_qc(bufr)

    attr_data  = get_meta_data(bufr)

    loc_data = get_loc_data(bufr)

    obs_data = get_obs_data(bufr)

    return (obs_data, loc_data, attr_data)

def get_meta_qc(bufr):

    ro_quality = codes_get_array(bufr, 'radioOccultationDataQualityFlags')
    ro_conf = codes_get_array(bufr, 'percentConfidence')[0]
#   skip if flagged? there's no spot in IODA for this 
#   if ro_conf == 0:
#     return

    qc_data = {
        'ro_quality_word'        : ro_quality,
        'ro_precent_confidence'  : ro_conf,
}

    return qc_data

def get_meta_data(bufr):

    # get some of the global attributes that we are interested in
    meta_data_keys = def_meta_data()

    # these are the MetaData we are interested in
    attr_data = {}
    for k in meta_data_keys.keys():
        attr_data[k] = codes_get(bufr, v)

    # these are derived but single per profile
    #attr_data["ascending_flag"] = bit decomposition of ro_quality_word
    #attr_data["record_number"]  = # do not know how this works (one per profile or occultation point)

    return attr_data


def get_loc_data(bufr):

    # location data
    loc_data_keys = {
        "latitude"                                : '#1#latitude',
        "longitude"                               : '#1#longitude',
    }

    # these are the MetaData we are interested in
    loc_data = {}
    for k, v in meta_data_keys.items():
        loc_data[k] = codes_get(bufr, v)

    # do the hokey time structure to time structure
    year  = codes_get(bufr, 'year')
    month = codes_get(bufr, 'month')
    day   = codes_get(bufr, 'day')
    hour  = codes_get(bufr, 'hour')
    minute = codes_get(bufr, 'minute')
    second = codes_get(bufr, 'second')  #  non-integer value

    # thought this string conversion was gone ???
    dtg = ( '%4i%.2i%.2i%.2i%.2i' % (year, month, day, hour, minute) )
    # which time is needed ???
    loc_data['datetime'] = dtg
    loc_data['datetime'] = datetime.strptime( dtg ).strftime("%Y-%m-%dT%H:%M:%SZ")

    return loc_data

    # allocate space for output depending on which variables are to be saved
    obs_data = {}

    # replication factors for the datasets, bending angle, refractivity and derived profiles
    krepfac = codes_get_array(bufr, 'extendedDelayedDescriptorReplicationFactor')
    #array([247, 247, 200])
    
    drepfac = codes_get_array(bufr, 'delayedDescriptorReplicationFactor')
    #len(drepfac) Out[13]: 247   # ALL values all 3
    # sequence is 3 * (freq,impact,bendang,first-ord stat, bendang error, first-ord sat)
    #  note the label bendingAngle is used for both the value and its error !!!

    # get the bending angle
    lats      = codes_get_array(bufr, 'latitude')[1:]             # geolocation the first value if the average
    lons      = codes_get_array(bufr, 'longitude')[1:]
    bang      = codes_get_array(bufr, 'bendingAngle')[4::6]       # L1, L2, combined -- only care about combined
    bang_err  = codes_get_array(bufr, 'bendingAngle')[5::6]
    impact    = codes_get_array(bufr, 'impactParameter')[2::3]
    bang_conf = codes_get_array(bufr, 'percentConfidence')[1:krepfac[0]+1]
    #len(bang) Out[19]: 1482   (krepfac * 6) -or- (krepfac * drepfac * 2 )`

    # value, ob_error, qc
    obs_data[('bending_angle', global_config['oval_name'])] = bang
    obs_data[('bending_angle', global_config['oerr_name'])] = bang_err
    obs_data[('bending_angle', global_config['opqc_name'])] = bang_conf

    ## left off here ##

    # get the refractivity
    height = codes_get_array(bufr, 'height')
    refrac = codes_get_array(bufr, 'atmosphericRefractivity')[0::2]
    refr_conf = codes_get_array(bufr, 'percentConfidence')[sum(krepfac[:1])+1:sum(krepfac[:2])+1]

    # get derived profiles
    geop   = codes_get_array(bufr, 'geopotentialHeight')[:-1]
    pres   = codes_get_array(bufr, 'nonCoordinatePressure')[0:-2:2]
    temp   = codes_get_array(bufr, 'airTemperature')[0::2]
    spchum = codes_get_array(bufr, 'specificHumidity')[0::2]
    prof_conf = codes_get_array(bufr, 'percentConfidence')[sum(krepfac[:2])+1:sum(krepfac)+1]

    return obs_data


def main():

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the GNSS-RO data from BUFR file'
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated' 
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
    args.date = datetime.strptime(args.date, '%Y%m%d%H')

    # setup the IODA writer
    writer = iconv.NcWriter(args.output, [], [])

    # Setup the configuration that is passed to each worker process
    # Note: Pool.map creates separate processes, and can only take iterable
    # objects. Rather than using global variables, embed them into
    # the iterable object together with argument array passed in (args.input)
    global_config = {}
    global_config['date'] = args.date
    global_config['oval_name'] = writer.OvalName()
    global_config['oerr_name'] = writer.OerrName()
    global_config['opqc_name'] = writer.OqcName()

    pool_inputs = [(i, global_config) for i in args.input]

    # read / process files in parallel
    pool = Pool(args.threads)
    obs = pool.map(read_input, pool_inputs)

    # concatenate the data from the files
    obs_data, loc_data, attr_data = obs[0]

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    attr_data['date_time_string'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    attr_data['converter'] = os.path.basename(__file__)

    # pass parameters to the IODA writer
    VarDims = {
        'bending_angle': ['nlocs']
        'refractivity': ['nlocs']
    }

    # Read in the profiles

    # write them out

    nlocs = obs_data[('bending_angle', 'ObsValue')].shape[0]

    DimDict = {'nlocs': nlocs}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('bending_angle', 'ObsValue')]['units'] = 'Radians'
    VarAttrs[('bending_angle', 'ObsError')]['units'] = 'Radians'
    VarAttrs[('refractivity', 'ObsValue')]['units'] = 'N'
    VarAttrs[('refractivity', 'ObsError')]['units'] = 'N'
#   there is no PreQc defined in Hailing's fortran
#   VarAttrs[('bending_angle', 'PreQC')]['units'] = 'unitless'
    #  REALLY???? say it isn't so????
    VarAttrs[('bending_angle', 'ObsValue')]['_FillValue'] = 9.9e10
    VarAttrs[('bending_angle', 'ObsError')]['_FillValue'] = 9.9e10
    VarAttrs[('refractivity', 'ObsValue')]['_FillValue'] = 9.9e10
    VarAttrs[('refractivity', 'ObsError')]['_FillValue'] = 9.9e10
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)

def def_meta_data():
    meta_data_keys = {
 "latitude"                                : '#1#latitude',
 "longitude"                               : '#1#longitude',
 "geoid_height_above_reference_ellipsoid"  : 'geoidUndulation',
 "earth_radius_of_curvature"               : 'earthLocalRadiusOfCurvature',
 "sensor_azimuth_angle"                    : '#1#bearingOrAzimuth',
 "occulting_sat_id"                        : 'satelliteIdentifier',
 "occulting_sat_is"                        : 'satelliteInstruments',
 "process_center"                          : 'centre',
 "reference_sat_id"                        : 'platformTransmitterIdNumber',
 "gnss_sat_class"                          : 'satelliteClassification',
}

    return meta_data_keys

if __name__ == '__main__':
    main()
