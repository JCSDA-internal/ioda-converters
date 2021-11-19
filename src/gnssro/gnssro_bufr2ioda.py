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

    # get some of the global attributes that we are interested in
    attr_data = {}

    meta_data_keys = {
 "latitude"                                : '#1#latitude',
 "longitude"                               : '#1#longitude',
 "geoid_height_above_reference_ellipsoid"  : 'geoidUndulation',
 "earth_radius_of_curvature"               : 'earthLocalRadiusOfCurvature',
 "sensor_azimuth_angle"
 "occulting_sat_id"
 "occulting_sat_is"
 "process_center"
 "reference_sat_id"
 "gnss_sat_class"
 "ascending_flag"
)
#  thought we take integer times now
 "record_number"
 "time"
 "impact_height"
 "impact_parameter"

    for v in ('gnss_sat_class', 'reference_sat_id', 'occulting_sat_id', 'occulting_sat_is', 'ascending_flag', 'process_center'):
        attr_data[v] = ncd.getncattr(v)
    # these are the MetaData we are interested in
    satellite_id = codes_get(bufr, 'satelliteIdentifier')
    sensor_id = codes_get(bufr, 'satelliteInstruments')
    year  = codes_get(bufr, 'year')
    month = codes_get(bufr, 'month')
    day   = codes_get(bufr, 'day')
    hour  = codes_get(bufr, 'hour')
    minute = codes_get(bufr, 'minute')
    second = codes_get(bufr, 'second')  #  non-integer value

    # get the QC flags
    data_in = {}
    data_in['quality_level'] = ncd.variables['quality_level'][:].ravel()
    ro_quality = codes_get_array(bufr, 'radioOccultationDataQualityFlags')
    ro_conf = codes_get_array(bufr, 'percentConfidence')[0]

    # Determine the lat/lon grid.
    # average occulation geolocation
    avglat = codes_get(bufr, '#1#latitude')
    avglon = codes_get(bufr, '#1#longitude')

    e_local_rad_curv = codes_get(bufr, 'earthLocalRadiusOfCurvature')
    geoid_undulation = codes_get(bufr, 'geoidUndulation')

    # create a string version of the date for each observation
    dates = []
    for i in range(len(lons)):
        obs_date = basetime + \
            timedelta(seconds=float(time[i]+data_in['sst_dtime'][i]))
        dates.append(obs_date.strftime("%Y-%m-%dT%H:%M:%SZ"))

    # calculate output values

    # allocate space for output depending on which variables are to be saved
    num_vars = 0
    obs_dim = (len(lons))
    obs_data = {}
    if global_config['output_sst']:
        obs_data[(output_var_names[0], global_config['oval_name'])] = np.zeros(obs_dim),
        obs_data[(output_var_names[0], global_config['oerr_name'])] = np.zeros(obs_dim),
        obs_data[(output_var_names[0], global_config['opqc_name'])] = np.zeros(obs_dim),
        num_vars += 1
    if global_config['output_skin_sst']:
        obs_data[(output_var_names[1], global_config['oval_name'])] = np.zeros(obs_dim),
        obs_data[(output_var_names[1], global_config['oerr_name'])] = np.zeros(obs_dim),
        obs_data[(output_var_names[1], global_config['opqc_name'])] = np.zeros(obs_dim),
        num_vars += 1

    # create the final output structures
    loc_data = {
        'latitude': lats,
        'longitude': lons,
        'datetime': dates,
    }

    if global_config['output_sst']:
        obs_data[(output_var_names[0], global_config['oval_name'])] = val_sst
        obs_data[(output_var_names[0], global_config['oerr_name'])] = err
        obs_data[(output_var_names[0], global_config['opqc_name'])] = qc
    if global_config['output_skin_sst']:
        obs_data[(output_var_names[1], global_config['oval_name'])] = val_sst_skin
        obs_data[(output_var_names[1], global_config['oerr_name'])] = err
        obs_data[(output_var_names[1], global_config['opqc_name'])] = qc

    return (obs_data, loc_data, attr_data)


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

    # Setup the configuration that is passed to each worker process
    # Note: Pool.map creates separate processes, and can only take iterable
    # objects. Rather than using global variables, embed them into
    # the iterable object together with argument array passed in (args.input)
    global_config = {}
    global_config['date'] = args.date
    pool_inputs = [(i, global_config) for i in args.input]

    # read / process files in parallel
    pool = Pool(args.threads)
    obs = pool.map(read_input, pool_inputs)

    # concatenate the data from the files
    obs_data, loc_data, attr_data = obs[0]
    for i in range(1, len(obs)):
        for k in obs_data:
            axis = len(obs[i][0][k].shape)-1
            obs_data[k] = np.concatenate(
                (obs_data[k], obs[i][0][k]), axis=axis)
        for k in loc_data:
            d = obs[i][1][k]
            loc_data[k] = np.concatenate((loc_data[k], d), axis=0)

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


if __name__ == '__main__':
    main()
