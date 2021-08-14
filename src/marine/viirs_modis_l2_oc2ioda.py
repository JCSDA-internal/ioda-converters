#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
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

import ioda_conv_ncio as iconv

output_var_names = [
    "ocean_mass_content_of_particulate_organic_matter_expressed_as_carbon",
    "mass_concentration_of_chlorophyll_in_sea_water"]


def read_input(input_args):
    """
    Reads/converts a single input file, performing optional thinning also.

    Arguments:

        input_args: A tuple of input filename and global_config
            input_file: The name of file to read
            global_config: structure for global arguments related to read

    Returns:

        A tuple of (obs_data, loc_data, attr_data) needed by the IODA writer.
    """
    input_file = input_args[0]
    global_config = input_args[1]

    print("Reading ", input_file)
    ncd = nc.Dataset(input_file, 'r')

    # get global attributes
    attr_data = {}
    for v in ('platform', 'instrument', 'processing_level'):
        attr_data[v] = ncd.getncattr(v)

    # get QC flags, and calculate a mask from the non-missing values
    # since L2 OC files are quite empty, need a mask applied immediately
    # to avoid using too much memory)
    gpd = ncd.groups['geophysical_data']
    data_in = {}
    data_in['l2_flags'] = gpd.variables['l2_flags'][:].ravel()
    mask = data_in['l2_flags'] < 1073741824
    data_in['l2_flags'] = data_in['l2_flags'][mask]

    # Determine the lat/lon grid
    ngd = ncd.groups['navigation_data']
    number_of_lines = len(ngd.variables['longitude'][:, 1])
    pixels_per_line = len(ngd.variables['longitude'][1, :])
    lons = ngd.variables['longitude'][:].ravel()[mask]
    lats = ngd.variables['latitude'][:].ravel()[mask]

    # get basetime and time as a difference in seconds
    sla = ncd.groups['scan_line_attributes']
    basetime = datetime(sla.variables['year'][0], 1, 1) + \
        timedelta(days=int(sla.variables['day'][0]-1),
                  milliseconds=int(sla.variables['msec'][0]))
    time = (np.repeat(sla.variables['msec'][:].ravel(),
            pixels_per_line).ravel() - sla.variables['msec'][0])/1000.0
    data_in['time'] = time[mask]

    # load in all the other data and apply the missing value mask
    input_vars = ('poc', 'chlor_a')
    for v in input_vars:
        if v not in data_in:
            data_in[v] = gpd.variables[v][:].ravel()[mask]
    ncd.close()

    # Create a mask for optional random thinning
    np.random.seed(
        int((global_config['date']-datetime(1970, 1, 1)).total_seconds()))
    mask = np.random.uniform(size=len(lons)) > global_config['thin']

    # apply the masks for thinning and missing values
    lons = lons[mask]
    lats = lats[mask]
    data_in['time'] = data_in['time'][mask]
    data_in['l2_flags'] = data_in['l2_flags'][mask]
    for v in input_vars:
        data_in[v] = data_in[v][mask]

    # create a string version of the date for each observation
    dates = []
    for i in range(len(lons)):
        obs_date = basetime + timedelta(seconds=float(data_in['time'][i]))
        dates.append(obs_date.strftime("%Y-%m-%dT%H:%M:%SZ"))

    # allocate space for output depending on which variables are to be saved
    num_vars = 0
    obs_dim = (len(lons))
    obs_data = {}
    if global_config['output_poc']:
        obs_data[(output_var_names[0], global_config['oval_name'])] = \
            np.zeros(obs_dim),
        obs_data[(output_var_names[0], global_config['oerr_name'])] = \
            np.zeros(obs_dim),
        obs_data[(output_var_names[0], global_config['opqc_name'])] = \
            np.zeros(obs_dim),
        num_vars += 1
    if global_config['output_chl']:
        obs_data[(output_var_names[1], global_config['oval_name'])] = \
            np.zeros(obs_dim),
        obs_data[(output_var_names[1], global_config['oerr_name'])] = \
            np.zeros(obs_dim),
        obs_data[(output_var_names[1], global_config['opqc_name'])] = \
            np.zeros(obs_dim),
        num_vars += 1

    # create the final output structures
    loc_data = {
        'latitude': lats,
        'longitude': lons,
        'datetime': dates,
    }
    if global_config['output_poc']:
        obs_data[(output_var_names[0], global_config['oval_name'])] = \
            data_in['poc']
        obs_data[(output_var_names[0], global_config['oerr_name'])] = \
            data_in['poc']*0.0
        obs_data[(output_var_names[0], global_config['opqc_name'])] = \
            data_in['l2_flags']
    if global_config['output_chl']:
        obs_data[(output_var_names[1], global_config['oval_name'])] = \
            data_in['chlor_a']
        obs_data[(output_var_names[1], global_config['oerr_name'])] = \
            data_in['chlor_a']*0.0
        obs_data[(output_var_names[1], global_config['opqc_name'])] = \
            data_in['l2_flags']

    return (obs_data, loc_data, attr_data)


def main():

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the particulate organic carbon and chlorophyll data from'
            ' VIIRS/MODIS Specification formatted L2 file(s) and converts'
            ' into IODA formatted output files. Multiple files are'
            ' concatenated and optional thinning can be performed.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of VIIRS/MODIS L2 Ocean Color observation input file(s)",
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
        '-t', '--thin',
        help="percentage of random thinning, from 0.0 to 1.0. Zero indicates"
             " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)
    optional.add_argument(
        '--threads',
        help='multiple threads can be used to load input files in parallel.'
             ' (default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '--poc',
        help='if set, only poc is output.'
             ' Otherwise, poc and chlor_a are both output.',
        action='store_true')
    optional.add_argument(
        '--chl',
        help='if set, only chlor_a (OCI algorithm) is output.'
             ' Otherwise, poc and chlor_a are both output.',
        action='store_true')

    args = parser.parse_args()
    args.date = datetime.strptime(args.date, '%Y%m%d%H')
    if not args.chl and not args.poc:
        args.chl = True
        args.poc = True

    # setup the IODA writer
    writer = iconv.NcWriter(args.output, [], [])

    # Setup the configuration that is passed to each worker process
    # Note: Pool.map creates separate processes, and can only take iterable
    # objects. Rather than using global variables, embed them into
    # the iterable object together with argument array passed in (args.input)
    global_config = {}
    global_config['date'] = args.date
    global_config['thin'] = args.thin
    global_config['oval_name'] = writer.OvalName()
    global_config['oerr_name'] = writer.OerrName()
    global_config['opqc_name'] = writer.OqcName()
    global_config['output_poc'] = args.poc
    global_config['output_chl'] = args.chl
    pool_inputs = [(i, global_config) for i in args.input]

    # read / process files in parallel
    pool = Pool(args.threads)
    obs = pool.map(read_input, pool_inputs)

    # concatenate the data from the files
    obs_data, loc_data, attr_data = obs[0]
    loc_data['datetime'] = writer.FillNcVector(
        loc_data['datetime'], "datetime")
    for i in range(1, len(obs)):
        for k in obs_data:
            axis = len(obs[i][0][k].shape)-1
            obs_data[k] = np.concatenate(
                (obs_data[k], obs[i][0][k]), axis=axis)
        for k in loc_data:
            d = obs[i][1][k]
            if k == 'datetime':
                d = writer.FillNcVector(d, 'datetime')
            loc_data[k] = np.concatenate((loc_data[k], d), axis=0)

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    attr_data['date_time_string'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    attr_data['thinning'] = args.thin
    attr_data['converter'] = os.path.basename(__file__)

    # determine which variables we are going to output
    selected_names = []
    if args.poc:
        selected_names.append(output_var_names[0])
    if args.chl:
        selected_names.append(output_var_names[1])
    var_data = {writer._var_list_name:
                writer.FillNcVector(selected_names, "string")}

    # pass parameters to the IODA writer
    # (needed because we are bypassing ExtractObsData within BuildNetcdf)
    writer._nvars = len(selected_names)
    writer._nlocs = obs_data[(selected_names[0], 'ObsValue')].shape[0]

    # use the writer class to create the final output file
    writer.BuildNetcdf(obs_data, loc_data, var_data, attr_data)


if __name__ == '__main__':
    main()
