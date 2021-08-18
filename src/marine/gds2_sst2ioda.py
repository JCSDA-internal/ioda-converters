#!/usr/bin/env python3

#
# (C) Copyright 2019 UCAR
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
    "sea_surface_temperature",
    "sea_surface_skin_temperature"]


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

    # get the base time (should only have 1 or 2 time slots)
    time_base = ncd.variables['time'][:]
    basetime = dateutil.parser.parse(ncd.variables['time'].units[-20:])

    # get some of the global attributes that we are interested in
    attr_data = {}
    for v in ('platform', 'sensor', 'processing_level'):
        attr_data[v] = ncd.getncattr(v)

    # get the QC flags, and calculate a mask from the non-missing values
    # (since L3 files are mostly empty, fields need a mask applied immediately
    # to avoid using way too much memory)
    data_in = {}
    data_in['quality_level'] = ncd.variables['quality_level'][:].ravel()
    mask = data_in['quality_level'] >= 0
    data_in['quality_level'] = data_in['quality_level'][mask]

    # Determine the lat/lon grid.
    # If L3, we need to convert 1D lat/lon to 2D lat/lon.
    # If len(time) > 1, also need to repeat the lat/lon vals
    lons = ncd.variables['lon'][:].ravel()
    lats = ncd.variables['lat'][:].ravel()
    if attr_data['processing_level'][:2] == 'L3':
        len_grid = len(lons)*len(lats)
        lons, lats = np.meshgrid(lons, lats, copy=False)
        lons = np.tile(lons.ravel(), len(time_base)).ravel()[mask]
        lats = np.tile(lats.ravel(), len(time_base)).ravel()[mask]
    else:
        len_grid = len(lons)
        lons = np.tile(lons, len(time_base)).ravel()[mask]
        lats = np.tile(lats, len(time_base)).ravel()[mask]

    # calculate the basetime offsets
    time = np.tile(np.atleast_2d(time_base).T, (1, len_grid)).ravel()[mask]

    # load in all the other data and apply the missing value mask
    input_vars = (
        'quality_level',
        'sst_dtime',
        'sses_bias',
        'sses_standard_deviation',
        'sea_surface_temperature')
    for v in input_vars:
        if v not in data_in:
            data_in[v] = ncd.variables[v][:].ravel()[mask]
    ncd.close()

    # Create a mask for optional random thinning
    np.random.seed(
        int((global_config['date']-datetime(1970, 1, 1)).total_seconds()))
    mask = np.random.uniform(size=len(lons)) > global_config['thin']

    # also, sometimes certain input variables have their own mask due to
    # missing values
    for v in input_vars:
        if np.ma.is_masked(data_in[v]):
            mask = np.logical_and(mask, np.logical_not(data_in[v].mask))

    # apply the masks for thinning and missing values
    time = time[mask]
    lons = lons[mask]
    lats = lats[mask]
    for v in input_vars:
        data_in[v] = data_in[v][mask]

    # create a string version of the date for each observation
    dates = []
    for i in range(len(lons)):
        obs_date = basetime + \
            timedelta(seconds=float(time[i]+data_in['sst_dtime'][i]))
        dates.append(obs_date.strftime("%Y-%m-%dT%H:%M:%SZ"))

    # calculate output values
    # Note: the qc flags in GDS2.0 run from 0 to 5, with higher numbers
    # being better. IODA typically expects 0 to be good, and higher numbers
    # are bad, so the qc flags flipped here.
    # TODO change everything in soca to handle K instead of C ?
    val_sst_skin = data_in['sea_surface_temperature'] - 273.15
    val_sst = val_sst_skin - data_in['sses_bias']
    err = data_in['sses_standard_deviation']
    qc = 5 - data_in['quality_level']

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
            'Reads the sea surface temperature from any GHRRST Data '
            ' Specification (GDS2.0) formatted L2 or L3 file(s) and converts'
            ' into IODA formatted output files. Multiple files are'
            ' concatenated and optional thinning can be performed.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of GHRSST GDS2.0 SST observation input file(s)",
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
        '--sst',
        help='if set, only the bias corrected bulk sst is output.'
             ' Otherwise, bulk sst, and skin sst are both output.',
        action='store_true')
    optional.add_argument(
        '--skin_sst',
        help='if set, only the skin or subskin sst is output.'
             ' Otherwise, bulk sst, and skin sst are both output.',
        action='store_true')

    args = parser.parse_args()
    args.date = datetime.strptime(args.date, '%Y%m%d%H')
    if not args.sst and not args.skin_sst:
        args.sst = True
        args.skin_sst = True

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
    global_config['output_sst'] = args.sst
    global_config['output_skin_sst'] = args.skin_sst
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
    if args.sst:
        selected_names.append(output_var_names[0])
    if args.skin_sst:
        selected_names.append(output_var_names[1])
    var_data = {writer._var_list_name: writer.FillNcVector(
        selected_names, "string")}

    # pass parameters to the IODA writer
    # (needed because we are bypassing ExtractObsData within BuildNetcdf)
    writer._nvars = len(selected_names)
    writer._nlocs = obs_data[(selected_names[0], 'ObsValue')].shape[0]

    # use the writer class to create the final output file
    writer.BuildNetcdf(obs_data, loc_data, var_data, attr_data)


if __name__ == '__main__':
    main()
