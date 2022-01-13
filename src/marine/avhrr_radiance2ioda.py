#!/usr/bin/env python3

#
# (C) Copyright 2019-2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

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

output_var_names = ["brightness_temperature"]

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

GlobalAttrs = {}

VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

DimDict = {}

VarDims = {}

chan_number = {3, 4, 5}


def read_input(input_args):
    """
    Reads/converts a single input file, performing optional thinning also.

    Arguments:

        input_args: A tuple of input filename and global_config
            input_file: The name of file to read
            global_config: structure for global arguments related to read

    Returns:

        A tuple of (obs_data, GlobalAttrs) needed by the IODA writer.
    """
    input_file = input_args[0]
    global_config = input_args[1]

    print("Reading ", input_file)
    ncd = nc.Dataset(input_file, 'r')

    # get the base time (should only have 1 or 2 time slots)
    time_base = ncd.variables['time'][:]
    basetime = dateutil.parser.parse(ncd.variables['time'].units[-20:])

    # get some of the global attributes that we are interested in

    #    for v in ('platform', 'sensor', 'processing_level'):
    #        GlobalAttrs[v] = ncd.getncattr(v)

    # get the QC flags, and calculate a mask from the non-missing values
    # (since L3 files are mostly empty, fields need a mask applied immediately
    # to avoid using way too much memory)
    data_in = {}
    data_in['bad_pixel_mask'] = ncd.variables['bad_pixel_mask'][:].ravel()
    mask = data_in['bad_pixel_mask'] >= 0
    data_in['bad_pixel_mask'] = data_in['bad_pixel_mask'][mask]

    # Determine the lat/lon grid.
    lons = ncd.variables['longitude'][:].ravel()
    lats = ncd.variables['latitude'][:].ravel()

    len_grid = len(lons)*len(lats)
    lons, lats = np.meshgrid(lons, lats, copy=False)
    lons = np.tile(lons.ravel(), len(time_base)).ravel()[mask]
    lats = np.tile(lats.ravel(), len(time_base)).ravel()[mask]

    # calculate the basetime offsets
    time = np.tile(np.atleast_2d(time_base).T, (1, len_grid)).ravel()[mask]

    # load in all the other data and apply the missing value mask
    input_vars = (
        'scan_line_number',  # "scan line number"
        'bad_pixel_mask',  # quality flaq
        'scan_line_time',  # "time for the scan line in fractional hours"
        'solar_zenith_angle',
        'sensor_zenith_angle',
        'sensor_azimuth_angle',
        'solar_azimuth_angle',
        'temp_11_0um_nom',
        'temp_12_0um_nom',
        'temp_3_75um_nom',
        'temp_11_0um_nom_stddev_3x3')

    for v in input_vars:
        if v not in data_in:
            data_in[v] = ncd.variables[v][:].ravel()[mask]
            if 'scale_factor' in ncd.variables[v].__dict__:
                scale_factor = ncd.variables[v].scale_factor
                add_offset = ncd.variables[v].add_offset
                data_in[v] = scale_factor * data_in[v] + add_offset

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
            timedelta(seconds=float(time[i]+3600*data_in['scan_line_time'][i]))  # check later
        dates.append(obs_date.strftime("%Y-%m-%dT%H:%M:%SZ"))

    # output values
    nchans = len(chan_number)
    obs_dim = (len(lons))
    val_tb = np.zeros((obs_dim, nchans))
    val_tb[:, 0] = data_in['temp_3_75um_nom']
    val_tb[:, 1] = data_in['temp_11_0um_nom']
    val_tb[:, 2] = data_in['temp_12_0um_nom']
    # there isn't std for every channels we use the same obs error for all chans
    err = np.zeros((obs_dim, nchans))
    err[:, 0] = data_in['temp_11_0um_nom_stddev_3x3']
    err[:, 1] = data_in['temp_11_0um_nom_stddev_3x3']
    err[:, 2] = data_in['temp_11_0um_nom_stddev_3x3']
    qc = np.zeros((obs_dim, nchans))
    qc[:, 0] = data_in['bad_pixel_mask']
    qc[:, 1] = data_in['bad_pixel_mask']
    qc[:, 2] = data_in['bad_pixel_mask']

    # allocate space for output depending on which variables are to be saved

    obs_data = {}
    obs_data[('datetime', 'MetaData')] = np.empty(len(dates), dtype=object)
    obs_data[('datetime', 'MetaData')][:] = dates
    obs_data[('latitude', 'MetaData')] = lats
    obs_data[('longitude', 'MetaData')] = lons
    obs_data[('record_number', 'MetaData')] = data_in['scan_line_number']
    obs_data[('height_above_mean_sea_level', 'MetaData')] = 840*np.ones((obs_dim))
    obs_data[('sensor_azimuth_angle', 'MetaData')] = data_in['sensor_azimuth_angle']
    obs_data[('sensor_zenith_angle', 'MetaData')] = data_in['sensor_zenith_angle']
    obs_data[('solar_zenith_angle', 'MetaData')] = data_in['solar_zenith_angle']
    obs_data[('solar_azimuth_angle', 'MetaData')] = data_in['solar_azimuth_angle']
    obs_data[('scan_position', 'MetaData')] = data_in['scan_line_number']
    obs_data[output_var_names[0], global_config['oval_name']] = val_tb
    obs_data[output_var_names[0], global_config['oerr_name']] = err
    obs_data[output_var_names[0], global_config['opqc_name']] = qc.astype('int32')

    return (obs_data, GlobalAttrs)


def main():

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the brightness temperature from any PATMOSX Data '
            ' and converts into IODA formatted output files.'
            ' Multiple files are concatenated ')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of patmosx avhrr observation input file(s)",
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

    args = parser.parse_args()
    args.date = datetime.strptime(args.date, '%Y%m%d%H')

    # Setup the configuration that is passed to each worker process
    # Note: Pool.map creates separate processes, and can only take iterable
    # objects. Rather than using global variables, embed them into
    # the iterable object together with argument array passed in (args.input)
    global_config = {}
    global_config['date'] = args.date
    global_config['thin'] = args.thin
    global_config['oval_name'] = iconv.OvalName()
    global_config['oerr_name'] = iconv.OerrName()
    global_config['opqc_name'] = iconv.OqcName()

    pool_inputs = [(i, global_config) for i in args.input]

    # read / process files in parallel
    pool = Pool(args.threads)
    obs = pool.map(read_input, pool_inputs)

    # concatenate the data from the files
    obs_data, GlobalAttrs = obs[0]

    for i in range(1, len(obs)):
        for k in obs_data:
            axis = len(obs[i][0][k].shape)-1
            obs_data[k] = np.concatenate(
                (obs_data[k], obs[i][0][k]), axis=axis)

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs['date_time_string'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    GlobalAttrs['thinning'] = args.thin
    GlobalAttrs['converter'] = os.path.basename(__file__)

    # determine which variables we are going to output
    selected_names = []
    selected_names.append(output_var_names[0])

    # pass parameters to the IODA writer
    # (needed because we are bypassing ExtractObsData within BuildNetcdf)
    VarDims = {
        'brightness_temperature': ['nlocs', 'nchans']
    }

    nchans = len(chan_number)
    nlocs = len(obs_data[('longitude', 'MetaData')])
    DimDict = {
        'nlocs': nlocs,
        'nchans': list(chan_number)
    }

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs[('brightness_temperature', 'ObsValue')]['units'] = 'Kelvin'
    VarAttrs[('brightness_temperature', 'ObsError')]['units'] = 'Kelvin'
    VarAttrs[('brightness_temperature', 'PreQC')]['units'] = 'unitless'
    VarAttrs[('brightness_temperature', 'ObsValue')]['_FillValue'] = 999
    VarAttrs[('brightness_temperature', 'ObsError')]['_FillValue'] = 999
    VarAttrs[('brightness_temperature', 'PreQC')]['_FillValue'] = 999

    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
