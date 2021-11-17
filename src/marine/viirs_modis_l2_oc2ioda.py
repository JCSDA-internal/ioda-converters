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

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

output_var_names = [
    "ocean_mass_content_of_particulate_organic_matter_expressed_as_carbon",
    "mass_concentration_of_chlorophyll_in_sea_water"]

DimDict = {}

VarDims = {}

GlobalAttrs = {}
VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string"),
]


def read_input(input_args):
    """
    Reads/converts a single input file, performing optional thinning also.

    Arguments:

        input_args: A tuple of input filename and global_config
            input_file: The name of file to read
            global_config: structure for global arguments related to read

    Returns:

        A tuple of (obs_data,  GlobalAttrs) needed by the IODA writer.
    """
    input_file = input_args[0]
    global_config = input_args[1]

    print("Reading ", input_file)
    ncd = nc.Dataset(input_file, 'r')

    # get global attributes
    for v in ('platform', 'instrument', 'processing_level'):
        GlobalAttrs[v] = ncd.getncattr(v)

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
    obs_dim = (len(lons))
    obs_data = {}
    if global_config['output_poc']:
        obs_data[(output_var_names[0], global_config['oval_name'])] = \
            np.zeros(obs_dim)
        obs_data[(output_var_names[0], global_config['oerr_name'])] = \
            np.zeros(obs_dim)
        obs_data[(output_var_names[0], global_config['opqc_name'])] = \
            np.zeros(obs_dim)
    if global_config['output_chl']:
        obs_data[(output_var_names[1], global_config['oval_name'])] = \
            np.zeros(obs_dim)
        obs_data[(output_var_names[1], global_config['oerr_name'])] = \
            np.zeros(obs_dim)
        obs_data[(output_var_names[1], global_config['opqc_name'])] = \
            np.zeros(obs_dim)

    # Add the metadata
    obs_data[('datetime', 'MetaData')] = np.empty(len(dates), dtype=object)
    obs_data[('datetime', 'MetaData')][:] = dates
    obs_data[('latitude', 'MetaData')] = lats
    obs_data[('longitude', 'MetaData')] = lons

    if global_config['output_poc']:
        obs_data[output_var_names[0], global_config['oval_name']] = \
            data_in['poc']
        obs_data[output_var_names[0], global_config['oerr_name']] = \
            data_in['poc']*0.0
        obs_data[output_var_names[0], global_config['opqc_name']] = \
            data_in['l2_flags']
    if global_config['output_chl']:
        obs_data[output_var_names[1], global_config['oval_name']] = \
            data_in['chlor_a']
        obs_data[output_var_names[1], global_config['oerr_name']] = \
            data_in['chlor_a']*0.0
        obs_data[output_var_names[1], global_config['opqc_name']] = \
            data_in['l2_flags']

    return (obs_data, GlobalAttrs)


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

    # Setup the configuration that is passed to each worker process
    global_config = {}
    global_config['date'] = args.date
    global_config['thin'] = args.thin
    global_config['oval_name'] = iconv.OvalName()
    global_config['oerr_name'] = iconv.OerrName()
    global_config['opqc_name'] = iconv.OqcName()
    global_config['output_poc'] = args.poc
    global_config['output_chl'] = args.chl

    # Note: Pool.map creates separate processes, and can only take iterable
    # objects. Rather than using global variables, embed them into
    # the iterable object together with argument array passed in (args.input)

    pool_inputs = [(i, global_config) for i in args.input]

    # read / process files in parallel
    pool = Pool(args.threads)
    obs = pool.map(read_input, pool_inputs)

    # concatenate the data from the files
    obs_data, GlobalAttrs = obs[0]
    for i in range(1, len(obs)):
        obs_data.update(obs[i][0])
    # Get the nlocs
    nlocs = len(obs_data[('longitude', 'MetaData')])

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs['date_time_string'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    GlobalAttrs['thinning'] = args.thin
    GlobalAttrs['converter'] = os.path.basename(__file__)
    DimDict['nlocs'] = nlocs
    GlobalAttrs['nlocs'] = np.int32(DimDict['nlocs'])

    # determine which variables we are going to output
    if args.poc:
        VarAttrs[output_var_names[0], global_config['oval_name']]['units'] = \
            'mg ^m-3'
        VarAttrs[output_var_names[0], global_config['oerr_name']]['units'] = \
            'mg ^m-3'
        VarAttrs[output_var_names[0], global_config['opqc_name']]['units'] = \
            'unitless'
        VarAttrs[output_var_names[0], global_config['oval_name']]['_FillValue'] = \
            -32767.
        VarAttrs[output_var_names[0], global_config['oerr_name']]['_FillValue'] = \
            -32767.
        VarAttrs[output_var_names[0], global_config['opqc_name']]['_FillValue'] = \
            -32767
        VarDims["ocean_mass_content_of_particulate_organic_matter_expressed_as_carbon"] = ['nlocs']

    if args.chl:
        VarAttrs[output_var_names[1], global_config['oval_name']]['units'] = \
            'mg ^m-3'
        VarAttrs[output_var_names[1], global_config['oerr_name']]['units'] = \
            'mg ^m-3'
        VarAttrs[output_var_names[1], global_config['opqc_name']]['units'] = \
            'unitless'
        VarAttrs[output_var_names[1], global_config['oval_name']]['_FillValue'] = \
            -32767.
        VarAttrs[output_var_names[1], global_config['oerr_name']]['_FillValue'] = \
            -32767.
        VarAttrs[output_var_names[1], global_config['opqc_name']]['_FillValue'] = \
            -32767
        VarDims["mass_concentration_of_chlorophyll_in_sea_water"] = ['nlocs']

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
