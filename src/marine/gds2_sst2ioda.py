#!/usr/bin/env python3

#
# (C) Copyright 2019-2022 UCAR
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

os.environ["TZ"] = "UTC"

# The first section of three variables are related to the IODA output file.

varInfo = [('sst', 'seaSurfaceTemperature', 'K', 999.0),
           ('skin_sst', 'seaSurfaceSkinTemperature', 'K', 999.0)]
var_keys = [var_key[0] for var_key in varInfo]

locationKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z")
]
meta_keys = [m_item[0] for m_item in locationKeyList]

GlobalAttrs = {
    'odb_version': 1,
    'converter': os.path.basename(__file__),
    'ioda_version': 2,
    'description': "Sea-surface temperature from GHRRST Data Specification (GDS2.0) L2/3 formatted"
}

# This list of variables is from the input file.

incoming_vars = ['quality_level',
                 'sses_bias',
                 'sses_standard_deviation',
                 'sea_surface_temperature']

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = 999.0   # More typically nc.default_fillvals['f4']
int_missing_value = nc.default_fillvals['i4']
double_missing_value = nc.default_fillvals['f8']
long_missing_value = nc.default_fillvals['i8']
string_missing_value = '_'

missing_vals = {'string': string_missing_value,
                'integer': int_missing_value,
                'long': long_missing_value,
                'float': float_missing_value,
                'double': double_missing_value}
dtypes = {'string': object,
          'integer': np.int32,
          'long': np.int64,
          'float': np.float32,
          'double': np.float64}


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

    print(f"Reading input file: {input_file}")
    ncd = nc.Dataset(input_file, 'r')

    # get the base time (should only have 1 or 2 time slots)
    time_base = ncd.variables['time'][:]
    basetime = dateutil.parser.parse(ncd.variables['time'].units[-20:])

    # get some of the global attributes that we are interested in

    for v in ('platform', 'sensor', 'processing_level'):
        GlobalAttrs[v] = ncd.getncattr(v)

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
    if GlobalAttrs['processing_level'][:2] == 'L3':
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

    # Determine the observed time.
    data_in['sst_dtime'] = ncd.variables['sst_dtime'][:].ravel()[mask]

    # load in all the other data and apply the missing value mask
    for v in incoming_vars:
        if v not in data_in:
            data_in[v] = ncd.variables[v][:].ravel()[mask]
    ncd.close()

    # Create a mask for optional random thinning
    np.random.seed(
        int((global_config['date']-datetime(1970, 1, 1)).total_seconds()))
    mask = np.random.uniform(size=len(lons)) > global_config['thin']

    # also, sometimes certain input variables have their own mask due to
    # missing values
    for v in incoming_vars:
        if np.ma.is_masked(data_in[v]):
            mask = np.logical_and(mask, np.logical_not(data_in[v].mask))

    # apply the masks for thinning and missing values
    time = time[mask]
    lons = lons[mask]
    lats = lats[mask]
    for v in incoming_vars:
        data_in[v] = data_in[v][mask]

    # create a string version of the date for each observation
    dates = []
    data_in['sst_dtime'] = data_in['sst_dtime'][mask]
    for i in range(len(lons)):
        obs_date = basetime + timedelta(seconds=float(time[i]+data_in['sst_dtime'][i]))
        obs_date = int((obs_date - epoch).total_seconds())
        dates.append(np.int64(obs_date))

    # calculate output values
    # Note: the qc flags in GDS2.0 run from 0 to 5, with higher numbers
    # being better. IODA typically expects 0 to be good, and higher numbers
    # are bad, so the qc flags flipped here.
    val_sst_skin = data_in['sea_surface_temperature']
    val_sst = val_sst_skin - data_in['sses_bias']
    err = data_in['sses_standard_deviation']
    qc = 5 - data_in['quality_level']

    obs_data = {}

    # First, populate the MetaData.
    obs_data[('dateTime', metaDataName)] = dates
    obs_data[('latitude', metaDataName)] = lats
    obs_data[('longitude', metaDataName)] = lons

    # Next, populate the observed variables.
    if global_config['output_sst']:
        obs_data[('sst', obsValName)] = val_sst.astype('float32')
        obs_data[('sst', obsErrName)] = err.astype('float32')
        obs_data[('sst', qcName)] = qc.astype('int32')

    if global_config['output_skin_sst']:
        obs_data[('skin_sst', obsValName)] = val_sst_skin.astype('float32')
        obs_data[('skin_sst', obsErrName)] = err.astype('float32')
        obs_data[('skin_sst', qcName)] = qc.astype('int32')

    return (obs_data, GlobalAttrs)


def IODA(filename, GlobalAttrs, nlocs, obs_data):

    DimDict = {'Location': nlocs}
    varDims = {}
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    # Set units and FillValue attributes for groups associated with observed variable.
    for key in var_keys:
        if (key, obsValName) in obs_data:
            var_name = varInfo[var_keys.index(key)][1]
            varDims[var_name] = 'Location'
            varAttrs[(var_name, obsValName)]['units'] = varInfo[var_keys.index(key)][2]
            varAttrs[(var_name, obsErrName)]['units'] = varInfo[var_keys.index(key)][2]
            varAttrs[(var_name, obsValName)]['_FillValue'] = varInfo[var_keys.index(key)][3]
            varAttrs[(var_name, obsErrName)]['_FillValue'] = varInfo[var_keys.index(key)][3]
            varAttrs[(var_name, qcName)]['_FillValue'] = int_missing_value

            data[(var_name, obsValName)] = np.array(obs_data[(key, obsValName)], dtype=np.float32)
            data[(var_name, obsErrName)] = np.array(obs_data[(key, obsErrName)], dtype=np.float32)
            data[(var_name, qcName)] = np.array(obs_data[(key, qcName)], dtype=np.int32)

            print(f"DEBUG: {var_name}")
            for n in range(len(data[(var_name, obsValName)])):
                print(f"  data: {data[(var_name, obsValName)][n]}, {data[(var_name, obsErrName)][n]}, {data[(var_name, qcName)][n]}")

    # Set units of the MetaData variables and all _FillValues.
    for key in meta_keys:
        dtypestr = locationKeyList[meta_keys.index(key)][1]
        if locationKeyList[meta_keys.index(key)][2]:
            varAttrs[(key, metaDataName)]['units'] = locationKeyList[meta_keys.index(key)][2]
        varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]
        data[(key, metaDataName)] = np.array(obs_data[(key, metaDataName)], dtype=dtypes[dtypestr])

        print(f"DEBUG: meta, {key}")
        for n in range(len(data[(key, metaDataName)])):
            print(f"  data: {data[(key, metaDataName)][n]}")

    # Initialize the writer, then write the file.
    writer = iconv.IodaWriter(filename, locationKeyList, DimDict)
    writer.BuildIoda(data, varDims, varAttrs, GlobalAttrs)

    return


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

    # Setup the configuration that is passed to each worker process
    # Note: Pool.map creates separate processes, and can only take iterable
    # objects. Rather than using global variables, embed them into
    # the iterable object together with argument array passed in (args.input)
    global_config = {}
    global_config['date'] = args.date
    global_config['thin'] = args.thin
    global_config['output_sst'] = args.sst
    global_config['output_skin_sst'] = args.skin_sst
    pool_inputs = [(i, global_config) for i in args.input]

    # read / process files in parallel
    pool = Pool(args.threads)
    obs = pool.map(read_input, pool_inputs)

    # concatenate the data from the files
    obs_data, GlobalAttrs = obs[0]

    # Add additional global attributes we want to output in the file.
    GlobalAttrs['sourceFiles'] = ", ".join(args.input)
    GlobalAttrs['datetimeReference'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    GlobalAttrs['thinning'] = args.thin

    for i in range(1, len(obs)):
        for k in obs_data:
            axis = len(obs[i][0][k].shape)-1
            obs_data[k] = np.concatenate(
                (obs_data[k], obs[i][0][k]), axis=axis)

    # Total number of observations.
    nlocs = len(obs_data[('dateTime', metaDataName)])

    print (f"Preparing to write {nlocs} observations to {args.output}")

    # Write out the file.
    IODA(args.output, GlobalAttrs, nlocs, obs_data)


if __name__ == '__main__':
    main()
