#!/usr/bin/env python3

#
# (C) Copyright 2022 UCAR
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

IODA_CONV_PATH = Path(__file__).parent/"../lib/pyiodaconv"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"
# The setting of basetime is temporary as it will get modified in the data reading routine.
basetime = datetime.fromisoformat('1970-01-01T00:00:00')

output_var_names = ["radiance"]

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]

GlobalAttrs = {
    'odb_version': 1,
    'converter': os.path.basename(__file__)
}

VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

DimDict = {}

VarDims = {}

chan_number = range(1, 250)  # we have 120 Blue band 120 Red band and 9 SWRI


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
    data_in = {}
    input_file = input_args[0]
    global_config = input_args[1]
    ncd = nc.Dataset(input_file, 'r')
    # get the base time (should only have 1 or 2 time slots)
    time_base = ncd.groups['scan_line_attributes'].variables['time'][:]
    global basetime   # We wish to overrride the initial setting of basetime.
    basetime = dateutil.parser.parse(ncd.groups['scan_line_attributes'].variables['time'].units[-20:])

    # Determine the lat/lon grid.
    lons = ncd.groups['geolocation_data'].variables['longitude'][:].ravel()
    lats = ncd.groups['geolocation_data'].variables['latitude'][:].ravel()
    pixels = ncd.groups['geolocation_data'].variables['longitude'][:].shape[1]

    # calculate the time
    time = np.tile((time_base), (pixels))

    # load in all the other data
    geo_vars = (
        'solar_zenith',
        'sensor_zenith',
        'sensor_azimuth',
        'solar_azimuth')

    band_vars = (
        'blue_wavelength',
        'red_wavelength',
        'SWIR_wavelength')

    obs_vars = (
        'Lt_blue',
        'Lt_red',
        'Lt_SWIR')

    input_vars = geo_vars + band_vars + obs_vars

    for v in geo_vars:
        if v not in data_in:
            data_in[v] = ncd.groups['geolocation_data'].variables[v][:].ravel()

    for v in band_vars:
        if v not in data_in:
            data_in[v] = ncd.groups['sensor_band_parameters'].variables[v][:].ravel()

    for v in obs_vars:
        if v not in data_in:
            data_in[v] = ncd.groups['observation_data'].variables[v][:]

    ncd.close()

    # Create a mask for optional random thinning
    mask = np.random.uniform(size=len(lons)) > global_config['thin']

    # also, sometimes certain input variables have their own mask due to
    # missing values
    for v in geo_vars:
        if np.ma.is_masked(data_in[v]):
            mask = np.logical_and(mask, np.logical_not(data_in[v].mask))

    time = time[mask]
    lons = lons[mask]
    lats = lats[mask]

    # create a string version of the date for each observation
    dates = np.empty(len(lons), dtype=np.int64)
    for i in range(len(lons)):
        dates[i] = round(time[i])

    # output values
    nchans = len(chan_number)
    obs_dim = (len(lons))
    val_radiance = np.concatenate((data_in['Lt_blue'], data_in['Lt_red'], data_in['Lt_SWIR']), axis=0)
    val_radiance = np.reshape(val_radiance, (val_radiance.shape[0], val_radiance.shape[1]*val_radiance.shape[2]))
    val_radiance = val_radiance.T

    # as there is not any obs error in data  we use the same obs error for all chans for now
    err = np.zeros((obs_dim, nchans))+0.5

    # the quality flaq is not developed for this data set yet, we need to change this part when
    # they add the qC to the obs data
    qc = np.zeros((obs_dim, nchans))

    wavelength = np.concatenate((data_in['blue_wavelength'], data_in['red_wavelength'], data_in['SWIR_wavelength']), axis=0)

    # allocate space for output depending on which variables are to be saved

    obs_data = {}
    obs_data[('dateTime', 'MetaData')] = dates
    obs_data[('latitude', 'MetaData')] = lats
    obs_data[('longitude', 'MetaData')] = lons
    obs_data[('height', 'MetaData')] = np.zeros((obs_dim), dtype=np.float32)
    obs_data[('sensorAzimuthAngle', 'MetaData')] = data_in['sensor_azimuth'].astype('float32')
    obs_data[('sensorZenithAngle', 'MetaData')] = data_in['sensor_zenith'].astype('float32')
    obs_data[('sensorViewAngle', 'MetaData')] = data_in['sensor_zenith'].astype('float32')
    obs_data[('solarZenithAngle', 'MetaData')] = data_in['solar_zenith'].astype('float32')
    obs_data[('solarAzimuthAngle', 'MetaData')] = data_in['solar_azimuth'].astype('float32')
    obs_data[('sensorCentralWavenumber', 'MetaData')] = wavelength.astype('float32')
    obs_data[output_var_names[0], global_config['oval_name']] = val_radiance.astype('float32')
    obs_data[output_var_names[0], global_config['oerr_name']] = err.astype('float32')
    obs_data[output_var_names[0], global_config['opqc_name']] = qc.astype('int32')

    return (obs_data, GlobalAttrs)


def main():

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the radiance from any PACE L1B Data '
            ' and converts into IODA formatted output files.'
            ' Multiple files are concatenated ')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="Path to a  pace L1B observation input file",
        type=str, nargs=1, required=True)
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

    # concatenate the data from the files
    obs_data = read_input((args.input[0], global_config))[0]

    for i in range(1, len(args.input)):
        for k in obs_data:
            obs_data[k] = np.concatenate(
                (obs_data[k], args.input[i]), axis=0)

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs['datetimeReference'] = args.date.strftime("%Y-%m-%dT%H:%M:%SZ")
    GlobalAttrs['thinning'] = args.thin

    # determine which variables we are going to output
    selected_names = []
    selected_names.append(output_var_names[0])

    # pass parameters to the IODA writer
    # (needed because we are bypassing ExtractObsData within BuildNetcdf)
    VarDims = {
        'radiance': ['Location', 'Channel'],
        'sensorCentralWavenumber': ['Channel']
    }

    nchans = len(chan_number)
    nlocs = len(obs_data[('longitude', 'MetaData')])
    DimDict = {
        'Location': nlocs,
        'Channel': list(chan_number),
    }

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs[('dateTime', 'MetaData')]['units'] = 'seconds since ' + basetime.strftime("%Y-%m-%dT%H:%M:%SZ")
    VarAttrs[('radiance', 'ObsValue')]['units'] = 'W m-2 sr-1'
    VarAttrs[('radiance', 'ObsError')]['units'] = 'W m-2 sr-1'
    VarAttrs[('radiance', 'ObsValue')]['_FillValue'] = -32767
    VarAttrs[('radiance', 'ObsError')]['_FillValue'] = 999
    VarAttrs[('radiance', 'PreQC')]['_FillValue'] = 999
    for k in list(obs_data.keys()):
        if 'angle' in k[0].lower():
            VarAttrs[(k[0], k[1])]['units'] = 'degree'
        elif 'wavenumber' in k[0].lower():
            VarAttrs[(k[0], k[1])]['units'] = 'm-1'
        elif k[0] == 'height':
            VarAttrs[(k[0], k[1])]['units'] = 'm'

    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
