#!/usr/bin/env python3

#
# (C) Copyright 2023 UCAR
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
import os
from pathlib import Path

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

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


def read_input(input_args):
    """
    Reads/converts a single input file, performing optional thinning also.

    Arguments:

        input_args: A tuple of input filename and global_config
            input_file: The name of file to read
            global_config: structure for global arguments related to read

    Returns:

        A tuple of (obs_data, GlobalAttrs, chan_number) needed by the IODA writer.
    """
    data_in = {}
    input_file = input_args[0]
    global_config = input_args[1]
    ncd = nc.Dataset(input_file, 'r')

    wavelength = ncd.groups['sensor_band_parameters'].variables['wavelength'][:].ravel()
    wavelength = wavelength[wavelength < 700]  # we just have Rrs for wavelength less than 700
    chan_number = range(1, len(wavelength)+1)  # we have 10 channels

    # since OC L2 files are quite empty, need a mask applied immediately
    # to avoid using too much memory)
    Rrs_ch1 = ncd.groups['geophysical_data'].variables['Rrs_'+str(wavelength[0])][:].ravel()
    mask = Rrs_ch1 >= -3000  # min valid value

    # Determine the lat/lon grid.
    lons = ncd.groups['navigation_data'].variables['longitude'][:].ravel()[mask]
    lats = ncd.groups['navigation_data'].variables['latitude'][:].ravel()[mask]
    pixels = ncd.groups['navigation_data'].variables['longitude'][:].shape[1]
    lines = ncd.groups['navigation_data'].variables['longitude'][:].shape[0]

    # get basetime and time as a difference in seconds
    global basetime
    sla = ncd.groups['scan_line_attributes']
    basetime = datetime(sla.variables['year'][0], 1, 1) + \
        timedelta(days=int(sla.variables['day'][0]-1),
                  milliseconds=int(sla.variables['msec'][0]))
    time = (np.repeat(sla.variables['msec'][:].ravel(),
            pixels).ravel() - sla.variables['msec'][0])/1000.0
    time = time[mask]

    sol_z = ncd.groups['scan_line_attributes'].variables['csol_z'][:].ravel()
    data_in['csol_z'] = np.tile((sol_z), (pixels)).ravel()[mask]
    data_in['l2_flags'] = ncd.groups['geophysical_data'].variables['l2_flags'][:].ravel()[mask]
    val_radiance = np.zeros((len(lats), len(wavelength)))

    for i in range(len(wavelength)):
        Sorar_radiance = ncd.groups['geophysical_data'].variables['Rrs_'+str(wavelength[i])].solar_irradiance
        val_radiance[:, i] = ncd.groups['geophysical_data'].variables['Rrs_'+str(wavelength[i])][:].ravel()[mask] * Sorar_radiance

    ncd.close()

    # Create a mask for optional random thinning
    mask = np.random.uniform(size=len(lons)) > global_config['thin']

    time = time[mask]
    lons = lons[mask]
    lats = lats[mask]
    data_in['l2_flags'] = data_in['l2_flags'][mask]

    # create a string version of the date for each observation
    dates = np.empty(len(lons), dtype=np.int64)
    for i in range(len(lons)):
        dates[i] = round(time[i])

    # output values
    nchans = len(wavelength)
    obs_dim = (len(lons))

    # as there is not any obs error in data  we use the same obs error for all chans for now
    err = np.zeros((obs_dim, nchans))+0.5
    qc = np.tile((data_in['l2_flags']), (nchans, 1)).T

    # allocate space for output depending on which variables are to be saved
    obs_data = {}
    obs_data[('dateTime', 'MetaData')] = dates
    obs_data[('latitude', 'MetaData')] = lats
    obs_data[('longitude', 'MetaData')] = lons
    obs_data[('height', 'MetaData')] = np.zeros((obs_dim), dtype=np.float32)
    obs_data[('solarZenithAngle', 'MetaData')] = data_in['csol_z'].astype('float32')
    obs_data[('sensorCentralWavenumber', 'MetaData')] = wavelength.astype('float32')
    obs_data[output_var_names[0], global_config['oval_name']] = val_radiance.astype('float32')
    obs_data[output_var_names[0], global_config['oerr_name']] = err.astype('float32')
    obs_data[output_var_names[0], global_config['opqc_name']] = qc.astype('int32')

    return (obs_data, GlobalAttrs, chan_number)


def main():

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the radiance from any OC L2 Data '
            ' and converts into IODA formatted output files.'
            ' Multiple files are concatenated ')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="Path to a  MODIS L2 observation input file",
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

    chan_number = read_input((args.input[0], global_config))[2]
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
