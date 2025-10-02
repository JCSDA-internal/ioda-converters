#!/usr/bin/env python3

#
# (C) Copyright 2019-2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import argparse
from datetime import datetime
import dateutil.parser
from concurrent.futures import ProcessPoolExecutor
import numpy as np
import os
from itertools import repeat
import h5py
from datetime import datetime

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.def_jedi_utils import epoch, iso8601_string, ioda_int_type, ioda_float_type, concat_obs_dict
from pyiodaconv.orddicts import DefaultOrderedDict

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

locationKeyList = [
    ('latitude', 'float'),
    ('longitude', 'float'),
    ("dateTime", "long")
]


def main(args):
    files = args.input
    inc = int(args.window/5)
    print(f'{len(files)} files to read')
    obs_data = {}
    for ifile in files:
        ds = h5py.File(ifile)

        if 'nc' in ifile:
            times = len(ds['timestamps'][:])
            file_type = 'nc'
        elif 'hdf' in ifile:
            times = len(ds['Data']['Array Layout']['timestamps'][:])
            file_type = 'hdf'
        else:
            print(f'File {ifile} not supported')
            continue

        for sindex in range(0, times, inc):
            obs_data = get_obs_data(ds, (sindex, sindex + inc), file_type)
            print('9')

            if not obs_data:
                print(f"INFO: non-nominal file skipping")
                continue

            # prepare global attributes we want to output in the file,
            # in addition to the ones already loaded in from the input file
            GlobalAttrs = {}
            if file_type =='nc':
                dtg = datetime.utcfromtimestamp(ds['timestamps'][sindex + int(inc/2)])
            else:
                print('10')
                dtg = datetime.utcfromtimestamp(ds['Data']['Array Layout']['timestamps'][sindex + int(inc/2)])

            print('11')
            GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
            date_time = np.array(int(dtg.strftime("%Y%m%d%H%M")), dtype=str)
            GlobalAttrs['date_time'] = date_time.item()
        
            GlobalAttrs['converter'] = os.path.basename(__file__)
        
            print('12')
            # pass parameters to the IODA writer
            VarDims = {
                'totalElectronContent': ['Location'],
            }
        
            # write them out
            nlocs = obs_data[('totalElectronContent', 'ObsValue')].shape[0]
            DimDict = {'Location': nlocs}
            meta_data_types = def_meta_types()
            for k, v in meta_data_types.items():
                locationKeyList.append((k, v))
            output = f'{args.output}_{date_time}.nc4'
            print('13')
            writer = iconv.IodaWriter(output, locationKeyList, DimDict)
            VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
            VarAttrs[('totalElectronContent', 'ObsValue')]['units'] = 'TECU'
            VarAttrs[('totalElectronContent', 'ObsError')]['units'] = 'TECU'
            VarAttrs[('latitude', 'MetaData')]['units'] = 'degree'
            VarAttrs[('longitude', 'MetaData')]['units'] = 'degree'
            VarAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string
            print('14')
        
            VarAttrs[('totalElectronContent', 'ObsValue')]['_FillValue'] = float_missing_value
            VarAttrs[('totalElectronContent', 'ObsError')]['_FillValue'] = float_missing_value
        
            VarAttrs[('latitude', 'MetaData')]['_FillValue'] = float_missing_value
            VarAttrs[('longitude', 'MetaData')]['_FillValue'] = float_missing_value
        
            print('15')
            # final write to IODA file
            writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)
            print('16')


def get_meta_data(ds, indices, file_type):
    print('1')
    # these are the MetaData we are interested in
    meta_data = {}

    if file_type == 'nc':
        lats = ds['gdlat'][:]
        lons = ds['glon'][:]
        times = []
        for ind in range(indices[0], indices[1], 1):
            times.append(ds['timestamps'][ind])
    else:
        lats = ds['Data']['Array Layout']['gdlat'][:]
        lons = ds['Data']['Array Layout']['glon'][:]
        times = []
        for ind in range(indices[0], indices[1], 1):
            times.append(ds['Data']['Array Layout']['timestamps'][ind])

    times3d, lats3d, lons3d = np.meshgrid(times, lats[indices[0]:indices[1]], lons[indices[0]:indices[1]])
    print('2')
    #times3d, lats3d, lons3d = np.meshgrid(times, lats, lons)

    meta_data['latitude'] = lats3d.ravel()
    meta_data['longitude'] = lons3d.ravel()
    meta_data['dateTime'] = times3d.ravel()

    print(len( meta_data['latitude']))
    meta_data['latitude'] = np.asarray(meta_data['latitude'], dtype=ioda_float_type)
    meta_data['longitude'] = np.asarray(meta_data['longitude'], dtype=ioda_float_type)
    meta_data['dateTime'] = np.asarray(meta_data['dateTime'], dtype=np.int64)

    print('3')
    return meta_data


def get_obs_data(ds, indices, file_type):
    # allocate space for output depending on which variables are to be saved
    obs_data = {}

    meta_data = get_meta_data(ds, indices, file_type)
    print('4')
    for k in meta_data.keys():
        obs_data[(k, 'MetaData')] = meta_data[k]

    print('5')
    if file_type == 'nc':
        obs_data[("totalElectronContent", "ObsValue")] = ds['tec'][indices[0]:indices[1], :, :].ravel()
        obs_data[("totalElectronContent", "ObsError")] = ds['dtec'][indices[0]:indices[1], :, :].ravel()
    else:
        obs_data[("totalElectronContent", "ObsValue")] = ds['Data']['Array Layout']['2D Parameters']['tec'][:, :, indices[0]:indices[1]].ravel()
        obs_data[("totalElectronContent", "ObsError")] = ds['Data']['Array Layout']['2D Parameters']['dtec'][:, :, indices[0]:indices[1]].ravel()

    print(len(obs_data[("totalElectronContent", "ObsValue")]))
    print('6')
    obs_data[("totalElectronContent", "ObsValue")] = np.asarray(obs_data[("totalElectronContent", "ObsValue")], dtype=ioda_float_type)
    obs_data[("totalElectronContent", "ObsError")] = np.asarray(obs_data[("totalElectronContent", "ObsError")], dtype=ioda_float_type)

    obs_data[("totalElectronContent", "ObsValue")][np.isnan(obs_data[("totalElectronContent", "ObsValue")])] = float_missing_value
    obs_data[("totalElectronContent", "ObsError")][np.isnan(obs_data[("totalElectronContent", "ObsError")])] = float_missing_value
    print('7')
    return obs_data


def def_meta_types():

    meta_data_types = {
        "latitude": "float",
        "longitude": "float",
        "dateTime": "long"
    }

    return meta_data_types


if __name__ == "__main__":

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the Ground-based GNSS TEC data from gridded netCDF or HDF5 files as downloaded from Madrigal'
            ' convert into hourly IODA formatted output files. '
            ' Multiple files can be given')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of GNSS TEC observation input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="full path and IODA output file name base (not including date or '.nc4')"
              "Should be given as /path/to/file/base and files will be saved as /path/to/file/base_date.nc4",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-w', '--window',
        help="Number of minutes to output to file. Will be rounded down to multiples of 5. Default 60 minutes",
        type=int, default=60)

    args = parser.parse_args()
    main(args)
