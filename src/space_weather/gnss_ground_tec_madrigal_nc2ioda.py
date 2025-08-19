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
    print(f'{len(files)} files to read')
    obs_data = {}
    for ifile in files:
        ds = h5py.File(ifile)

        times = len(ds['timestamps'][:])
        for eindex in range(12, times + 1, 12):  # 12 5-minute increments per hour
            obs_data = get_obs_data(ds, (eindex - 12, eindex))
            if not obs_data:
                print(f"INFO: non-nominal file skipping")
                continue

            # prepare global attributes we want to output in the file,
            # in addition to the ones already loaded in from the input file
            GlobalAttrs = {}
            dtg = datetime.utcfromtimestamp(ds['timestamps'][eindex - 6])
            GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
            date_time_int32 = np.array(int(dtg.strftime("%Y%m%d%H")), dtype='int32')
            GlobalAttrs['date_time'] = date_time_int32.item()
        
            GlobalAttrs['converter'] = os.path.basename(__file__)
        
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
            output = f'{args.output}_{date_time_int32}.nc4'
            writer = iconv.IodaWriter(output, locationKeyList, DimDict)
            VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
            VarAttrs[('totalElectronContent', 'ObsValue')]['units'] = 'TECU'
            VarAttrs[('totalElectronContent', 'ObsError')]['units'] = 'TECU'
            VarAttrs[('latitude', 'MetaData')]['units'] = 'degree'
            VarAttrs[('longitude', 'MetaData')]['units'] = 'degree'
            VarAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string
        
            VarAttrs[('totalElectronContent', 'ObsValue')]['_FillValue'] = float_missing_value
            VarAttrs[('totalElectronContent', 'ObsError')]['_FillValue'] = float_missing_value
        
            VarAttrs[('latitude', 'MetaData')]['_FillValue'] = float_missing_value
            VarAttrs[('longitude', 'MetaData')]['_FillValue'] = float_missing_value
        
            # final write to IODA file
            writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_meta_data(ds, indices):

    # these are the MetaData we are interested in
    meta_data = {}

    lats = ds['gdlat'][:]
    lons = ds['glon'][:]
    times = []
    for ind in range(indices[0], indices[1], 1):
        times.append(datetime.utcfromtimestamp(ds['timestamps'][ind]).strftime("%Y%m%d%H"))

    times3d, lats3d, lons3d = np.meshgrid(times, lats, lons)

    meta_data['latitude'] = lats3d.ravel()
    meta_data['longitude'] = lons3d.ravel()
    meta_data['dateTime'] = times3d.ravel()

    meta_data['latitude'] = np.asarray(meta_data['latitude'], dtype=ioda_float_type)
    meta_data['longitude'] = np.asarray(meta_data['longitude'], dtype=ioda_float_type)
    meta_data['dateTime'] = np.asarray(meta_data['dateTime'], dtype=np.int64)

    return meta_data


def get_obs_data(ds, indices):
    # allocate space for output depending on which variables are to be saved
    obs_data = {}

    profile_meta_data = get_meta_data(ds, indices)
    for k in profile_meta_data.keys():
        obs_data[(k, 'MetaData')] = profile_meta_data[k]

    obs_data[("totalElectronContent", "ObsValue")] = ds['tec'][indices[0]:indices[1], :, :].ravel()
    obs_data[("totalElectronContent", "ObsError")] = ds['dtec'][indices[0]:indices[1], :, :].ravel()

    obs_data[("totalElectronContent", "ObsValue")] = np.asarray(obs_data[("totalElectronContent", "ObsValue")], dtype=ioda_float_type)
    obs_data[("totalElectronContent", "ObsError")] = np.asarray(obs_data[("totalElectronContent", "ObsError")], dtype=ioda_float_type)

    obs_data[("totalElectronContent", "ObsValue")][np.isnan(obs_data[("totalElectronContent", "ObsValue")])] = float_missing_value
    obs_data[("totalElectronContent", "ObsError")][np.isnan(obs_data[("totalElectronContent", "ObsError")])] = float_missing_value
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
            'Reads the Ground-based GNSS TEC data from gridded netCDF files as downloaded from Madrigal'
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
        help="full path and IODA output file name base (not including date or '.nc4')
              Should be given as /path/to/file/base and files will be saved as /path/to/file/base_date.nc4",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')

    args = parser.parse_args()
    main(args)
