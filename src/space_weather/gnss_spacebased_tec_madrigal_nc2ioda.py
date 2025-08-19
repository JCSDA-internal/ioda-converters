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
    RO_files = args.input
    print(f'{len(RO_files)} files to read')
    obs_data = {}
    # for file_obs_data in executor.map(read_input, pool_inputs, repeat(qc), repeat(addLSW), repeat(only_bang)):
    for ifile in RO_files:
        file_obs_data = get_obs_data(ifile, args)
        if not file_obs_data:
            print(f"INFO: non-nominal file skipping")
            continue
        if obs_data:
            file_obs_data[('sequenceNumber', 'MetaData')] += 1
            concat_obs_dict(obs_data, file_obs_data)
        else:
            obs_data = file_obs_data

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    GlobalAttrs = {}
    if args.date:
        dtg = datetime.strptime(args.date, '%Y%m%d%H')
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
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('totalElectronContent', 'ObsValue')]['units'] = 'TECU'
    VarAttrs[('totalElectronContent', 'ObsError')]['units'] = 'TECU'
    VarAttrs[('latitude', 'MetaData')]['units'] = 'degree'
    VarAttrs[('longitude', 'MetaData')]['units'] = 'degree'
    VarAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string

    VarAttrs[('totalElectronContent', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('totalElectronContent', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('totalElectronContent', 'PreQC')]['_FillValue'] = int_missing_value

    VarAttrs[('latitude', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('longitude', 'MetaData')]['_FillValue'] = float_missing_value

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_meta_data(ds):

    # these are the MetaData we are interested in
    profile_meta_data = {}
    psize = len(ds['Data']['Table Layout'][:])

    # bespoke table of letter to WMO code
    transmitterConstellationId = get_GNSS_constellation(str(ds['Metadata']['Experiment Parameters'][2][1]).split('\'')[1])
    profile_meta_data['satelliteConstellationRO'] = np.array(np.repeat(transmitterConstellationId, psize), dtype=ioda_int_type)

    profile_meta_data['latitude'] = []
    profile_meta_data['longitude'] = []
    profile_meta_data['dateTime'] = []
    for i in range(psize):
        profile_meta_data['latitude'].append(ds['Data']['Table Layout'][i][11])
        profile_meta_data['longitude'].append(ds['Data']['Table Layout'][i][12])

        # the time convert to epoch and handle array of values
        profile_meta_data['dateTime'].append(datetime(ds['Data']['Table Layout'][i][0], ds['Data']['Table Layout'][i][1], ds['Data']['Table Layout'][i][2], ds['Data']['Table Layout'][i][3], ds['Data']['Table Layout'][i][4], ds['Data']['Table Layout'][i][5]).timestamp())

    profile_meta_data['latitude'] = np.asarray(profile_meta_data['latitude'], dtype=ioda_float_type)
    profile_meta_data['longitude'] = np.asarray(profile_meta_data['longitude'], dtype=ioda_float_type)
    profile_meta_data['dateTime'] = np.asarray(profile_meta_data['dateTime'], dtype=np.int64)

    return profile_meta_data


def get_obs_data(ifile, get_obs_data_args):
    # allocate space for output depending on which variables are to be saved
    obs_data = {}

    ds = h5py.File(ifile)

    profile_meta_data = get_meta_data(ds)
    psize = len(profile_meta_data['longitude'])
    for k in profile_meta_data.keys():
        obs_data[(k, 'MetaData')] = profile_meta_data[k]

    obs_data[('sequenceNumber', 'MetaData')] = []
    obs_data[("totalElectronContent", "ObsValue")] = []
    for i in range(psize):
        # number to keep track of profile
        obs_data[('sequenceNumber', 'MetaData')].append(ds['Data']['Table Layout'][i][6])
        # the observation value
        obs_data[("totalElectronContent", "ObsValue")].append(ds['Data']['Table Layout'][i][13])

    obs_data[('sequenceNumber', 'MetaData')] = np.asarray(obs_data[('sequenceNumber', 'MetaData')])
    obs_data[("totalElectronContent", "ObsValue")] = np.asarray(obs_data[("totalElectronContent", "ObsValue")])

    return obs_data


def def_meta_types():

    meta_data_types = {
        "latitude": "float",
        "longitude": "float",
        "dateTime": "long",
        "satelliteConstellationRO": 'integer',
    }

    return meta_data_types


def get_GNSS_constellation(constellationId):
    # convert letter codes to WMO constellation ID
    if constellationId == 'Sentinel 6 TEC':
        transmitterConstellationId = 401
    else:
        transmitterConstellationId = int_missing_value
    return transmitterConstellationId


def get_GNSS_mission(ds):
    # return WMO satellite ID
    try:
        mission = ds.mission
    except Exception as e:
        return int_missing_value

    satID = int_missing_value
    if mission == 'COSEQ':
        satID = 749 + ds.leo_id
    return satID


if __name__ == "__main__":

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the Space-based GNSS TEC data from netCDF files as downloaded from Madrigal'
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of GNSS TEC observation input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="full path and name of IODA output file",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, required=False, default=None)
    optional.add_argument(
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             '(default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '-r', '--recordnumber',
        help=' optional record number to associate with profile ',
        type=int, default=1)

#   optional.add_argument(
#       '-q', '--qualitycontrol',
#       help='turn on quality control georeality checks',
#       default=False, action='store_true', required=False)

    args = parser.parse_args()
    main(args)
