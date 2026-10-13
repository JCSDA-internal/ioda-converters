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
import time

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
    window = args.time_window * 60  # window time in seconds
    seqStart = args.sequence
    print(f'{len(files)} files to read')
    obs_data = {}
    for ifile in files:
        ds = h5py.File(ifile)

        if 'hdf' in ifile:
            times = ds['Data']['Table Layout']['ut1_unix']
        else:
            print(f'File {ifile} not supported')
            continue

        ctime = times[0]
        while ctime <= times[-1]:
            wbegin = ctime
            wend = ctime + window

            tindex = (times > wbegin) & (times <= wend)
            obs_data = get_obs_data(ds, tindex, seqStart)

            if not obs_data:
                print(f"INFO: non-nominal file skipping")
                continue

            if obs_data[('dateTime', 'MetaData')][-1] < wend and ifile != files[-1]:
                part_one = obs_data.copy()
                seqStart = obs_data['sequenceNumber'][-1] + 1
                continue
            if ifile != files[0] and part_one:
                for k in part_one.keys():
                    obs_data[k] = np.append(part_one[k], obs_data[k])

            # prepare global attributes we want to output in the file,
            # in addition to the ones already loaded in from the input file
            GlobalAttrs = {}
            dtg = datetime.fromtimestamp(ctime + int(window / 2))
            GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
            date_time = np.array(int(dtg.strftime("%Y%m%d%H%M")), dtype=str)
            GlobalAttrs['date_time'] = date_time.item()

            GlobalAttrs['converter'] = os.path.basename(__file__)

            # pass parameters to the IODA writer
            VarDims = {
                'totalElectronContent': ['Location'],
            }

            # write them out
            nlocs = obs_data[('totalElectronContent', 'ObsValue')].shape[0]
            DimDict = {'Location': nlocs}
            output = f'{args.output}_{date_time}.nc4'
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
            print(f'writing {output}')
            writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)

            ctime = ctime + window


def get_meta_data(ds, tindex, seqStart):

    # these are the MetaData we are interested in
    meta_data = {}

    meta_data['stationLatitude'] = ds['Data']['Table Layout'][tindex]['gdlatr']
    meta_data['stationLongitude'] = ds['Data']['Table Layout'][tindex]['gdlonr']
    meta_data['piercePointLatitude'] = ds['Data']['Table Layout'][tindex]['gdlat']
    meta_data['piercePointLongitude'] = ds['Data']['Table Layout'][tindex]['glon']
    meta_data['piercePointAltitude'] = ds['Data']['Table Layout'][tindex]['pierce_alt']
    meta_data['elevationAngle'] = ds['Data']['Table Layout'][tindex]['elm']
    meta_data['azimuthAngle'] = ds['Data']['Table Layout'][tindex]['azm']
    meta_data['satelliteSubID'] = ds['Data']['Table Layout'][tindex]['sat_id']
    meta_data['dateTime'] = ds['Data']['Table Layout'][tindex]['ut1_unix']
    gnss_type = ds['Data']['Table Layout'][tindex]['gnss_type']
    meta_data['satelliteID'] = np.zeros(len(gnss_type))
    gps = np.where(gnss_type == b'GPS     ')
    glonass = np.where(gnss_type == b'GLONASS ')
    meta_data['satelliteID'][gps] = 401
    meta_data['satelliteID'][glonass] = 401
    meta_data['sequenceNumber'] = np.arange(seqStart, len(gnss_type) + seqStart, 1)

    meta_data['stationLatitude'] = np.asarray(meta_data['stationLatitude'], dtype=ioda_float_type)
    meta_data['stationLongitude'] = np.asarray(meta_data['stationLongitude'], dtype=ioda_float_type)
    meta_data['piercePointLatitude'] = np.asarray(meta_data['piercePointLatitude'], dtype=ioda_float_type)
    meta_data['piercePointLongitude'] = np.asarray(meta_data['piercePointLongitude'], dtype=ioda_float_type)
    meta_data['piercePointAltitude'] = np.asarray(meta_data['piercePointAltitude'], dtype=ioda_float_type)
    meta_data['elevationAngle'] = np.asarray(meta_data['elevationAngle'], dtype=ioda_float_type)
    meta_data['azimuthAngle'] = np.asarray(meta_data['azimuthAngle'], dtype=ioda_float_type)
    meta_data['dateTime'] = np.asarray(meta_data['dateTime'], dtype=np.int64)
    meta_data['sequenceNumber'] = np.asarray(meta_data['sequenceNumber'], dtype=np.int64)

    return meta_data


def get_obs_data(ds, tindex, seqStart):
    # allocate space for output depending on which variables are to be saved
    obs_data = {}

    meta_data = get_meta_data(ds, tindex, seqStart)
    for k in meta_data.keys():
        obs_data[(k, 'MetaData')] = meta_data[k]

    obs_data[("totalElectronContent", "ObsValue")] = ds['Data']['Table Layout'][tindex]['los_tec']
    obs_data[("totalElectronContent", "ObsError")] = ds['Data']['Table Layout'][tindex]['dlos_tec']

    obs_data[("totalElectronContent", "ObsValue")] = np.asarray(obs_data[("totalElectronContent", "ObsValue")], dtype=ioda_float_type)
    obs_data[("totalElectronContent", "ObsError")] = np.asarray(obs_data[("totalElectronContent", "ObsError")], dtype=ioda_float_type)

    obs_data[("totalElectronContent", "ObsValue")][np.isnan(obs_data[("totalElectronContent", "ObsValue")])] = float_missing_value
    obs_data[("totalElectronContent", "ObsError")][np.isnan(obs_data[("totalElectronContent", "ObsError")])] = float_missing_value

    obs_data[("latitude", "MetaData")] = meta_data["piercePointLatitude"]
    obs_data[("longitude", "MetaData")] = meta_data["piercePointLongitude"]

    return obs_data


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
        '--time-window',
        help="Number of minutes to output to file. Default 60 minutes",
        type=int, default=60)
    optional.add_argument(
        '--sequence',
        help="Value to use as first sequence number."
             "SequenceNumber variable will count up by one from this number."
             "Default 1",
        type=int, default=1)

    args = parser.parse_args()
    main(args)
