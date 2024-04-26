#!/usr/bin/env python3

#
# (C) Copyright 2019-2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import argparse
from datetime import datetime, timedelta
import dateutil.parser
from concurrent.futures import ProcessPoolExecutor
import numpy as np
import os
from itertools import repeat
import netCDF4 as nc

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.def_jedi_utils import epoch, ioda_int_type, ioda_float_type
from pyiodaconv.orddicts import DefaultOrderedDict

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

def main(args):
    RO_files = args.input
    print(f'{len(RO_files)} files to read')
    RO_number = 0
    for ifile in RO_files:
 
        ds = nc.Dataset(ifile, "r")
        # ds.variables.keys()
        # ['time', 'TEC', 'S4', 'elevation', 'caL1_SNR', 'pL2_SNR', 'x_LEO', 'y_LEO', 'z_LEO', 'x_GPS', 'y_GPS', 'z_GPS']
        # ds.ncattrs()
        x_leo = np.array(ds['x_LEO'][:])        # LEO x position (ECF) at time of signal reception
        y_leo = np.array(ds['y_LEO'][:])
        z_leo = np.array(ds['z_LEO'][:])
        x_gps = np.array(ds['x_GPS'][:])        # GPS x position (ECF) at time of signal transmission
        y_gps = np.array(ds['y_GPS'][:])
        z_gps = np.array(ds['z_GPS'][:])
        tec = np.array(ds['TEC'][:])            # Total Electron Content along LEO-GPS link
        elev = np.array(ds['elevation'][:])     # Elevation angle of LEO-GPS link
 
        RO_ind = np.zeros(x_leo.size) + RO_number
        unique_number = np.full(x_leo.size, str(ds.leo_id) + '_' + str(ds.prn_id))

        profile_meta_data = get_meta_data(bufr)

        if RO_number == 0:
            ax_leo = x_leo
            ay_leo = y_leo
            az_leo = z_leo
            ax_gps = x_gps
            ay_gps = y_gps
            az_gps = z_gps
            atec = tec
            aelev = elev
            aUT_RO = UT
            aRO_ind = RO_ind
            aunique_number = unique_number
        else:
            ax_leo = np.concatenate((ax_leo, x_leo), axis=None)
            ay_leo = np.concatenate((ay_leo, y_leo), axis=None)
            az_leo = np.concatenate((az_leo, z_leo), axis=None)
            ax_gps = np.concatenate((ax_gps, x_gps), axis=None)
            ay_gps = np.concatenate((ay_gps, y_gps), axis=None)
            az_gps = np.concatenate((az_gps, z_gps), axis=None)
            atec = np.concatenate((atec, tec), axis=None)
            aelev = np.concatenate((aelev, elev), axis=None)
            aUT_RO = np.concatenate((aUT_RO, UT), axis=None)
            aRO_ind = np.concatenate((aRO_ind, RO_ind), axis=None)
            aunique_number = np.concatenate((aunique_number, unique_number), axis=None)
        print(RO_number, np.shape(ax_leo))
 
        RO_number += 1

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
    VarAttrs[('totalElectronContent', 'ObsValue')]['units'] = 'Unknown'
    VarAttrs[('totalElectronContent', 'ObsError')]['units'] = 'Unknown'
    VarAttrs[('elevationAngleGNSSlink', 'MetaData')]['units'] = 'degree'
    VarAttrs[('GNSSxECFPosition', 'MetaData')]['units'] = 'km'
    VarAttrs[('GNSSyECFPosition', 'MetaData')]['units'] = 'km'
    VarAttrs[('GNSSzECFPosition', 'MetaData')]['units'] = 'km'
    VarAttrs[('SATxECFPosition', 'MetaData')]['units'] = 'km'
    VarAttrs[('SATyECFPosition', 'MetaData')]['units'] = 'km'
    VarAttrs[('SATzECFPosition', 'MetaData')]['units'] = 'km'
    VarAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string

    VarAttrs[('totalElectronContent', 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[('totalElectronContent', 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[('totalElectronContent', 'PreQC')]['_FillValue'] = int_missing_value

    VarAttrs[('elevationAngleGNSSlink', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('GNSSxECFPosition', 'MetaData')]['_FillValue'] = float_missing_value 
    VarAttrs[('GNSSyECFPosition', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('GNSSzECFPosition', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('SATxECFPosition', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('SATyECFPosition', 'MetaData')]['_FillValue'] = float_missing_value
    VarAttrs[('SATzECFPosition', 'MetaData')]['_FillValue'] = float_missing_value

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_meta_data(ds):

    # get some of the global attributes that we are interested in
    meta_data_keys = def_meta_data()

    # these are the MetaData we are interested in
    profile_meta_data = {}
    psize = len(ds['time'])
    for k, v in meta_data_keys.items():
        try:
            profile_meta_data[k] = getattr(ds, v)
        except Exception as e:
            print(f'  WARNING: could not retrieve key: {k} -- Skipping')
        if type(v) is np.int64:
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, psize), dtype=np.int64)
        elif type(v) is int:
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, psize), dtype=ioda_int_type)
        elif type(v) is float:
            obs_data[(k, 'MetaData')] = np.array(np.repeat(v, psize), dtype=ioda_float_type)
        else:  # something else (what do we do with it)
            print(f"Found neither float nor in, type={type(v)}; skipping")

    time_offset = np.array(ds['time'].add_offset - ds['time'][:])
    profile_meta_data['dateTime'] = np.int64(time_offset)

    return profile_meta_data


def def_meta_data():

    meta_data_keys = {
#       "elevationAngleGNSSlink": 'elevation',
#       "GNSSxECFPosition": 'x_GPS',
#       "GNSSyECFPosition": 'y_GPS',
#       "GNSSzECFPosition": 'z_GPS',
#       "SATxECFPosition": 'x_GPS',
#       "SATyECFPosition": 'y_GPS',
#       "SATzECFPosition": 'z_GPS',
        "satelliteTransmitterId": 'prn_id',
#       "satelliteConstellationRO": 'satelliteClassification',
    }

    return meta_data_keys


def def_meta_types():

    meta_data_types = {
        "dateTime": "long",
        "elevationAngleGNSSlink": "float",
        "GNSSxECFPosition": "float",
        "GNSSyECFPosition": "float",
        "GNSSzECFPosition": "float",
        "SATxECFPosition": "float",
        "SATyECFPosition": "float",
        "SATzECFPosition": "float",
        "satelliteIdentifier": 'integer',
        "satelliteSubIdentifier": 'integer',
        "satelliteInstrument": 'integer',
        "satelliteTransmitterId": 'integer',
        "satelliteConstellationRO": 'integer',
    }

    return meta_data_types


if __name__ == "__main__":

    # Get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads the GNSS TEC data from netCDF file'
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
