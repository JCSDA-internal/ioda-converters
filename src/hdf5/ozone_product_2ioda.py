#!/usr/bin/env python3

#
# (C) Copyright 2020-2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#


"""
Python code to ingest netCDF4 Ozone product data
"""

import argparse
from datetime import datetime, timezone
import os.path
import re
import sys

import h5py
import numpy as np

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import ioda_int_type, ioda_float_type, epoch, iso8601_string
from pyiodaconv.def_jedi_utils import concat_obs_dict

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

# globals
SNPP_WMO_sat_ID = 224
NOAA20_WMO_sat_ID = 225
NOAA21_WMO_sat_ID = 226

# these should be defined by metaData in the file vote for these two
# commonName
# k='instrument_name' f.attrs[k]=b'OMPS'
# Description a sum of these two
# k='title' f.attrs[k]=b'V8PRO L2'
# k='summary' f.attrs[k]=b'V8PRO retrieved ozone profile, total column amount of ozone, and aerosol index'

GlobalAttrs = {
    "platformCommonName": "OMPS",
    "platformLongDescription": "V8PRO L2 - V8PRO retrieved ozone profile, total column amount of ozone, and aerosol index"
}

locationKeyList = [
    ("latitude", "float", "degree_north"),
    ("longitude", "float", "degree_east"),
    ("pressure", "float", "Pa"),
    ("dateTime", "long", iso8601_string),
]

metaDataKeyList = [
    ('satelliteIdentifier', "int", "WMO satellite identifier"),
    ('surfaceQualifier', "int", "surface classification from NOAA V8PRO"),
    ('satelliteAscendingFlag', "int", "ascending descending orbit flag")
]


def main(args):

    output_filename = args.output
    dtg = None
    if args.date:
        dtg = datetime.strptime(args.date, '%Y%m%d%H')

    input_files = [(i) for i in args.input]
    # initialize
    obs_data = {}
    # read / process files in parallel
#   with ProcessPoolExecutor(max_workers=args.threads) as executor:
#       for file_obs_data in executor.map(get_data_from_files, input_files):
#           if not file_obs_data:
#               print("INFO: non-nominal file skipping")
#               continue
#           if obs_data:
#               concat_obs_dict(obs_data, file_obs_data)
#           else:
#               obs_data = file_obs_data

    for afile in input_files:
        file_obs_data = get_data_from_files(afile, skip=args.skip)
        if not file_obs_data:
            print("INFO: non-nominal file skipping")
            continue
        if obs_data:
            concat_obs_dict(obs_data, file_obs_data)
        else:
            obs_data = file_obs_data

    nlocs_int = np.array(len(obs_data[('latitude', metaDataName)]), dtype='int64')
    nlocs = nlocs_int.item()
    # determine if profiles are in obs_data
    if 'ozoneProfile' in {key[0] for key in obs_data.keys()}:
        has_ozoneProfile = True
    else:
        has_ozoneProfile = False

    if has_ozoneProfile:
        nvertice_int = np.array(len(obs_data[('pressure', metaDataName)]), dtype='int64')
        nvertice = nvertice_int.item()
    else:
        global locationKeyList  # globals, globals everywhere
        locationKeyList = [item for item in locationKeyList if item[0] != 'pressure']

    if nlocs == 0:
        print(f'  ...  WARNING: no data found exiting without writing output')
        return

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    if dtg:
        GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    GlobalAttrs['converter'] = os.path.basename(__file__)

    # pass parameters to the IODA writer
    VarDims = {
        'ozoneColumn': ['Location'],
    }
    if has_ozoneProfile:
        VarDims['ozoneProfile'] = ['Location', 'Pressure']

    DimDict = {
        'Location': nlocs,
    }
    if has_ozoneProfile:
        DimDict['Pressure'] = nvertice

    writer = iconv.IodaWriter(output_filename, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    for k in locationKeyList + metaDataKeyList:
        VarAttrs[(k[0], metaDataName)]['units'] = k[2]
        if k[1] == 'float':
            VarAttrs[(k[0], metaDataName)]['_FillValue'] = float_missing_value
        elif k[1] == 'int':
            VarAttrs[(k[0], metaDataName)]['_FillValue'] = int_missing_value
        elif k[1] == 'long':
            VarAttrs[(k[0], metaDataName)]['_FillValue'] = long_missing_value

    varKeys = ['ozoneColumn']
    if has_ozoneProfile:
        varKeys.append('ozoneProfile')
    for k in varKeys:
        VarAttrs[(k, obsValName)]['_FillValue'] = float_missing_value
        VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
        VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
        # need to convert Dobson to ppmv? or mixing ratio?
        # Output units are mol m-2
#       VarAttrs[(k, obsValName)]['units'] = 'DU'
#       VarAttrs[(k, 'ObsError')]['units'] = 'DU'
        VarAttrs[(k, obsValName)]['units'] = 'mol m-2'
        VarAttrs[(k, 'ObsError')]['units'] = 'mol m-2'

    for k in varKeys:
        # Need  Convert data from DU to mole m-2, 1DU = 4.4615E-04 mol m-2
        val_array = obs_data[(k, obsValName)] != float_missing_value
        obs_data[(k, obsValName)][val_array] = obs_data[(k, obsValName)][val_array]*4.4615E-04
        err_array = obs_data[(k, 'ObsError')] != float_missing_value
        obs_data[(k, 'ObsError')][err_array] = obs_data[(k, 'ObsError')][err_array]*4.4615E-04

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_data_from_files(afile, skip=1):

    f = h5py.File(afile, 'r')
    product_title = f.attrs['title'].decode('utf-8')
    product_description = f.attrs['summary'].decode('utf-8')
    GlobalAttrs["platformLongDescription"] = ' - '.join([product_title, product_description])
    if 'PRO' in product_title:
        # allocate space for output depending on which variables are to be saved
        obs_data = init_obs_loc(profile=True)
        # designed with 'V8PRO L2 ozone profile product'
        obs_data = get_np_data(f, obs_data, skip=skip)
    elif 'TOZ' in product_title:
        obs_data = init_obs_loc(profile=False)
        # designed with V8TOZ_EDR ozone total column product'
        obs_data = get_tc_data(f, obs_data, skip=skip)
    f.close()

    return obs_data


def get_np_data(f, obs_data, skip=1):

    WMO_sat_ID = get_WMO_satellite_ID(f.attrs['platform_name'].decode("utf-8"))

    # possible dimensions are location, times and vertice
    dataset_float_fill = f['Latitude'].fillvalue
    dataset_int_fill = f['SurfaceCategory'].fillvalue

    # only the initial time appears to be populated
    # use an assertion to verify this is the case for data being processed
    itime = 0
#   assert not np.allclose(f['Latitude'][:, itime], dataset_float_fill), f'index {itime} has all fill_value'
    if np.all(f['Latitude'][:, itime] == dataset_float_fill):
        # rather than use assertion just return None in case file has no valid data
        print(f'time index {itime} has all fill_value')
        return None
    for i in range(1, np.shape(f['Latitude'])[-1]):
        assert np.allclose(f['Latitude'][:, i], dataset_float_fill), f'time index {i} contains some data'
    data = np.array(f['Latitude'][:, itime].flatten(), dtype=ioda_float_type)
    obs_data[('latitude', metaDataName)] = reassign_missing_values(data, dataset_missing=dataset_float_fill)
    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('longitude', metaDataName)] = np.array(f['Longitude'][:, itime].flatten(), dtype=ioda_float_type)
    obs_data[('pressure', metaDataName)] = np.array(f['Pressure'][:], dtype=ioda_float_type)
    nvertice = len(obs_data[('pressure', metaDataName)])
    data = np.array(f['SurfaceCategory'][:, itime], dtype=ioda_int_type)
    obs_data[('surfaceQualifier', metaDataName)] = reassign_missing_values(data, dataset_missing=dataset_int_fill)

    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype=ioda_int_type)
    obs_data[('dateTime', metaDataName)] = get_epoch_time(f, f['MidTime'][:, itime], timekey='MidTime')

    k = 'ozoneProfile'
    data = np.array(f['O3FINAL'][:, itime, :], dtype=ioda_float_type)
    obs_data[(k, obsValName)] = reassign_missing_values(data, dataset_missing=dataset_float_fill)
    obs_data[(k, "ObsError")] = np.full((nlocs, nvertice), 5.0, dtype=ioda_float_type)
    # f['AlgorithmFlag_TO3'][:, 0]  # do not know what the codes for these values are is 1 == good?
    obs_data[(k, "PreQC")] = np.full((nlocs, nvertice), 0, dtype=ioda_int_type)

    k = 'ozoneColumn'
    data = get_obs_total(f, k="O3FINAL", itime=itime)
    obs_data[(k, obsValName)] = reassign_missing_values(data, dataset_missing=dataset_float_fill)
    obs_data[(k, "ObsError")] = np.full((nlocs), 5.0, dtype=ioda_float_type)
    # f['AlgorithmFlag_TO3'][:, 0]  # do not know what the codes for these values are
    obs_data[(k, "PreQC")] = np.full((nlocs), 0, dtype=ioda_int_type)

    obs_data[('satelliteAscendingFlag', metaDataName)] = np.array(f['Ascending_Descending'][:, itime], dtype=ioda_int_type)

#   # check here seems to use the qc_mask
    valLimit = {}
    valLimit['ozoneProfile'] = (0., 100.)
    valLimit['ozoneColumn'] = (0., 1000.)
    for k in ['ozoneProfile', 'ozoneColumn']:
        obs_data[(k, obsValName)][np.isnan(obs_data[(k, obsValName)])] = float_missing_value
        qc_array = (
            (obs_data[(k, obsValName)] < valLimit[k][0])
            | (
                (obs_data[(k, obsValName)] > valLimit[k][1])
                & (obs_data[(k, obsValName)] != float_missing_value)
            )
        )
        obs_data[(k, obsValName)][qc_array] = float_missing_value

    return obs_data


def get_tc_data(f, obs_data, skip=1):

    WMO_sat_ID = get_WMO_satellite_ID(f.attrs['platform_name'].decode("utf-8"))

    # possible dimensions are location, times and vertice
    dataset_float_fill = f['Latitude'].fillvalue
    dataset_int_fill = f['ErrorFlag'].fillvalue
    dataset_mask_2d = f['Latitude'][:, :] != f['Latitude'].fillvalue
    # these locations coincide with ErrFlag8
    qc_mask = f['ColumnAmountO3'][dataset_mask_2d] != f['ColumnAmountO3'].fillvalue

    # dimensions
    nIFOV = np.shape(f['nIFOV'])[0]
    nTimes = np.shape(f['nTimes'])[0]

    obs_data[('latitude', metaDataName)] = np.array(f['Latitude'][dataset_mask_2d].flatten(), dtype=ioda_float_type)
    obs_data[('longitude', metaDataName)] = np.array(f['Longitude'][dataset_mask_2d].flatten(), dtype=ioda_float_type)
    nlocs = len(obs_data[('latitude', metaDataName)])
    # there are layer pressures?  "nLayer": shape (11,), type "f4"
    # obs_data[('pressure', metaDataName)] = np.array(f['Pressure'][:], dtype=ioda_float_type)

    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype=ioda_int_type)

    # broadcast scanTime to (nTime, nIFOV)
    expanded_scan_time = f['ScanTime'][:]
    expanded_scan_time = expanded_scan_time[:, np.newaxis] * np.ones(nIFOV)
    obs_data[('dateTime', metaDataName)] = get_epoch_time(f, expanded_scan_time[dataset_mask_2d], timekey='ScanTime')
    # broadcast ascending/descending to (nTime, nIFOV)
    expanded_iasc = f['Ascending_Descending'][:]
    expanded_iasc = expanded_iasc[:, np.newaxis] * np.ones(nIFOV)
    obs_data[('satelliteAscendingFlag', metaDataName)] = np.array(expanded_iasc[dataset_mask_2d], dtype=ioda_int_type)

    k = 'ozoneColumn'
    # there are missing values in the ColumnAmountO3 these match ErrorFlag=8
    data = np.array(f['ColumnAmountO3'][dataset_mask_2d], dtype=ioda_float_type)
    obs_data[(k, obsValName)] = reassign_missing_values(data, dataset_missing=dataset_float_fill)
    obs_data[(k, "ObsError")] = np.full((nlocs), 5.0, dtype=ioda_float_type)
    # f['ErrorFlag'][:, 0]  # do not know what the codes for these values are
    # f['QualityFlag'][:, 0]  # do not know what the codes for these values are
    # appears QualityFlag is the correct one to use but translation is needed
    # does not follow a convention where =0 == good; >0 == bad
    obs_data[(k, "PreQC")] = np.full((nlocs), 0, dtype=ioda_int_type)
    obs_data[('surfaceQualifier', metaDataName)] = np.full((nlocs), int_missing_value, dtype=ioda_int_type)
    # not used -- tropospheric Ozone
    # k='O3BelowCloud' f[k]=<HDF5 dataset "O3BelowCloud": shape (30, 240), type "<f4">

#   # check here seems to use the qc_mask
    obs_data[(k, obsValName)][np.isnan(obs_data[(k, obsValName)])] = float_missing_value
    qc_array = (
        (obs_data[(k, obsValName)] < 1)
        | (
            (obs_data[(k, obsValName)] > 1000.) & (obs_data[(k, obsValName)] != float_missing_value)
        )
    )
    obs_data[(k, obsValName)][qc_array] = float_missing_value

    return obs_data


def reassign_missing_values(data, dataset_missing=-9999):
    if data.dtype == ioda_float_type:
        data[data == dataset_missing] = float_missing_value
        return np.array(data, dtype=ioda_float_type)
    elif data.dtype == ioda_int_type:
        data[data == dataset_missing] = int_missing_value
        return np.array(data, dtype=ioda_int_type)


def assign_values(data):
    if data.dtype == float:
        data[np.abs(data) >= np.abs(float_missing_value)] = float_missing_value
        return np.array(data, dtype=ioda_float_type)
    elif data.dtype == int:
        data[np.abs(data) >= np.abs(int_missing_value)] = int_missing_value
        return np.array(data, dtype=ioda_int_type)


def get_WMO_satellite_ID(filename):

    afile = os.path.basename(filename)
    if 'NPP' in afile or 'npp' in afile:
        WMO_sat_ID = SNPP_WMO_sat_ID
    elif 'J1' in afile or 'j01' in afile or 'J01' in afile:
        WMO_sat_ID = NOAA20_WMO_sat_ID
    elif 'J2' in afile or 'j02' in afile or 'J02' in afile:
        WMO_sat_ID = NOAA21_WMO_sat_ID
    else:
        WMO_sat_ID = -1
        print(f"could not determine satellite from filename: {afile}")
        sys.exit()

    return WMO_sat_ID


def get_epoch_time(f, values, timekey='ScanTime'):

    # get the epoch time references to the IODA epoch
    try:
        # expected value: b'IET, Elapsed time in seconds since Jan 1, 1958 including leap seconds.'
        time_attribute = f[timekey].attrs['long_name'].decode('utf-8')
        match = re.search(r'since (\w+ \d+, \d{4})', time_attribute)
        if match:
            date_str = match.group(1)  # Extracted date string
            # Convert the extracted date to a datetime object
            iet_epoch = datetime.strptime(date_str, '%b %d, %Y')
        else:
            # default to 01Jan1958
            iet_epoch = datetime(1958, 1, 1)
    except Exception as e:
        # Catch and print any errors
        print(f"An error occurred: {e}")
    iet_epoch = iet_epoch.replace(tzinfo=timezone.utc)
    offset = (epoch - iet_epoch).total_seconds()  # Offset in seconds
    # Convert IET to Unix time
    ioda_dateTime = np.array([val/1.e6 - offset for val in values], dtype='int64')

    return ioda_dateTime


def get_obs_total(f, k="O3FINAL", itime=0):
    # sum for instance the Ozone final profiles to a single value over locations
    # assumed dimension is (locations, times, vertice)
    obs_value_column = np.array(f[k][:, itime, :], dtype=ioda_float_type)
    masked_data = np.ma.masked_equal(obs_value_column, f[k].fillvalue)
    # Sum over the dimensions
    obs_value_total = np.sum(masked_data, axis=1)
    return obs_value_total


def init_obs_loc(profile=True):
    obs = {
        ('ozoneColumn', obsValName): [],
        ('ozoneColumn', "ObsError"): [],
        ('ozoneColumn', "PreQC"): [],
        ('satelliteIdentifier', metaDataName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('surfaceQualifier', metaDataName): [],
        ('satelliteAscendingFlag', metaDataName): [],
    }
    if profile:
        obs[('ozoneProfile', obsValName)] = []
        obs[('ozoneProfile', "ObsError")] = []
        obs[('ozoneProfile', "PreQC")] = []
        obs[('pressure', metaDataName)] = []

    return obs


if __name__ == "__main__":

    parser = argparse.ArgumentParser(
        description=(
            'Reads the satellite data '
            ' convert into IODA formatted output files. '
            ' Multiple files are concatenated')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of satellite observation input file(s)",
        type=str, nargs='+', required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-j', '--threads',
        help='multiple threads can be used to load input files in parallel.'
             ' (default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '-o', '--output',
        help='path to output ioda file',
        type=str, default=os.path.join(os.getcwd(), 'output.nc4'))
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, default=None)
    optional.add_argument(
        '--skip',
        help="default pixel skip factor to be applied",
        type=int, default=1)

    args = parser.parse_args()

    main(args)
