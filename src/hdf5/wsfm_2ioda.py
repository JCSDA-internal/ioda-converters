#!/usr/bin/env python3

#
# (C) Copyright 2020-2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#


"""
Python code to ingest WSF Microwave Imager data
"""

import argparse
from datetime import datetime, timedelta
import os.path
import sys

import h5py
import numpy as np

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import set_metadata_attributes, set_obspace_attributes
from pyiodaconv.def_jedi_utils import compute_scan_angle
from pyiodaconv.def_jedi_utils import ioda_int_type, ioda_float_type, epoch
from pyiodaconv.def_jedi_utils import concat_obs_dict

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

# globals
WSF_WMO_sat_ID = 999     # unknown

iso8601_string = "seconds since 1970-01-01T00:00:00Z"
epoch = datetime.fromisoformat(iso8601_string[14:-1])
#  Epoch Time (ET) offset
# 'UTC Time corresponding to ... Seconds since January 1, 1958, 00 h 00 m.'
base_date = datetime(1958, 1, 1)
et_offset = (epoch - base_date).total_seconds()

GlobalAttrs = {
    "platformCommonName": "WSFMI",
    "platformLongDescription": "WSF Microwave Imager Brightness Temperature Data",
    "sensorCentralFrequency": "[10.85V, 10.85H, 10.85S3, 10.85S4, 18.85V, 18.85H, 18.85S3, 18.85S4, 23.8V, "
                               " 36.75V, 36.75H, 36.75S3, 36.75S4, 37.3V, 37.3H, 89V, 89H]"
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
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
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    if dtg:
        GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    GlobalAttrs['converter'] = os.path.basename(__file__)

    # pass parameters to the IODA writer
    VarDims = {
        'brightnessTemperature': ['Location', 'Channel'],
        'sensorChannelNumber': ['Channel'],
    }

    DimDict = {
        'Location': nlocs,
        'Channel': obs_data[('sensorChannelNumber', metaDataName)],
    }
    writer = iconv.IodaWriter(output_filename, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    set_obspace_attributes(VarAttrs)
    set_metadata_attributes(VarAttrs)

    k = 'brightnessTemperature'
    VarAttrs[(k, 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[(k, 'ObsValue')]['units'] = 'K'
    VarAttrs[(k, 'ObsError')]['units'] = 'K'
    # VarAttrs[(k, 'PreQC')]['units'] = 'unitless'

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def get_data_from_files(afile, skip=1):

    # allocate space for output depending on which variables are to be saved
    obs_data = init_obs_loc()

    f = h5py.File(afile, 'r')
    obs_data = get_data(f, obs_data, skip=skip)
    f.close()

    return obs_data


def get_data(f, obs_data, skip=1):

    WMO_sat_ID = get_WMO_satellite_ID(f.attrs['GranuleFilename'].decode("utf-8"))

    nscans = f['NumScanR'][0]
    nbeam_pos = f['NumFOVR'][0]
    nchans = len(f['ChanFrequency'][:])
    nbands = len(f['BandFrequency'][:])  # not used
    band_norm = 2  # define a band to use for angles
    obs_data[('latitude', metaDataName)] = np.array(f['Latitude'][:, :].flatten(), dtype='float32')
    obs_data[('longitude', metaDataName)] = np.array(f['Longitude'][:, :].flatten(), dtype='float32')
    obs_data[('longitude', metaDataName)][obs_data[('longitude', metaDataName)] > 180] -= 360.
    obs_data[('sensorChannelNumber', metaDataName)] = np.array(np.arange(nchans)+1, dtype='int32')
    obs_data[('sensorScanPosition', metaDataName)] = np.tile(np.arange(nbeam_pos, dtype='int32')+1, (nscans, 1)).flatten()
    obs_data[('solarZenithAngle', metaDataName)] = np.array(f['SolarZenithAngle'][:, :].flatten(), dtype='float32')
    obs_data[('solarAzimuthAngle', metaDataName)] = np.array(f['SolarAzimuthAngle'][:, :].flatten(), dtype='float32')
    obs_data[('sensorZenithAngle', metaDataName)] = np.array(f['EarthIncidenceAngle'][band_norm, :, :].flatten(), dtype='float32')
    obs_data[('sensorAzimuthAngle', metaDataName)] = np.array(f['EarthAzimuthAngle'][band_norm, :, :].flatten(), dtype='float32')
    obs_data[('sensorViewAngle', metaDataName)] = np.array(f['EarthIncidenceAngle'][band_norm, :, :].flatten(), dtype='float32')

    nlocs = len(obs_data[('latitude', metaDataName)])
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')

    obs_data[('dateTime', metaDataName)] = np.array(np.repeat(f['ScanStartTime'][:] - et_offset, nbeam_pos), dtype='int64')

    k = 'brightnessTemperature'
    # have to reorder the channel axis to be last then merge ( nscans x nspots = nlocs )
    # 'TbLowRes' - 'Brightness temperatures on the resampled scan grid for each polarimetric channel, 30 km RFOV'
    # 'TbHighRes' - 'Brightness temperatures on the resampled scan grid for each polarimetric channel, 20 km RFOV'
    # for channels less than 20 GHz use the TbLowRes and the TbHighRes for the other bands
    freq_cf = 20.

    obs_data[(k, "ObsValue")] = np.zeros((nscans*nbeam_pos, nchans), dtype='float32')
    for i, freq in enumerate(f['ChanFrequency']):
        obs_data[(k, "ObsValue")][:, i] = f['TbLowRes'][i, :, :].flatten() if freq < freq_cf else f['TbHighRes'][i, :, :].flatten()
    obs_data[(k, "ObsError")] = np.full((nlocs, nchans), 5.0, dtype='float32')
    # initialize PreQC
    obs_data[(k, "PreQC")] = np.full((nlocs, nchans), 0, dtype='int32')

    # there is a quality word for each band
    quality_word = np.vstack(np.stack(f['QualityFlag'], axis=2))
    # obs_data[('satelliteAscendingFlag', metaDataName)] = np.array(get_normalized_bit(quality_word[:, 0], bit_index=6), dtype='int32')

    # check some global satellite geometry will compress all data using this
    chk_geolocation = (obs_data[('latitude', metaDataName)] > 90) | (obs_data[('latitude', metaDataName)] < -90) | \
        (obs_data[('longitude', metaDataName)] > 180) | (obs_data[('longitude', metaDataName)] < -180) | \
        (obs_data[('sensorZenithAngle', metaDataName)] > 80) | (obs_data[('sensorZenithAngle', metaDataName)] < 0)

    obs_data[('latitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('longitude', metaDataName)][chk_geolocation] = float_missing_value
    obs_data[('sensorZenithAngle', metaDataName)][chk_geolocation] = float_missing_value

    obs_key = ('brightnessTemperature', "ObsValue")
    obs_data = set_missing_value(f, nchans, chk_geolocation, quality_word, obs_key, obs_data, skip=skip)

    return obs_data


def set_missing_value(f, nchans, chk_geolocation, quality_word, obs_key, obs_data, skip=1):

    # use quality word to determine where to set for missing values
    bands = f['BandFrequency']
    fillvalue = f['TbHighRes'].fillvalue
    for jchan, freq in enumerate(f['ChanFrequency']):
        band_idx = np.where(bands == freq)[0][0]
        i_pass = quality_word[:, band_idx]
        chk_ob = (i_pass > 0) | (obs_data[obs_key][:, jchan] == fillvalue) | (chk_geolocation)
        obs_data[obs_key][:, jchan][chk_ob] = float_missing_value

    # this is a data reduction step that removes values that are missing
    tb_key = 'brightnessTemperature'
    good = (obs_data[(tb_key, obsValName)][:, 0] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 4] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 8] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 9] != float_missing_value) & \
        (obs_data[(tb_key, obsValName)][:, 13] != float_missing_value)
    for k in obs_data:
        if metaDataName in k[1] and 'sensorChannelNumber' not in k[0]:
            obs_data[k] = obs_data[k][good][::skip]
        elif tb_key in k[0]:
            obs_data[k] = obs_data[k][good, :][::skip]

    return obs_data


def get_normalized_bit(value, bit_index):
    return (value >> bit_index) & 1


def assign_values(data):
    if data.dtype == float:
        data[np.abs(data) >= np.abs(float_missing_value)] = float_missing_value
        return np.array(data, dtype=ioda_float_type)
    elif data.dtype == int:
        data[np.abs(data) >= np.abs(int_missing_value)] = int_missing_value
        return np.array(data, dtype=ioda_int_type)


def get_WMO_satellite_ID(attrs_shortname):

    if 'WSFM' in attrs_shortname:
        WMO_sat_ID = WSF_WMO_sat_ID
    else:
        WMO_sat_ID = -1
        print("could not determine satellite from filename: %s" % attrs_shortname)
        sys.exit()

    return WMO_sat_ID


def init_obs_loc():
    obs = {
        ('brightnessTemperature', "ObsValue"): [],
        ('brightnessTemperature', "ObsError"): [],
        ('brightnessTemperature', "PreQC"): [],
        ('satelliteIdentifier', metaDataName): [],
        ('sensorChannelNumber', metaDataName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('sensorScanPosition', metaDataName): [],
        ('solarZenithAngle', metaDataName): [],
        ('solarAzimuthAngle', metaDataName): [],
        ('sensorZenithAngle', metaDataName): [],
        ('sensorAzimuthAngle', metaDataName): [],
        ('sensorViewAngle', metaDataName): [],
    }
#       ('satelliteAscendingFlag', metaDataName): [],

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
