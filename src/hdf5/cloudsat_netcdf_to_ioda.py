#!/usr/bin/env python3

#
# (C) Copyright 2020-2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
#
# contribution based on prototype by 2023, Isaac Moradi
#

import sys
import netCDF4 as nc
import numpy as np
import datetime
import time

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.def_jedi_utils import epoch, iso8601_string
import read_cloudsat

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

# globals
CLOUDSAT_WMO_sat_ID = 788

GlobalAttrs = {
    "platformCommonName": "CloudSat",
    "platformLongDescription": "CloudSat reflectance",
    "sensorCentralFrequency": "[94.05]"
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]


def main(args):
    # fname_in = ['2009212223327_17338_CS_2B-GEOPROF_GRANULE_P1_R05_E02_F00.hdf']
    fname_in = args.input
    fname_out = args.output

    cpr_obs = read_cloudsat.read_cloudsat(fname_in)
    
    obsData = get_obsData(cpr_obs)

def get_obsData(cpr_obs):

    # initialize
    obsData = {}

    # allocate space for output depending on which variables are to be saved
    obsData = init_obs_loc()

    # ideally an attribute from the file read
    # example: cpr_obs.attrs['ShortName'].decode("utf-8")
    WMO_sat_ID = get_WMO_satellite_ID('Cloudsat')

    # appears to be observation cleansing and conditioning
    cpr_obs = cpr_obs.rename_vars({"elevation": "elevation1"})
    cpr_obs = cpr_obs.stack(Location=['obs_id', 'elevation']).reset_index("Location")
    cpr_obs = cpr_obs.transpose("Location", "channel")

    locid = (cpr_obs.obs.values < -100) | (cpr_obs.obs.values > 100) | np.isnan(cpr_obs.obs.values) | np.isinf(cpr_obs.obs.values)
    locid = cpr_obs.Location.values[np.sum(locid, axis=1) == 0]
    cpr_obs = cpr_obs.isel(Location=locid)

    # now begin populating dictionary obsData to pass to IODA writer
    nobs = cpr_obs.Location.size
    nchans = cpr_obs.channel.size
    records = np.unique(cpr_obs.sequenceNumber.values).flatten()
    nrecords = np.int32(records.size)

    import pdb
    pdb.set_trace()
    sys.exit()

    refl_dims = ('Location', 'Channel')


    # ds_g.variables['height'][:] = cpr_obs.height.values.astype(np.float32)
    # ds_g.variables['Layer'][:] = cpr_obs.elevation.values.astype(np.int32)

    obs_data[('latitude', metaDataName)] = cpr_obs.lat.values.astype(np.float32)
    obs_data[('longitude', metaDataName)] = cpr_obs.lon.values.astype(np.float32)
    obs_data[('sensorCentralFrequency', metaDataName)] = np.array([94.05]).astype(np.float32)    # cpr_sim.Frequency.values.astype(np.float32)
    obs_data[('sensorCentralWavenumber', metaDataName)] = np.array([3.1371]).astype(np.float32)  # cpr_sim.Wavenumber.values.astype(np.float32)
    obs_data[('sensorPolarizationDirection', metaDataName)] = np.array([9]).astype(np.int32)
    obs_data[('sensorScanPosition', metaDataName)] = cpr_obs.fov1.values.astype(np.int32)
    obs_data[('sensorChannelNumber', metaDataName)] = cpr_obs.channel.values.astype(np.int32)
    obs_data[('sensorViewAngle', metaDataName)] = cpr_obs.zenith_angle.values.astype(np.float32)
    obs_data[('sensorZenithAngle', metaDataName)] = cpr_obs.zenith_angle.values.astype(np.float32)
    obs_data[('sensorAzimuthAngle', metaDataName)] = cpr_obs.azimuth_angle.values.astype(np.float32)
    obs_data[('solarZenithAngle', metaDataName)] = np.zeros(nobs).astype(np.float32)
    obs_data[('solarAzimuthAngle', metaDataName)] = np.zeros(nobs).astype(np.float32)
    obs_data[('sequenceNumber', metaDataName)] = cpr_obs.sequenceNumber.values.astype(np.int32)
        # ?? ds_g.variables['sequenceNumber'][:] = cpr_obs["sequenceNumber"].values.astype(np.int32)
    obs_data[('dateTime', metaDataName)] = cpr_obs.epoch_time.values.astype(np.int64)

    obs_data[('sequenceNumber', metaDataName)] = cpr_obs.sequenceNumber.values.astype(np.int32)
    k = 'ReflectivityAttenuated'
    # have to reorder the channel axis to be last then merge ( nscans x nspots = nlocs )
    obs_data[(k, "ObsValue")] = cpr_obs.obs.values.astype(np.float32)
    obs_data[(k, "ObsError")] = np.full((nobs, nchans), 5.0, dtype='float32')
    obs_data[(k, "PreQC")] = np.full((nobs, nchans), 0, dtype='int32')


def init_obs_loc():
    obs = {
        ('ReflectivityAttenuated', "ObsValue"): [],
        ('ReflectivityAttenuated', "ObsError"): [],
        ('ReflectivityAttenuated', "PreQC"): [],
        ('satelliteIdentifier', metaDataName): [],
        ('sensorChannelNumber', metaDataName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('solarZenithAngle', metaDataName): [],
        ('solarAzimuthAngle', metaDataName): [],
        ('sensorZenithAngle', metaDataName): [],
        ('sensorAzimuthAngle', metaDataName): [],
#       ('sensorViewAngle', metaDataName): [],
#       ('satelliteAscendingFlag', metaDataName): [],
    }

    return obs


def get_WMO_satellite_ID(attrs_shortname):

    if 'CloudSat' in attrs_shortname:
        WMO_sat_ID = CLOUDSAT_WMO_sat_ID
    else:
        WMO_sat_ID = -1
        print("could not determine satellite from filename: %s" % attrs_shortname)
        sys.exit()

    return WMO_sat_ID


def get_epoch_time(f):

    nbeam_pos = len(f['spots'])
    # ugh this does not seem generic

if __name__ == "__main__":

    import argparse
    import os
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
        '-o', '--output',
        help='path to output ioda file',
        type=str, default=os.path.join(os.getcwd(), 'output.nc4'))
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDHH",
        help="base date for the center of the window",
        type=str, default=None)

    args = parser.parse_args()

    main(args)
