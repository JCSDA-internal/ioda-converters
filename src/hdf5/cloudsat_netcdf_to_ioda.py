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
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import set_metadata_attributes, set_obspace_attributes
from pyiodaconv.def_jedi_utils import epoch, iso8601_string
import read_cloudsat

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

# globals
CLOUDSAT_WMO_sat_ID = 788

# parameter
radarReflectivityAtt = 'ReflectivityAttenuated'

GlobalAttrs = {
    "platformCommonName": "CloudSat",
    "platformLongDescription": "CloudSat reflectance",
    "sensorCentralFrequency": "[94.05 GHz]"
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]


def main(args):

    # take a user input and call read_cloudsat decoder
    # place this xarray into a dictionary and pass to IODA writer

    # example: input_filename = ['2009212223327_17338_CS_2B-GEOPROF_GRANULE_P1_R05_E02_F00.hdf']
    input_filename = args.input
    output_filename = args.output

    cpr_obs = read_cloudsat.read_cloudsat(input_filename)
    obs_data = import_obs_data(cpr_obs)

    nlocs_int = np.array(len(obs_data[('latitude', metaDataName)]), dtype='int64')
    nlocs = nlocs_int.item()
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])
    # still need to add these metaData - TODO !
    # ds_g.variables['height'][:] = cpr_obs.height.values.astype(np.float32)
    # ds_g.variables['Layer'][:] = cpr_obs.elevation.values.astype(np.int32)

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    dtg = False  # pass a reference time for the observation such as to an NWP analysis
    if dtg:
        GlobalAttrs['datetimeReference'] = dtg.strftime("%Y-%m-%dT%H:%M:%SZ")
    GlobalAttrs['converter'] = os.path.basename(__file__)

    # set key for observable
    k = radarReflectivityAtt

    # pass parameters to the IODA writer
    VarDims = {
        k: ['Location', 'Channel'],
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

    VarAttrs[(k, 'ObsValue')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
    VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
    VarAttrs[(k, 'ObsValue')]['units'] = 'dBz'
    VarAttrs[(k, 'ObsError')]['units'] = 'dBz'

    # final write to IODA file
    writer.BuildIoda(obs_data, VarDims, VarAttrs, GlobalAttrs)


def import_obs_data(cpr_obs):

    # appears to be observation cleansing and conditioning
    cpr_obs = cpr_obs.rename_vars({"elevation": "elevation1"})
    cpr_obs = cpr_obs.stack(Location=['obs_id', 'elevation']).reset_index("Location")
    cpr_obs = cpr_obs.transpose("Location", "channel")

    locid = (cpr_obs.obs.values < -100) | (cpr_obs.obs.values > 100) | np.isnan(cpr_obs.obs.values) | np.isinf(cpr_obs.obs.values)
    locid = cpr_obs.Location.values[np.sum(locid, axis=1) == 0]
    cpr_obs = cpr_obs.isel(Location=locid)
    # end conditioning and cleansing block

    # this function will map the cloud radar data in the cpr xarray
    # into a dictionary to be passed to the IODA writer functions
    nobs = cpr_obs.Location.size
    nchans = cpr_obs.channel.size

    # ideally an attribute from the file read in
    # example: cpr_obs.attrs['ShortName'].decode("utf-8")
    # here we are hardcoding to the function the name of the satellite
    WMO_sat_ID = get_WMO_satellite_ID('CloudSat')

    # allocate space for output depending on which variables are to be saved
    obs_data = init_obs_loc()

    obs_data[('latitude', metaDataName)] = cpr_obs.lat.values.astype(np.float32)
    obs_data[('longitude', metaDataName)] = cpr_obs.lon.values.astype(np.float32)
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nobs), WMO_sat_ID, dtype='int32')
    obs_data[('sensorCentralFrequency', metaDataName)] = np.array([94.05]).astype(np.float32)    # cpr_sim.Frequency.values.astype(np.float32)
    obs_data[('sensorCentralWavenumber', metaDataName)] = np.array([3.1371]).astype(np.float32)  # cpr_sim.Wavenumber.values.astype(np.float32)
    obs_data[('sensorPolarizationDirection', metaDataName)] = np.array([9]).astype(np.int32)
    obs_data[('sensorScanPosition', metaDataName)] = cpr_obs.fov1.values.astype(np.int32)
    obs_data[('sensorChannelNumber', metaDataName)] = cpr_obs.channel.values.astype(np.int32)
    # add satellite altitude (height in meters) and use compute_scan_angle function
    obs_data[('sensorViewAngle', metaDataName)] = cpr_obs.zenith_angle.values.astype(np.float32)
    obs_data[('sensorZenithAngle', metaDataName)] = cpr_obs.zenith_angle.values.astype(np.float32)
    obs_data[('sensorAzimuthAngle', metaDataName)] = cpr_obs.azimuth_angle.values.astype(np.float32)
    obs_data[('solarZenithAngle', metaDataName)] = np.zeros(nobs).astype(np.float32)
    obs_data[('solarAzimuthAngle', metaDataName)] = np.zeros(nobs).astype(np.float32)
    obs_data[('height', metaDataName)] = cpr_obs.height.values.astype(np.float32)
    obs_data[('Layer', metaDataName)] = cpr_obs.elevation1.values.astype(np.float32)
    obs_data[('sequenceNumber', metaDataName)] = cpr_obs.sequenceNumber.values.astype(np.int32)
    obs_data[('dateTime', metaDataName)] = cpr_obs.epoch_time.values.astype(np.int64)

    obs_data[('sequenceNumber', metaDataName)] = cpr_obs.sequenceNumber.values.astype(np.int32)
    k = radarReflectivityAtt
    # have to reorder the channel axis to be last then merge ( nscans x nspots = nlocs )
    obs_data[(k, "ObsValue")] = cpr_obs.obs.values.astype(np.float32)
    obs_data[(k, "ObsError")] = np.full((nobs, nchans), 5.0, dtype='float32')
    obs_data[(k, "PreQC")] = np.full((nobs, nchans), 0, dtype='int32')

    return obs_data


def init_obs_loc():
    k = radarReflectivityAtt
    obs = {
        (k, "ObsValue"): [],
        (k, "ObsError"): [],
        (k, "PreQC"): [],
        ('satelliteIdentifier', metaDataName): [],
        ('sensorChannelNumber', metaDataName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('solarZenithAngle', metaDataName): [],
        ('solarAzimuthAngle', metaDataName): [],
        ('sensorZenithAngle', metaDataName): [],
        ('sensorAzimuthAngle', metaDataName): [],
        ('height', metaDataName): [],
        ('Layer', metaDataName): [],
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
