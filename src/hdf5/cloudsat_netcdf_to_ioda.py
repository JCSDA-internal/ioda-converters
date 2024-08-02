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
import xarray as xr
from datetime import datetime

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import set_metadata_attributes, set_obspace_attributes
from pyiodaconv.def_jedi_utils import epoch, iso8601_string
from pyiodaconv.def_jedi_utils import record_time
from read_cloudsat import read_cloudsat, is_hdf4
from read_dpr_gpm import read_dpr_gpm, is_hdf5

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

# globals
CLOUDSAT_WMO_sat_ID = 788
GPM_WMO_sat_ID = 288

# parameter
radarReflectivityAtt = 'ReflectivityAttenuated'

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

GlobalAttrs = {}


def main(args):

    # start timer
    tic = record_time()

    # take a user input and call read_cloudsat decoder
    # place this xarray into a dictionary and pass to IODA writer

    # example: input_filename = ['2009212223327_17338_CS_2B-GEOPROF_GRANULE_P1_R05_E02_F00.hdf']
    output_filename = args.output

    if args.input_files is None:
        print(f'no observation files provided exiting')
        sys.exit()

    if len(args.input_files) > 1:
        print(f'please provide unique input files associated to a unique output file')
        print(f'   ... TBD multiple input files to a single output file')
        sys.exit()

    for input_filename in args.input_files:

        file_is_hdf4 = is_hdf4(input_filename)
        file_is_hdf5 = is_hdf5(input_filename)
        if file_is_hdf4 and not file_is_hdf5:
            file_obs_data = read_cloudsat(input_filename)
            sensor_name = 'CloudSat'
        elif file_is_hdf5 and not file_is_hdf4:
            file_obs_data = read_dpr_gpm(input_filename)
            sensor_name = 'GPM-DPR'

    # report time
    toc = record_time(tic=tic)

    if not file_obs_data:
        print(f'no observations found exiting')
        sys.exit()

    sensor_upper = sensor_name.replace("_", " ").upper()
    GlobalAttrs["platformCommonName"] = sensor_upper
    GlobalAttrs["platformLongDescription"] = f"{sensor_upper} Attenuated Reflectivity"
    GlobalAttrs["sensorCentralFrequency"] = str(file_obs_data.centerFreq.values)

    obs_data = populate_obs_data(file_obs_data, sensor_name)

    nlocs_int = np.array(len(obs_data[('latitude', metaDataName)]), dtype='int64')
    nlocs = nlocs_int.item()
    nchans = len(obs_data[('sensorChannelNumber', metaDataName)])

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
        'sensorCentralFrequency': ['Channel'],
        'sensorCentralWavenumber': ['Channel'],
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

    # report time
    toc = record_time(tic=tic)


def populate_obs_data(file_obs_data, sensor_name):

    # appears to be observation cleansing and conditioning
    file_obs_data = file_obs_data.rename_vars({"elevation": "elevation1"})
    file_obs_data = file_obs_data.stack(Location=['obs_id', 'elevation']).reset_index("Location")
    file_obs_data = file_obs_data.transpose("Location", "channel")

    locid = (file_obs_data.obs.values < -100) | (file_obs_data.obs.values > 100) | np.isnan(file_obs_data.obs.values) | np.isinf(file_obs_data.obs.values)
    locid = file_obs_data.Location.values[np.sum(locid, axis=1) == 0]
    file_obs_data = file_obs_data.isel(Location=locid)
    # end conditioning and cleansing block

    # this function will map the cloud radar data in the cpr xarray
    # into a dictionary to be passed to the IODA writer functions
    nobs = file_obs_data.Location.size
    nchans = file_obs_data.channel.size

    # ideally an attribute from the file read in
    # example: file_obs_data.attrs['ShortName'].decode("utf-8")
    # here we are hardcoding to the function the name of the satellite
    WMO_sat_ID = get_WMO_satellite_ID(sensor_name)

    # allocate space for output depending on which variables are to be saved
    obs_data = init_obs_loc()

    obs_data[('latitude', metaDataName)] = file_obs_data.lat.values.astype(np.float32)
    obs_data[('longitude', metaDataName)] = file_obs_data.lon.values.astype(np.float32)
    obs_data[('satelliteIdentifier', metaDataName)] = np.full((nobs), WMO_sat_ID, dtype='int32')
    obs_data[('sensorCentralFrequency', metaDataName)] = file_obs_data.centerFreq.values.astype(np.float32)
    obs_data[('sensorCentralWavenumber', metaDataName)] = file_obs_data.centerWN.values.astype(np.float32)
    obs_data[('sensorPolarizationDirection', metaDataName)] = np.array([9]).astype(np.int32)
    obs_data[('sensorScanPosition', metaDataName)] = file_obs_data.fov1.values.astype(np.int32)
    obs_data[('sensorChannelNumber', metaDataName)] = file_obs_data.channel.values.astype(np.int32)
    # add satellite altitude (height in meters) and use compute_scan_angle function
    obs_data[('sensorViewAngle', metaDataName)] = file_obs_data.zenith_angle.values.astype(np.float32)
    obs_data[('sensorZenithAngle', metaDataName)] = file_obs_data.zenith_angle.values.astype(np.float32)
    obs_data[('sensorAzimuthAngle', metaDataName)] = file_obs_data.azimuth_angle.values.astype(np.float32)
    obs_data[('solarZenithAngle', metaDataName)] = np.zeros(nobs).astype(np.float32)
    obs_data[('solarAzimuthAngle', metaDataName)] = np.zeros(nobs).astype(np.float32)
    obs_data[('height', metaDataName)] = file_obs_data.height.values.astype(np.float32)
    obs_data[('Layer', metaDataName)] = file_obs_data.elevation1.values.astype(np.float32)
    obs_data[('sequenceNumber', metaDataName)] = file_obs_data.sequenceNumber.values.astype(np.int32)
    obs_data[('dateTime', metaDataName)] = file_obs_data.epoch_time.values.astype(np.int64)

    obs_data[('sequenceNumber', metaDataName)] = file_obs_data.sequenceNumber.values.astype(np.int32)
    k = radarReflectivityAtt
    # have to reorder the channel axis to be last then merge ( nscans x nspots = nlocs )
    obs_data[(k, "ObsValue")] = file_obs_data.obs.values.astype(np.float32)
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
    elif 'GPM' in attrs_shortname:
        WMO_sat_ID = GPM_WMO_sat_ID
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
        '-f', '--input_files',
        help="path of satellite observation input file(s)",
        type=str, nargs='+')
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-o', '--output',
        help='path to output ioda file',
        type=str, default=os.path.join(os.getcwd(), 'output.nc4'))

    args = parser.parse_args()

    main(args)
