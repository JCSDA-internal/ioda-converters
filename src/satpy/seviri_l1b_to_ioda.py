#!/usr/bin/env python3

#
# (C) Copyright 2020-2025 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# author: Benjamin Ruston
# This script will work with native EUMETSAT MeteoSat SEVIRI Native Level 1B files
#

from datetime import datetime, timedelta
import numpy as np
from pyproj import CRS
import pyresample
from pyresample.kd_tree import resample_nearest
import re
from satpy.scene import Scene
from satpy.readers import seviri_l1b_native

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import set_metadata_attributes, set_obspace_attributes
from pyiodaconv.def_jedi_utils import compute_scan_angle
from pyiodaconv.def_jedi_utils import ioda_int_type, ioda_float_type, epoch
from pyiodaconv.def_jedi_utils import concat_obs_dict

import pdb
import sys

# globals
MSG_WMO_sat_ID = 267   # MeteoSat second generation uses single WMO BUFR ID

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

GlobalAttrs = {
    "platformCommonName": "SEVIRI",
    "platformLongDescription": "EUMETSAT MeteoSat SEVIRI L1B Brightness Temperature and Reflectance Data",
    "sensorCentralWavelength": "[0.7, 0.635, 0.81, 1.64, 3.92, 6.25, 7.35, 8.7, 9.66, 10.8, 12.0, 13.4]"
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]


class DataIdInfo:
    """A helper class to extract clean data from satpy's DataID object."""
    def __init__(self, data_id_obj):
        # Extract the name using a regular expression
        name_match = re.search(r"name='(.*?)'", str(data_id_obj))
        self.name = name_match.group(1) if name_match else None
        
        # Extract the central wavelength using a regular expression
        wavelength_match = re.search(r"central=(\d+\.?\d*)", str(data_id_obj))
        self.central_wavelength = float(wavelength_match.group(1)) if wavelength_match else None


def get_seviri_scene(filenames):

    """
    decode an EUMETSAT MeteoSat SEVIRI native L1B file using satpy

    Args:
        filename list - list of files to decode

    Returns:
       obs_scene - the resampled obs_scene to a common lat/lon projection
       obs_dateTime - the datetime for each new pixel in lat/lon projection
    """

    # filename(s) to be read
    # filenames = ['MSG4-SEVI-MSG15-0100-NA-20220622191243.890000000Z-NA.nat']

    # what datasets are available
    header = seviri_l1b_native.read_header(filenames[0])
    available_datasets = seviri_l1b_native.get_available_channels(header)
    aload = [k for k, v in available_datasets.items() if v]

    # Add the angle datasets to the list you want to load
    datasets_to_load = aload + [
        'satellite_zenith_angle',
        'satellite_azimuth_angle',
        'solar_zenith_angle',
        'solar_azimuth_angle'
    ]

    # load Scene
    scn = Scene(reader="seviri_l1b_native", filenames=filenames, reader_kwargs={'fill_disk': False})
    # scn.load(['IR_108'])  # test single channel
    scn.load(aload)

    # ensure the the loaded datasets in the Scene are calibrated (version dependent)
    # scn.calibrate()

    satellite_name, instrument_name, satellite_altitude = get_metadata(scn)

    # Create a target area with the default 0.1 degree resolution
    target_area = create_latlon_area(resolution_deg=10.)
    print(f"target area shape: {target_area.shape}")

    # Create a target area with a higher 0.05 degree resolution
    # target_area = create_latlon_area(resolution_deg=0.05)
    # print(f"target area shape: {target_area.shape}")

    # Resample the scene to the new target area
    scn_latlon = scn.resample(target_area)

    # get a time for each pixel on new target area
    locationDateTime = get_pixel_time(scn, target_area)

    # Get the resampled solar and satellite angles
#   resampled_satellite_zenith = resample_ancillary_data(scn, 'satellite_zenith_angle', target_area)
#   resampled_satellite_azimuth = resample_ancillary_data(scn, 'satellite_azimuth_angle', target_area)
#   resampled_solar_zenith = resample_ancillary_data(scn, 'solar_zenith_angle', target_area)
#   resampled_solar_azimuth = resample_ancillary_data(scn, 'solar_azimuth_angle', target_area)

#   # Access the new latitude and longitude coordinates
#   latitude = scn_latlon['IR_108'].coords['y']
#   longitude = scn_latlon['IR_108'].coords['x']

#   ir_data = scn_latlon['IR_108'].data
#   vis_data = scn_latlon['VIS008'].data

    return scn_latlon, locationDateTime

def create_latlon_area(resolution_deg=0.1, area_extent=(-81, -81, 81, 81)):
    """
    Creates a lat/lon AreaDefinition

    Args:
        resolution_deg (float): Desired resolution in degrees
        area_extent (tuple): (min_lon, min_lat, max_lon, max_lat) in degrees

    Returns:
        pyresample.AreaDefinition: The defined grid
    """
    min_lon, min_lat, max_lon, max_lat = area_extent
    
    # Calculate the number of points for the shape
    # Lon/x-dimension span: max_lon - min_lon
    # Lat/y-dimension span: max_lat - min_lat
    width = int(np.ceil((max_lon - min_lon) / resolution_deg))
    height = int(np.ceil((max_lat - min_lat) / resolution_deg))

    target_crs = CRS.from_epsg(4326)

    target_area = pyresample.create_area_def('latlon_area',
                                             proj_id='latlon',
                                             projection={'proj': 'longlat', 'ellps': 'WGS84', 'no_defs': True},
                                             area_extent=area_extent,
                                             shape=(height, width))
    return target_area


def variables_to_obs(obs_scene, obs_dateTime, VarDims, albedo=False, dataset='IR_108', apply_gross_qc=True):
    """
    Move data from satpy Scene into IODA convention

    Args:
        obs_scene: satpy Scene of satellite data
        obs_dateTime: dateTime for each pixel

    Returns:
        obs: dictionary following IODA conventions

    SEVIRI definition of channel:
        Channel 01 - HRV, Central Wavelength: 0.7 µm
        Channel 02 - VIS006, Central Wavelength: 0.635 µm
        Channel 03 - VIS008, Central Wavelength: 0.81 µm
        Channel 04 - IR_016, Central Wavelength: 1.64 µm
        Channel 05 - IR_039, Central Wavelength: 3.92 µm
        Channel 06 - WV_062, Central Wavelength: 6.25 µm
        Channel 07 - WV_073, Central Wavelength: 7.35 µm
        Channel 08 - IR_087, Central Wavelength: 8.7 µm
        Channel 09 - IR_097, Central Wavelength: 9.66 µm
        Channel 10 - IR_108, Central Wavelength: 10.8 µm
        Channel 11 - IR_120, Central Wavelength: 12.0 µm
        Channel 12 - IR_134, Central Wavelength: 13.4 µm
    """
    obs = init_obs()
    # order for channels
    albedo_channels = ['HRV', 'VIS006', 'VIS008']
    bt_channels = ['IR_016', 'IR_039', 'WV_062', 'WV_073', 'IR_087', 'IR_097', 'IR_108', 'IR_120', 'IR_134']
    nlocs = obs_scene[dataset].size
    bt_nchans = len(bt_channels)
    albedo_nchans = len(albedo_channels)


    if albedo:
        albedo_data = []
        # Loop through the channels and flatten the data
        for channel_name in albedo_channels:
            albedo_data.append(obs_scene[channel_name].data.flatten())
        albedo_data_stacked = np.vstack(albedo_data)
        albedo_final = albedo_data_stacked.T.astype('float32')
        k = 'albedo'
        obs[(k, "ObsValue")] = albedo_final
        obs[(k, "ObsError")] = np.full((nlocs, albedo_nchans), 5.0, dtype='float32')
        obs[(k, "PreQC")] = np.full((nlocs, albedo_nchans), 0, dtype='int32')
        obs[('sensorChannelNumber', metaDataName)] = np.array(np.arange(albedo_nchans)+1, dtype='int32')

    else:
        bt_data = []
        for channel_name in bt_channels:
            bt_data.append(obs_scene[channel_name].data.flatten())
        # Convert the lists of flattened arrays into a single 2D NumPy array
        # The result will have shape (num_channels, num_locations)
        bt_data_stacked = np.vstack(bt_data)
        # Transpose the arrays to get the desired (location, channel) shape
        bt_final = bt_data_stacked.T.astype('float32')
        k = 'brightnessTemperature'
        obs[(k, "ObsValue")] = bt_final
        obs[(k, "ObsError")] = np.full((nlocs, bt_nchans), 5.0, dtype='float32')
        obs[(k, "PreQC")] = np.full((nlocs, bt_nchans), 0, dtype='int32')
        obs[('sensorChannelNumber', metaDataName)] = np.array(np.arange(bt_nchans)+4, dtype='int32')

    latitude = obs_scene[dataset].coords['y']
    longitude = obs_scene[dataset].coords['x']
    lon_2d, lat_2d = np.meshgrid(longitude, latitude)

    satellite_name, instrument_name, satellite_altitude = get_metadata(obs_scene)
    WMO_sat_ID = get_WMO_sat_ID(satellite_name)

    obs[("latitude", metaDataName)] = np.array(lat_2d.flatten(), dtype='float32')
    obs[("longitude", metaDataName)] = np.array(lon_2d.flatten(), dtype='float32')
    obs[('dateTime', metaDataName)] = np.array(obs_dateTime.flatten(), dtype='int64')
    obs[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    obs[('stationElevation', metaDataName)] = np.full((nlocs), satellite_altitude, dtype='float32')
    obs[('sensorZenithAngle', metaDataName)] = np.full((nlocs), 0., dtype='float32')
    obs[('sensorViewAngle', metaDataName)] = np.full((nlocs), 0., dtype='float32')
    obs[('sensorAzimuthAngle', metaDataName)] = np.full((nlocs), 0., dtype='float32')
    obs[('solarZenithAngle', metaDataName)] = np.full((nlocs), 0., dtype='float32')
    obs[('solarAzimuthAngle', metaDataName)] = np.full((nlocs), 0., dtype='float32')
    obs[('stationElevation', metaDataName)] = np.full((nlocs), satellite_altitude, dtype='float32')

    if apply_gross_qc:
        obs = location_gross_qc(obs)
        obs = gross_qc(obs)

    return obs


def init_obs(albedo=False):
    obs = {
        ('brightnessTemperature', "ObsValue"): [],
        ('brightnessTemperature', "ObsError"): [],
        ('brightnessTemperature', "PreQC"): [],
        ('satelliteIdentifier', metaDataName): [],
        ('stationElevation', metaDataName): [],
        ('sensorChannelNumber', metaDataName): [],
        ('latitude', metaDataName): [],
        ('longitude', metaDataName): [],
        ('dateTime', metaDataName): [],
        ('sensorZenithAngle', metaDataName): [],
        ('sensorAzimuthAngle', metaDataName): [],
        ('sensorViewAngle', metaDataName): [],
        ('solarZenithAngle', metaDataName): [],
        ('solarAzimuthAngle', metaDataName): [],
    }
    if albedo:
        obs[('albedo', "ObsValue")]  = []
        obs[('albedo', "ObsError")]  = []
        obs[('albedo', "PreQC")]  = []
        # Remove the brightnessTemperature entries using pop()
        obs.pop(('brightnessTemperature', "ObsValue"), None)
        obs.pop(('brightnessTemperature', "ObsError"), None)
        obs.pop(('brightnessTemperature', "PreQC"), None)


    return obs


def location_gross_qc(obs):
    """
    Apply gross quality control to the latitude, longitude and dateTime
    Args: obs - IODA dictionary of observation
    Returns: obs - IODA dictionary after gross quality control
    """
    chk_location = (obs[('latitude', metaDataName)] > 90) | (obs[('latitude', metaDataName)] < -90) | \
        (obs[('longitude', metaDataName)] > 180) | (obs[('longitude', metaDataName)] < -180) | \
        (obs[('sensorZenithAngle', metaDataName)] > 80) | (obs[('sensorZenithAngle', metaDataName)] < 0) | \
        (obs[('dateTime', metaDataName)] <= 0)
    obs[('latitude', metaDataName)][chk_location] = float_missing_value
    obs[('longitude', metaDataName)][chk_location] = float_missing_value
    obs[('sensorZenithAngle', metaDataName)][chk_location] = float_missing_value
    obs[('dateTime', metaDataName)][chk_location] = long_missing_value
    obs = add_to_preQC(obs, chk_location)
    return obs


def gross_qc(obs):
    """
    Apply gross quality control this is specific for SEVIR I
    Args: obs - IODA dictionary of observation
    Returns: obs - IODA dictionary after gross quality control
    """
    obs_limits = {
        'brightnessTemperature': {'min': 20.0, 'max': 400.0},
        'albedo': {'min': 0.0, 'max': 1.0}
    }
    obs_key = next((key for key in obs_limits if (key, 'ObsValue') in obs), None)

    if obs_key is None:
        # chk_obs = np.zeros(len(obs[('sensorChannelNumber', metaDataName)]), dtype=bool)
        return obs
    else:
        # Get the min and max limits for the current observation type
        kmin = obs_limits[obs_key]['min']
        kmax = obs_limits[obs_key]['max']

        nrec = len(obs[('sensorChannelNumber', metaDataName)])

        # Initialize boolean False array to aggregate checks
        chk_obs = np.zeros(obs[(obs_key, 'ObsValue')].shape[0], dtype=bool)

        for j in range(nrec):
            # Perform physical reality and PreQC check for current channel
            is_bad_data = (
                (obs[(obs_key, 'ObsValue')][:, j] < kmin) |
                (obs[(obs_key, 'ObsValue')][:, j] > kmax) |
                (obs[(obs_key, 'PreQC')][:, j] > 0)
            )

            obs[(obs_key, 'ObsValue')][is_bad_data, j] = float_missing_value
            # Create mask True when bad data AND current PreQC is 0
            mask = is_bad_data & (obs[(obs_key, 'PreQC')][:, j] == 0)
            obs[(obs_key, 'PreQC')][mask, j] = 2
            # Use OR operator to accumulate checks
            chk_obs = chk_obs | is_bad_data

    # reject all channels if any are bad
#   for j in range(nrec):
#       obs[(obs_key, 'ObsValue')][chk_obs, j] = float_missing_value

#   obs = add_to_preQC(obs, chk_obs)
    return obs


def add_to_preQC(obs, chk_array):
    key_map = {
        ('brightnessTemperature', 'PreQC'): 'brightnessTemperature',
        ('albedo', 'PreQC'): 'albedo'
    }

    k = None
    for key_tuple, key_str in key_map.items():
        if key_tuple in obs:
            k = key_str
            break
    if not k:
        return obs

    obs[(k, "PreQC")][chk_array, :] = 1
    return obs


def get_dimDict_varDims(obs_scene, dataset='IR_108', albedo=False):

    """
    define dimensions using  IODA conventions

    Args:
        obs_scene - Scene structure from satpy 

    Returns:
        VarDims, DimDict
    """

    nlocs = obs_scene[dataset].size
    # pass parameters to the IODA writer
    VarDims = {
        'brightnessTemperature': ['Location', 'Channel'],
        'sensorChannelNumber': ['Channel'],
    }

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    set_obspace_attributes(VarAttrs)
    set_metadata_attributes(VarAttrs)

    if albedo:
        k = 'albedo'
        VarAttrs[(k, 'ObsValue')]['_FillValue'] = float_missing_value
        VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
        VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
        VarAttrs[(k, 'ObsValue')]['units'] = '%'
        VarAttrs[(k, 'ObsError')]['units'] = '%'
        # VarAttrs[(k, 'PreQC')]['units'] = 'unitless'
    else:
        k = 'brightnessTemperature'
        VarAttrs[(k, 'ObsValue')]['_FillValue'] = float_missing_value
        VarAttrs[(k, 'ObsError')]['_FillValue'] = float_missing_value
        VarAttrs[(k, 'PreQC')]['_FillValue'] = int_missing_value
        VarAttrs[(k, 'ObsValue')]['units'] = 'K'
        VarAttrs[(k, 'ObsError')]['units'] = 'K'
        # VarAttrs[(k, 'PreQC')]['units'] = 'unitless'

    # Replace 'obs_scene' with the variable name of your satpy Scene object
    data_info_list = [DataIdInfo(data_id) for data_id in obs_scene.keys()]

    # print list of objects containing name and central wavelength
#   for info in data_info_list:
#       print(f"Data ID Name: {info.name}, Central Wavelength: {info.central_wavelength} µm")

    DimDict = {
        'Location': nlocs,
        'Channel': len(data_info_list),
    }

    return VarDims, VarAttrs, DimDict


def get_WMO_sat_ID(satellite_name):
    """
    get the WMO BUFR satellite identifier
    Args:
        satellite_name - string input

    Returns:
        WMO_sat_ID
    """

    # Create a tuple of the satellite names
    meteosat_series = ('Meteosat-8', 'Meteosat-9', 'Meteosat-10', 'Meteosat-11')

    if satellite_name in meteosat_series:
        WMO_sat_ID = 267
    else:
        # Code for other satellite IDs
        WMO_sat_ID = -1
    return WMO_sat_ID


def get_metadata(scn, dataset='IR_108'):

    """
    retrieve specific metaData from attributes

    Args:
        scn - Scene structure from satpy

    Returns:
        satellite_name, instrument_name, satellite_altitude
    """

    satellite_name = scn[dataset].attrs['platform_name']
    instrument_name = scn[dataset].attrs['sensor']
    satellite_altitude = scn[dataset].attrs['orbital_parameters']['satellite_actual_altitude']
    return satellite_name, instrument_name, satellite_altitude


def get_pixel_time(scn, target_area, dataset='IR_108'):

    """
    get a dateTime for each pixel and remap to target_area projection

    Args:
        scn - Scene structure from satpy
        target_area - destination project
        dataset (optional) - dataset from which time will be taken

    Returns:
       resampled_pixel_time - time on new target_area projection
    """

    # Get the acquisition times for each row
    acq_times_dt64 = scn[dataset].coords['acq_time'].values

    # Convert nanoseconds since epoch to seconds by dividing by 1 billion
    acq_times = acq_times_dt64.astype('int64') / 1_000_000_000

    # Reshape the 1D time array into a 2D array and repeat it for each column
    num_cols = scn[dataset].shape[1]
    pixel_time_array = np.tile(acq_times.reshape(-1, 1), (1, num_cols))

    # --- Get the original area definition from the scene ---
    source_area = scn[dataset].attrs['area']

    # --- Perform the manual resampling of the time data ---
    resampled_pixel_time = resample_nearest(
        source_area,
        pixel_time_array,
        target_area,
        radius_of_influence=50000, # Example radius in meters
        fill_value=np.datetime64('NaT') # Not a Time fill value for datetime data
    )

    return resampled_pixel_time


def resample_ancillary_data(scn, dataset_name, target_area, source_dataset='IR_108'):
    """
    Resamples an ancillary dataset from the original scene to a new target area.

    Args:
        scn (satpy.Scene): The original scene with the loaded data.
        dataset_name (str): The name of the ancillary dataset to resample.
        target_area (pyresample.AreaDefinition): The destination grid.
        source_dataset (str): A primary dataset name to get the original area definition.

    Returns:
        numpy.ndarray: The resampled data array.
    """
    # Get the original data from the scene
    ancillary_data = scn[dataset_name].data

    # Get the source grid from a primary channel
    source_area = scn[source_dataset].attrs['area']

    # Perform the manual resampling
    resampled_data = resample_nearest(
        source_area,
        ancillary_data,
        target_area,
        radius_of_influence=50000,
        fill_value=np.nan  # Use NaN for float data
    )
    return resampled_data


def main():

    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
    import os
    desc = 'Convert SEVIRI Native L1B into IODA convention use a netCDF4 backend'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="full path name of satellite observation input file(s)",
        type=str, nargs='+', required=True, default=None)
    required.add_argument(
        '-o', '--output',
        help='name of the output netCDF IODA-compliant file',
        type=str, required=True, default='output.nc')
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--resolution',
        help='output resolution in degrees on fixed lat lon grid',
        type=str, required=False, default=0.1)
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDTHHMMSSZ",
        help="base dateTime for observation window",
        type=str, required=False, default=None)

    args = parser.parse_args()

    GlobalAttrs['converter'] = os.path.basename(__file__)
    obs_scene, obs_dateTime = get_seviri_scene(args.input)

    VarDims, VarAttrs, DimDict = get_dimDict_varDims(obs_scene)
    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    obs = variables_to_obs(obs_scene, obs_dateTime, VarDims)

    del(obs_scene)
    del(obs_dateTime)
    for k in obs.keys():
        print(f"{k=}  {np.shape(obs[k])}  {np.min(obs[k])}  {np.max(obs[k])}  {np.mean(obs[k])}")
#   pdb.set_trace()
#   sys.exit()
    # write everything out
    writer.BuildIoda(obs, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
