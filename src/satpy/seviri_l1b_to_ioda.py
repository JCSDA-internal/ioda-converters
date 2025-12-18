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
import pyproj
from pyproj import CRS
import pyresample
from pyresample.kd_tree import resample_nearest
import re
from satpy.scene import Scene
from satpy.readers import seviri_l1b_native

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import (
    compute_scan_angle,
    concat_obs_dict,
    epoch,
    ioda_float_type,
    ioda_int_type,
    set_metadata_attributes,
    set_obspace_attributes,
)

# globals
Meteosat08_WMO_sat_ID = 55
Meteosat09_WMO_sat_ID = 56
Meteosat10_WMO_sat_ID = 57
Meteosat11_WMO_sat_ID = 70
Meteosat12_WMO_sat_ID = 71
Meteosat13_WMO_sat_ID = 72
Meteosat16_WMO_sat_ID = 75

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

GlobalAttrs = {
    "platformCommonName": "SEVIRI",
    "platformLongDescription": "EUMETSAT MeteoSat SEVIRI L1B Brightness Temperature Data",
    "sensorCentralWavelength": "[1.64, 3.92, 6.25, 7.35, 8.7, 9.66, 10.8, 12.0, 13.4]"
}
#   "platformLongDescription": "EUMETSAT MeteoSat SEVIRI L1B Reflectance Data",
#   "sensorCentralWavelength": "[0.56-0.71, 0.74-0.88, 0.6-0.9 um]"

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


def get_seviri_scene(args, resample=True, ref_dataset='IR_108'):

    """
    decode an EUMETSAT MeteoSat SEVIRI native L1B file using satpy

    Args:
        filename list - list of files to decode

    Returns:
       obs_scene - the resampled obs_scene to a common lat/lon projection
       ancillary_data - scene lat, lon, dateTime, sensorZenigth and scanPosition
    """

    # filename(s) to be read example below
    # filenames = ['MSG4-SEVI-MSG15-0100-NA-20220622191243.890000000Z-NA.nat']

    # what datasets are available
    filenames = args.input
    resolution = args.resolution
    header = seviri_l1b_native.read_header(filenames[0])
    available_datasets = seviri_l1b_native.get_available_channels(header)
    aload = [k for k, v in available_datasets.items() if v]

    # load Scene
    scn = Scene(reader="seviri_l1b_native", filenames=filenames, reader_kwargs={'fill_disk': False})
    scn.load(aload)

    # ensure the the loaded datasets in the Scene are calibrated (version dependent)
    # scn.calibrate()

    # ancillary information
    # latitude, longitude and dateTime, satelliteZenith, sensorScan
    lat, lon, satellite_zenith_angle = get_zenith_angle(scn)
    # get a time for each pixel on new target area
    locationDateTime = get_pixel_time(scn)
    # scan position is the x-coordinate
    sensorScanPosition = get_scanPosition(scn)

    # set ancillary_data
    ancillary_data = {
        'lat': lat,
        'lon': lon,
        'dateTime': locationDateTime,
        'satellite_zenith_angle': satellite_zenith_angle,
        'sensor_scan_position': sensorScanPosition,
    }

    if not resample:
        return scn, ancillary_data

    # Create a target area with the default degree resolution
    target_area = create_latlon_area(resolution_deg=resolution)
    # print(f"target area shape: {target_area.shape}")

    resampled_satellite_zenith = resample_ancillary_data(scn, satellite_zenith_angle, target_area)
    resampled_dateTime = resample_ancillary_data(scn, locationDateTime, target_area, missing_value=np.datetime64('NaT'))

    # Resample the scene to the new target area
    scn_latlon = scn.resample(target_area)

    # get a psuedo scanPosition
    sensorScanPosition = get_scanPosition(scn_latlon)

    latitude = scn_latlon[ref_dataset].coords['y']
    longitude = scn_latlon[ref_dataset].coords['x']
    lon_2d, lat_2d = np.meshgrid(longitude, latitude)

#   # Access the new latitude and longitude coordinates (example)
#   latitude = scn_latlon['IR_108'].coords['y']
#   longitude = scn_latlon['IR_108'].coords['x']

#   ir_data = scn_latlon['IR_108'].data
#   vis_data = scn_latlon['VIS008'].data

    # set ancillary_data
    ancillary_data = {
        'lat': lat_2d,
        'lon': lon_2d,
        'dateTime': resampled_dateTime,
        'satellite_zenith_angle': resampled_satellite_zenith,
        'sensor_scan_position': sensorScanPosition,
    }

    return scn_latlon, ancillary_data


def create_latlon_area(resolution_deg=0.25, area_extent=(-81, -81, 81, 81)):
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


def variables_to_obs(obs_scene, ancillary_data, VarDims, albedo=False, dataset='IR_108', apply_gross_qc=True):
    """
    Move data from satpy Scene into IODA convention

    Args:
        obs_scene: satpy Scene of satellite data
        obs_lat: 2d array of latitude
        obs_lon: 2d array of longitude
        obs_dateTime: dateTime for each pixel

    Returns:
        obs: dictionary following IODA conventions

    SEVIRI definition of channel:
        Channel 01 - VIS006, Central Wavelength: 0.635 µm
        Channel 02 - VIS008, Central Wavelength: 0.81 µm
        Channel 03 - IR_016, Central Wavelength: 1.64 µm
        Channel 04 - IR_039, Central Wavelength: 3.92 µm
        Channel 05 - WV_062, Central Wavelength: 6.25 µm
        Channel 06 - WV_073, Central Wavelength: 7.35 µm
        Channel 07 - IR_087, Central Wavelength: 8.7 µm
        Channel 08 - IR_097, Central Wavelength: 9.66 µm
        Channel 09 - IR_108, Central Wavelength: 10.8 µm
        Channel 10 - IR_120, Central Wavelength: 12.0 µm
        Channel 11 - IR_134, Central Wavelength: 13.4 µm
        Channel 12 - HRV, Central Wavelength: 0.7 µm
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
        obs[('sensorChannelNumber', metaDataName)] = np.array([1, 2, 12], dtype='int32')

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
        obs[('sensorChannelNumber', metaDataName)] = np.array(np.arange(bt_nchans)+3, dtype='int32')

    satellite_name, instrument_name, satellite_altitude = get_metadata(obs_scene)
    WMO_sat_ID = get_WMO_sat_ID(satellite_name)

    obs[("latitude", metaDataName)] = np.array(ancillary_data['lat'].flatten(), dtype='float32')
    obs[("longitude", metaDataName)] = np.array(ancillary_data['lon'].flatten(), dtype='float32')
    obs[('dateTime', metaDataName)] = np.array(ancillary_data['dateTime'].flatten(), dtype='int64')
    obs[('satelliteIdentifier', metaDataName)] = np.full((nlocs), WMO_sat_ID, dtype='int32')
    obs[('stationElevation', metaDataName)] = np.full((nlocs), satellite_altitude, dtype='float32')
    obs[('sensorScanPosition', metaDataName)] = np.array(ancillary_data['sensor_scan_position'].flatten(), dtype='int32')
    obs[('sensorZenithAngle', metaDataName)] = np.array(ancillary_data['satellite_zenith_angle'].flatten(), dtype='float32')
    obs[('sensorViewAngle', metaDataName)] = obs[('sensorZenithAngle', metaDataName)]
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
        obs[('albedo', "ObsValue")] = []
        obs[('albedo', "ObsError")] = []
        obs[('albedo', "PreQC")] = []
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
                (obs[(obs_key, 'ObsValue')][:, j] < kmin)
                | (obs[(obs_key, 'ObsValue')][:, j] > kmax)
                | ~np.isfinite(obs[(obs_key, 'ObsValue')][:, j])
                | (obs[(obs_key, 'PreQC')][:, j] > 0)
            )
            obs[(obs_key, 'ObsValue')][is_bad_data, j] = float_missing_value

            # Create a mask for values that were bad and have PreQC == 0
            mask = is_bad_data & (obs[(obs_key, 'PreQC')][:, j] == 0)
            obs[(obs_key, 'PreQC')][mask, j] = 2

            # Accumulate checks across all channels
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


def get_obs_properties(obs_scene, dataset='IR_108', albedo=False):

    """
    define dimensions using  IODA conventions

    Args:
        obs_scene - Scene structure from satpy

    Returns:
        VarDims, VarAttrs, DimDict
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

    # get information from obs_scene
    data_info_list = [DataIdInfo(data_id) for data_id in obs_scene.keys()]
    nrec = 0
    if albedo:       # if albedo VIS (1 & 2) and HRV (12)
        nrec = sum(1 for item in data_info_list if item.name.startswith('HRV') or item.name.startswith('VIS'))
        assert nrec == 3, "ERROR: nrec not equal to 3 (Actual: {nrec}), reflectance or albedo expect 3 channels (VIS006, VIS008, HRV)"
        channelNumber = np.array([1, 2, 12], dtype='int32')
    else:            # else sum IR entries
        nrec = sum(1 for item in data_info_list if item.name.startswith('IR') or item.name.startswith('WV'))
        channelNumber = np.array(np.arange(nrec)+3, dtype='int32')

    # print list of objects containing name and central wavelength
#   for info in data_info_list:
#       print(f"Data ID Name: {info.name}, Central Wavelength: {info.central_wavelength} µm")

    DimDict = {
        'Location': nlocs,
        'Channel': channelNumber,
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

    print(f"{satellite_name=}")
    if 'Meteosat-8' in satellite_name:
        WMO_sat_ID = Meteosat08_WMO_sat_ID
    elif 'Meteosat-9' in satellite_name:
        WMO_sat_ID = Meteosat09_WMO_sat_ID
    elif 'Meteosat-10' in satellite_name:
        WMO_sat_ID = Meteosat10_WMO_sat_ID
    elif 'Meteosat-11' in satellite_name:
        WMO_sat_ID = Meteosat11_WMO_sat_ID
    else:
        # Code for other satellite IDs
        WMO_sat_ID = -1
    return WMO_sat_ID


def get_platform_short_name(WMO_sat_ID):
    """
    use the WMO BUFR satellite identifier to create a platform shortname
    Args:
        WMO BUFR satellite Identifier - integer input

    Returns:
        platform
    """

    if WMO_sat_ID == Meteosat08_WMO_sat_ID:
        platform = 'm08'
    elif WMO_sat_ID == Meteosat09_WMO_sat_ID:
        platform = 'm09'
    elif WMO_sat_ID == Meteosat10_WMO_sat_ID:
        platform = 'm10'
    elif WMO_sat_ID == Meteosat11_WMO_sat_ID:
        platform = 'm11'
    else:
        platform = 'meteosat'

    return platform


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


# def get_pixel_time(scn, target_area, dataset='IR_108'):
def get_pixel_time(scn, dataset='IR_108'):

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

    return pixel_time_array


def get_zenith_angle(scn, source_dataset='IR_108'):

    ir_data = scn[source_dataset]

    # Get the projection (AreaDefinition) and pixel coordinates (x, y)
    area_def = ir_data.attrs['area']
    x_coords = ir_data['x'].values
    y_coords = ir_data['y'].values

    # Create a PyProj Transformer to convert (x, y) to (lon, lat)
    proj = pyproj.Proj(area_def.proj_dict)

    # Create 2D arrays of the x and y coordinates
    X, Y = np.meshgrid(x_coords, y_coords)

    # Transform the (x, y) coordinates to (lon, lat)
    lon, lat = proj(X, Y, inverse=True)

    # Get the full projection definition to get the sub-satellite point (sat_lon) and altitude (H).
    proj_dict = area_def.proj_dict

    # Extract fixed satellite parameters from the projection
    sat_lon = proj_dict.get('lon_0', 0.0)    # Satellite longitude (sub-satellite point)
    H = proj_dict.get('h', 35785831.0)       # Satellite altitude in meters
    R_e = proj_dict.get('a', 6378137.0)      # Earth radius (semi-major axis) in meters

    # Convert all degrees to radians
    lon_rad = np.deg2rad(lon)
    lat_rad = np.deg2rad(lat)
    sat_lon_rad = np.deg2rad(sat_lon)

    # Compute the angle (beta) between the satellite-center and pixel-center vectors
    # (This is the Earth-Centered Angle)
    # Formula: cos(beta) = cos(lat) * cos(lon - sat_lon)
    cos_beta = np.cos(lat_rad) * np.cos(lon_rad - sat_lon_rad)
    beta = np.arccos(cos_beta)

    # Compute the satellite zenith angle (theta_zen)
    # Formula: theta_zen = 90 - arcsin( (R_e * sin(beta)) / sqrt(R_e^2 + H^2 - 2*R_e*H*cos(beta)) )

    # The distance from the satellite to the pixel (r_s)
    # Note: r_s^2 = R_e^2 + H^2 - 2 * R_e * H * cos(beta)  (where H = R_e + satellite altitude)
    r_s = np.sqrt(R_e**2 + (R_e + H)**2 - 2 * R_e * (R_e + H) * cos_beta)    # Corrected distance term
    # The corrected H is the total distance from the center of the Earth to the satellite: R_e + sat_alt
    # Let's redefine H to be the total distance:
    R_sat = R_e + H

    r_s = np.sqrt(R_e**2 + R_sat**2 - 2 * R_e * R_sat * cos_beta)

    # Calculate the satellite zenith angle (SZA)
    # Compute the angle (gamma) at the satellite
    # then use the Law of Sines/Law of Cosines on the Earth-Pixel-Satellite triangle
    # Zenith angle is the angle at the Pixel, which is 180 - (90 + gamma)

    # the standard vector approach for the Law of Sines:
    # sin(zenith) = (R_sat / r_s) * sin(beta)
    sin_theta_zen = (R_sat / r_s) * np.sin(beta)

    # Handle potential floating-point errors near 1 (limit to 1.0)
    sin_theta_zen = np.clip(sin_theta_zen, -1.0, 1.0)

    # The zenith angle (angle at the pixel) is asin(sin_theta_zen)
    theta_zen_rad = np.arcsin(sin_theta_zen)

    # Convert back to degrees
    satellite_zenith_angle_deg = np.rad2deg(theta_zen_rad)

    return lat, lon, satellite_zenith_angle_deg


def get_scanPosition(scn, dataset='IR_108'):
    # create a psuedo scanPosition the x-coordinate index
    num_rows, num_columns = scn[dataset].shape
    sensorScanPosition_1d = np.arange(num_columns) + 1
    sensorScanPosition = np.tile(sensorScanPosition_1d, (num_rows, 1))
    return sensorScanPosition


def resample_ancillary_data(scn, ancillary_data, target_area, source_dataset='IR_108', missing_value=np.nan):
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
#   ancillary_data = scn[dataset_name].data

    # Get the source grid from a primary channel
    source_area = scn[source_dataset].attrs['area']

    # Perform the manual resampling
    resampled_data = resample_nearest(
        source_area,
        ancillary_data,
        target_area,
        radius_of_influence=50000,
        fill_value=missing_value,
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
        type=str, required=False, default=0.25)
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDTHHMMSSZ",
        help="base dateTime for observation window",
        type=str, required=False, default=None)

    args = parser.parse_args()

    GlobalAttrs['converter'] = os.path.basename(__file__)
    obs_scene, ancillary_data = get_seviri_scene(args)

    VarDims, VarAttrs, DimDict = get_obs_properties(obs_scene)

    obs = variables_to_obs(obs_scene, ancillary_data, VarDims)
    del obs_scene
    del ancillary_data
    platform = get_platform_short_name(obs[('satelliteIdentifier', metaDataName)][0])
    GlobalAttrs["platformCommonName"] = " ".join([GlobalAttrs["platformCommonName"], platform])

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    # write everything out
    writer.BuildIoda(obs, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
