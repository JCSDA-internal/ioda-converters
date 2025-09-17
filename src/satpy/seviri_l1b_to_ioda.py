
import xarray as xr
import pyresample
from pyresample.kd_tree import resample_nearest
from pyproj import CRS
from satpy.scene import Scene
from satpy.readers import seviri_l1b_native
# from satpy.readers import get_reader_datasets
# from satpy.utils import get_available_readers

import numpy as np
from datetime import datetime, timedelta

import pdb
import sys

import pyresample
from pyproj import CRS
import numpy as np


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



def get_metadata(scn, dataset='IR_108'):

    # retrieve specific metaData from attributes
    #
    #   Input:
    #       scn - Scene structure from satpy
    #
    #   Output:
    #       satellite_name, instrument_name, satellite_altitude

    satellite_name = scn[dataset].attrs['platform_name']
    instrument_name = scn[dataset].attrs['sensor']
    satellite_altitude = scn[dataset].attrs['orbital_parameters']['satellite_actual_altitude']
    return satellite_name, instrument_name, satellite_altitude


def get_pixel_time(scn, target_area, dataset='IR_108'):

    # get a dateTime for each pixel and remap to target_area projection
    #
    #   Input:
    #       scn - Scene structure from satpy
    #       target_area - destination project
    #       dataset (optional) - dataset from which time will be taken
    #
    #   Output:
    #      resampled_pixel_time - time on new target_area projection

    # Get the acquisition times for each row
    acq_times = scn[dataset].coords['acq_time'].values

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


# filename(s) to be read
filenames = ['MSG4-SEVI-MSG15-0100-NA-20220622191243.890000000Z-NA.nat']

# what datasets are available
header = seviri_l1b_native.read_header(filenames[0])
available_datasets = seviri_l1b_native.get_available_channels(header)
aload = [k for k, v in available_datasets.items() if v]

# load Scene
scn = Scene(reader="seviri_l1b_native", filenames=filenames, reader_kwargs={'fill_disk': True})
# scn.load(['IR_108'])  # test single channel
scn.load(aload)

satellite_name, instrument_name, satellite_altitude = get_metadata(scn)

# Create a target area with the default 0.1 degree resolution
target_area = create_latlon_area()
print(f"target area shape: {target_area.shape}")

# Create a target area with a higher 0.05 degree resolution
# target_area = create_latlon_area(resolution_deg=0.05)
# print(f"target area shape: {target_area.shape}")

# Resample the scene to the new target area
scn_latlon = scn.resample(target_area)

# get a time for each pixel on new target area
locationDateTime = get_pixel_time(scn, target_area)

# Access the new latitude and longitude coordinates
latitude = scn_latlon['IR_108'].coords['y']
longitude = scn_latlon['IR_108'].coords['x']

ir_data = scn_latlon['IR_108'].data
vis_data = scn_latlon['VIS008'].data
print("Shape of latitude resampled data:", latitude.shape)
print("\nShape of longitude resampled data:", longitude.shape)
print("\nShape of IR_108 resampled data:", ir_data.shape)
print("\nShape of VIS008 resampled data:", vis_data.shape)
print("\nShape of dateTime resampled data:", locationDateTime.shape)

pdb.set_trace()
sys.exit()
