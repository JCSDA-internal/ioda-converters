
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


def get_pixel_time(scn, target_area, dataset='IR_108'):

    # this will get a time for each pixel and remap to target_area projection
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

satellite_name = scn['IR_108'].attrs['platform_name']
instrument_name = scn['IR_108'].attrs['sensor']
satellite_altitude = scn['IR_108'].attrs['orbital_parameters']['satellite_actual_altitude']

# Define the target CRS (WGS84 lat/lon)
target_crs = CRS.from_epsg(4326)

# Create a new AreaDefinition for full-disk lat/lon grid
target_area = pyresample.create_area_def('latlon_area',
                                         proj_id='latlon',
                                         projection={'proj': 'longlat', 'ellps': 'WGS84', 'no_defs': True},
                                         area_extent=(-81, -81, 81, 81),
                                         shape=(1620, 1620)) # Reduced shape for testing


# Resample the scene to the new target area
scn_latlon = scn.resample(target_area)

locationDateTime = get_pixel_time(scn, target_area)
# locationDateTime = scn_latlon['pixel_time'].data

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
