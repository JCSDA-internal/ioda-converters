

import pyresample
from pyproj import CRS
from satpy.scene import Scene

import numpy as np
from datetime import datetime, timedelta

import pdb
import sys


# Your existing setup
filenames = ['MSG4-SEVI-MSG15-0100-NA-20220622191243.890000000Z-NA.nat']
scn = Scene(reader="seviri_l1b_native", filenames=filenames, reader_kwargs={'fill_disk': True})
scn.load(['IR_108'])

satellite_name = scn['IR_108'].attrs['platform_name']
instrument_name = scn['IR_108'].attrs['sensor']
satellite_altitude = scn['IR_108'].attrs['orbital_parameters']['satellite_actual_altitude']
num_rows = scn['IR_108'].shape[0]
num_cols = scn['IR_108'].shape[1]


# Get the acquisition times for each row
acq_times = scn['IR_108'].coords['acq_time'].values
# Reshape the 1D time array into a 2D array and repeat it for each column
pixel_time_array = np.tile(acq_times.reshape(-1, 1), (1, num_cols))

# print(f"Time of the first row: {first_row_time}")
# print(f"Time of the last row: {last_row_time}")
# print(f"Total scan duration: {scan_duration}")

# To find the time for a specific pixel's row:
pixel_row_index = 500
pixel_time = acq_times[pixel_row_index].item()
print(f"\nTime for a pixel in row {pixel_row_index}: {pixel_time}")

# You can access a specific pixel's time like this:
specific_pixel_time = pixel_time_array[500, 1000].item()
print(f"Time for pixel at (500, 1000): {specific_pixel_time}")

# Description: MSG SEVIRI Full Earth Scanning service area definition with 3 km resolution
# k='orbital_parameters'  {'projection_longitude': 0.0, 
#                          'projection_latitude': 0.0, 
#                          'projection_altitude': 35785831.0, 
#                          'satellite_nominal_longitude': 0.0, 
#                          'satellite_nominal_latitude': 0.0, 
#                          'satellite_actual_longitude': 0.07105862061307676, 
#                          'satellite_actual_latitude': 0.18968877529929085, 
#                          'satellite_actual_altitude': 35781956.88290232}
# k='wavelength'  10.8 µm (9.8-11.8 µm)
# k='platform_name'  Meteosat-11
# k='sensor'  seviri
# k='time_parameters'  {'nominal_start_time': datetime.datetime(2022, 6, 22, 19, 0), 'nominal_end_time': datetime.datetime(2022, 6, 22, 19, 15), 'observation_start_time': datetime.datetime(2022, 6, 22, 19, 0, 11, 248000), 'observation_end_time': datetime.datetime(2022, 6, 22, 19, 12, 43, 890000)}
# k='start_time'  2022-06-22 19:00:00
# k='end_time'  2022-06-22 19:15:00
# Number of columns: 3712
# Number of rows: 3712
# k='resolution'  3000.403165817
# k='_satpy_id'  DataID(name='IR_108', wavelength=WavelengthRange(min=9.8, central=10.8, max=11.8, unit='µm'), resolution=3000.403165817, calibration=<2>, modifiers=())
# pdb.set_trace()
# sys.exit()

# Step 1: Define the target CRS (WGS84 lat/lon)
target_crs = CRS.from_epsg(4326)

# Step 2: Create a new AreaDefinition for a known, full-disk lat/lon grid.
# This version is more explicit to avoid ProjError.
target_area = pyresample.create_area_def('latlon_area',
                                         proj_id='latlon',
                                         projection={'proj': 'longlat', 'ellps': 'WGS84', 'no_defs': True},
                                         area_extent=(-81, -81, 81, 81),
                                         shape=(1620, 1620)) # Reduced shape for testing

# Step 3: Resample the scene to the new target area
scn_latlon = scn.resample(target_area)

# Step 4: Access the new latitude and longitude coordinates
latitude = scn_latlon['IR_108'].coords['y']
longitude = scn_latlon['IR_108'].coords['x']

print("Latitude coordinates:")
print(latitude)

print("\nLongitude coordinates:")
print(longitude)
