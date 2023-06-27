
# (C) Copyright 2023 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

import numpy as np

from pyiodaconv import bufr
from pyioda import ioda_obs_space as ioda_ospace

DATA_PATH = './testinput/gdas.t00z.1bhrs4.tm00.bufr_d'
OUTPUT_PATH = './testrun/bufr_query_python_to_ioda_test.nc'

def test_bufr_to_ioda():

    # Make the QuerySet for all the data we want
    q = bufr.QuerySet()
    q.add('year', '*/YEAR')
    q.add('month', '*/MNTH')
    q.add('day', '*/DAYS')
    q.add('hour', '*/HOUR')
    q.add('minute', '*/MINU')
    q.add('second', '*/SECO')
    q.add('latitude', '*/CLON')
    q.add('longitude', '*/CLAT')
    q.add('radiance', '*/BRIT/TMBR')

    # Open the BUFR file and execute the QuerySet
    with bufr.File(DATA_PATH) as f:
        r = f.execute(q)

    # Use the ResultSet returned to get numpy arrays of the data
    lat = r.get('latitude')
    lon = r.get('longitude')
    rad = r.get('radiance')

    datetime = r.get_datetime('year', 'month', 'day', 'hour', 'minute', 'second')

    # Create the dimensions
    dims = {'Location': rad.shape[0],
            'Channel': rad.shape[1]}

    # Create the IODA ObsSpace
    obsspace = ioda_ospace.ObsSpace(OUTPUT_PATH, mode='w', dim_dict=dims)

    # Create the global attributes
    obsspace.write_attr('MyGlobal_str', 'My Global String Data')
    obsspace.write_attr('MyGlobal_int', [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

    # Create the variables
    obsspace.create_var('MetaData/Timestamp', dtype=np.int64, fillval=datetime.fill_value.astype('int64')) \
        .write_attr('units', 'seconds_since_epoch') \
        .write_attr('long_name', 'Timestamp') \
        .write_data(datetime.astype('int64'))

    obsspace.create_var('MetaData/Latitude', dtype=lat.dtype, fillval=lat.fill_value) \
        .write_attr('units', 'degrees_north') \
        .write_attr('long_name', 'Latitude') \
        .write_attr('valid_range', [-90.0, 90.0]) \
        .write_data(lat)

    obsspace.create_var('MetaData/Longitude', dtype=lon.dtype, fillval=lon.fill_value) \
        .write_attr('units', 'degrees_east') \
        .write_attr('long_name', 'Longitude') \
        .write_attr('valid_range', [-180.0, 180.0]) \
        .write_data(lon)

    obsspace.create_var('ObsValue/brightnessTemperature',  dtype=rad.dtype,
                        dim_list=['Location', 'Channel'], fillval=rad.fill_value) \
        .write_attr('units', 'K') \
        .write_attr('long_name', 'ATMS Observed (Uncorrected) Brightness Temperature') \
        .write_attr('valid_range', [100.0, 500.0]) \
        .write_data(rad)

if __name__ == '__main__':
    test_bufr_to_ioda()

