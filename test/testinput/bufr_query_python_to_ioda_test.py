
# (C) Copyright 2023 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

import numpy as np

from pyiodaconv import bufr
from pyioda import ioda

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

    # Write the data to an IODA file

    # Create the variable creation parameters. Make sure to set the fill value and set one on
    # each variable. Otherwise, the "missing" will be set to 0.0.

    pfloat = ioda.VariableCreationParameters()
    pfloat.setFillValue.float(rad.fill_value)
    pfloat.compressWithGZIP()

    pdatetime = ioda.VariableCreationParameters()
    pdatetime.setFillValue.int(datetime.fill_value.astype(np.int64))
    pdatetime.compressWithGZIP()


    g = ioda.Engines.HH.createFile(name=OUTPUT_PATH,
                                   mode=ioda.Engines.BackendCreateModes.Truncate_If_Exists)

    # Create the dimensions
    num_locs = rad.shape[0]
    num_chans = rad.shape[1]

    dim_location = g.vars.create('Location', ioda.Types.int32, [num_locs])
    dim_location.scales.setIsScale('Location')

    dim_channel = g.vars.create('Channel', ioda.Types.int32, [num_chans])
    dim_channel.scales.setIsScale('Channel')

    # # Create some globals
    my_global = g.atts.create('MyGlobal_str', ioda.Types.str, [1])
    my_global.writeVector.str(['My Global String Data'])

    my_global_int = g.atts.create('MyGlobal_int', ioda.Types.int, [10])
    my_global_int.writeVector.int([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

    # Create the variables
    timestamp = g.vars.create('MetaData/Timestamp', ioda.Types.int64, scales=[dim_location], params=pdatetime)
    timestamp.atts.create('units', ioda.Types.str).writeVector.str(['seconds_since_epoch'])
    timestamp.atts.create('long_name', ioda.Types.str).writeVector.str(['Timestamp'])

    longitude = g.vars.create('MetaData/Longitude', ioda.Types.float, scales=[dim_location], params=pfloat)
    longitude.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([-180, 180])
    longitude.atts.create('units', ioda.Types.str).writeVector.str(['degrees_east'])
    longitude.atts.create('long_name', ioda.Types.str).writeVector.str(['Longitude'])

    latitude = g.vars.create('MetaData/Latitude', ioda.Types.float,  scales=[dim_location], params=pfloat)
    latitude.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([-90, 90])
    latitude.atts.create('units', ioda.Types.str).writeVector.str(['degrees_north'])
    latitude.atts.create('long_name', ioda.Types.str).writeVector.str(['Latitude'])

    tb = g.vars.create('ObsValue/brightnessTemperature', ioda.Types.float, scales=[dim_location, dim_channel], params=pfloat)
    tb.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([100, 500])
    tb.atts.create('units', ioda.Types.str).writeVector.str(['K'])
    tb.atts.create('long_name', ioda.Types.str).writeVector.str(['ATMS Observed (Uncorrected) Brightness Temperature'])

    # Write the data to the variables
    timestamp.writeNPArray.int64(datetime.astype(np.int64))
    longitude.writeNPArray.float(lon.flatten())
    latitude.writeNPArray.float(lat.flatten())
    tb.writeNPArray.float(rad.flatten())

if __name__ == '__main__':
    test_bufr_to_ioda()

