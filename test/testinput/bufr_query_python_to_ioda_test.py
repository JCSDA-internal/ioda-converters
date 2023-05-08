
# (C) Copyright 2023 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

import numpy as np

import bufr
import ioda

DATA_PATH = './testinput/gdas.t00z.1bhrs4.tm00.bufr_d'
OUTPUT_PATH = './testrun/bufr_query_python_to_ioda_test.nc'

def test_bufr_to_ioda():

    # Make the QuerySet for all the data we want
    q = bufr.QuerySet()
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


    # Write the data to an IODA file
    g = ioda.Engines.HH.createFile(name=OUTPUT_PATH,
                                   mode=ioda.Engines.BackendCreateModes.Truncate_If_Exists)

    # Create the dimensions
    num_locs = rad.shape[0]
    num_chans = rad.shape[1]

    dim_location = g.vars.create('Location', ioda.Types.int32, [num_locs])
    dim_location.scales.setIsScale('Location')

    dim_channel = g.vars.create('Channel', ioda.Types.int32, [num_chans])
    dim_channel.scales.setIsScale('Channel')

    # Create the variables
    longitude = g.vars.create('MetaData/Longitude', ioda.Types.float, scales=[dim_location])
    longitude.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([-180, 180])
    longitude.atts.create('units', ioda.Types.str).writeVector.str(['degrees_east'])
    longitude.atts.create('long_name', ioda.Types.str).writeVector.str(['Longitude'])

    latitude = g.vars.create('MetaData/Latitude', ioda.Types.float,  scales=[dim_location])
    latitude.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([-90, 90])
    latitude.atts.create('units', ioda.Types.str).writeVector.str(['degrees_north'])
    latitude.atts.create('long_name', ioda.Types.str).writeVector.str(['Latitude'])

    p1 = ioda.VariableCreationParameters()
    p1.setFillValue.float(rad.fill_value)
    p1.compressWithGZIP()

    tb = g.vars.create('ObsValue/brightnessTemperature', ioda.Types.float, scales=[dim_location, dim_channel], params=p1)
    tb.atts.create('valid_range', ioda.Types.float, [2]).writeVector.float([100, 500])
    tb.atts.create('units', ioda.Types.str).writeVector.str(['K'])
    tb.atts.create('long_name', ioda.Types.str).writeVector.str(['ATMS Observed (Uncorrected) Brightness Temperature'])

    # Write the data to the variables
    longitude.writeNPArray.float(lon.flatten())
    latitude.writeNPArray.float(lat.flatten())
    tb.writeNPArray.float(rad.flatten())

if __name__ == '__main__':
    test_bufr_to_ioda()

