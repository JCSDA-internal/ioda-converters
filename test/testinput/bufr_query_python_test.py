# (C) Copyright 2023 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

from pyiodaconv import bufr
import numpy as np


def test_bufr2ioda():
    YAML_PATH = './testinput/bufr_ncep_1bamua_ta.yaml'

    container = bufr.Parser(YAML_PATH).parse()

    categories = container.allSubCategories()
    for id in ['n18', 'metop-b', 'n19', 'metop-c', 'metop-a']:
       data = container.get('variables/antennaTemperature', [id])
       paths = container.getPaths('variables/antennaTemperature', [id])

       container.set('variables/antennaTemperature', data, [id])
       container.add('variables/antennaTemperature1', data, paths, [id])

    iodaDescription = bufr.IodaDescription(YAML_PATH)
    iodaDescription.add_variable(name='obsData/antennaTemperature1',
                                 source='variables/antennaTemperature1',
                                 units='K')

    bufr.IodaEncoder(iodaDescription).encode(container)

def test_basic_query():
    DATA_PATH = './testinput/gdas.t00z.1bhrs4.tm00.bufr_d'

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
    q.add('radiance', '*/BRIT{1}/TMBR')
    q.add('radiance_all', '*/BRIT/TMBR')

    # Open the BUFR file and execute the QuerySet
    with bufr.File(DATA_PATH) as f:
        r = f.execute(q)

    # Use the ResultSet returned to get numpy arrays of the data
    lat = r.get('latitude')
    rad = r.get('radiance')
    rad_all = r.get('radiance_all')

    # Validate the values that were returned
    assert np.allclose(lat[0:3], np.array([166.7977, 166.4078, 165.992]))
    assert np.allclose(rad[0:3], np.array([198.69, 254.06, 233.85]))
    assert len(rad_all.shape) == 2

    datetimes = r.get_datetime('year', 'month', 'day', 'hour', 'minute', 'second')
    assert datetimes[5] == np.datetime64('2020-10-26T21:00:01')
    assert datetimes.fill_value == np.datetime64('1970-01-01T00:00:00')


def test_string_field():
    DATA_PATH = './testinput/gdas.t12z.adpupa.tm00.bufr_d'

    # Make the QuerySet for all the data we want
    q = bufr.QuerySet()
    q.add('borg', '*/BID/BORG')

    # Open the BUFR file and execute the QuerySet
    with bufr.File(DATA_PATH) as f:
        r = f.execute(q)

    # Use the ResultSet returned to get numpy arrays of the data
    borg = r.get('borg')

    # Validate the values that were returned
    assert (np.all(borg[0][0:3] == ['KWBC', 'KWBC', 'KAWN']))


def test_long_str_field():
    DATA_PATH ='./testinput/gdas.t06z.snocvr.tm00.bufr_d'

    # Make the QuerySet for all the data we want
    q = bufr.QuerySet()
    q.add('lid', '*/WGOSLID')

    # Open the BUFR file and execute the QuerySet
    with bufr.File(DATA_PATH) as f:
        r = f.execute(q)

    # Use the ResultSet returned to get numpy arrays of the data
    lid = r.get('lid')

    # Validate the values that were returned
    assert (lid[0] == '570282')
    assert (lid[6] == '613180')
    assert (np.all(lid[0:7].mask == [False, True, True, True, True, True, False]))

def test_type_override():
    DATA_PATH = './testinput/gdas.t00z.1bhrs4.tm00.bufr_d'

    # Make the QuerySet for all the data we want
    q = bufr.QuerySet()
    q.add('day', '*/DAYS')
    q.add('longitude', '*/CLAT')

    # Open the BUFR file and execute the QuerySet
    with bufr.File(DATA_PATH) as f:
        r = f.execute(q)

    day = r.get('day')
    day_float = r.get('day', type='float')

    assert day.dtype == 'int32'
    assert day_float.dtype == 'float32'

    lat = r.get('longitude')
    lat_int = r.get('longitude', type='int')

    assert lat.dtype == 'float32'
    assert lat_int.dtype == 'int32'
    assert lat_int.fill_value == 2147483647  # the max int32 value

def test_invalid_query():
    q = bufr.QuerySet()

    try:
        q.add('latitude', '!!! */CLON')
    except Exception as e:
        return

    assert False, "Didn't throw exception for invalid query."



if __name__ == '__main__':
    test_bufr2ioda()
    # test_basic_query()
    # test_string_field()
    # test_long_str_field()
    # test_type_override()
    # test_invalid_query()
