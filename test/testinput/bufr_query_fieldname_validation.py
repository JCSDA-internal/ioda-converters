# (C) Copyright 2023 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

from pyiodaconv import bufr

def test_field_val():
    DATA_PATH = './testinput/gdas.t00z.1bhrs4.tm00.bufr_d'

    # Make the QuerySet for all the data we want
    q = bufr.QuerySet()
    q.add('latitude', '*/CLON')

    # Open the BUFR file and execute the QuerySet
    with bufr.File(DATA_PATH) as f:
        r = f.execute(q)

    try:
        r.get('lattittude')
    except Exception as e:
        return  # Success!

    assert False, "Didn't throw exception for invalid field name."

def test_groupby_field_val():
    DATA_PATH = './testinput/gdas.t00z.1bhrs4.tm00.bufr_d'

    # Make the QuerySet for all the data we want
    q = bufr.QuerySet()
    q.add('latitude', '*/CLON')

    # Open the BUFR file and execute the QuerySet
    with bufr.File(DATA_PATH) as f:
        r = f.execute(q)

    try:
        r.get('latitude', 'latittude')
    except Exception as e:
        return  # Success!

    assert False, "Didn't throw exception for invalid groupby field name."


if __name__ == '__main__':
    test_field_val()
    test_groupby_field_val()
