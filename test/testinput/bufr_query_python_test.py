import bufr
import numpy as np

DATA_PATH = './testinput/gdas.t00z.1bhrs4.tm00.bufr_d'

def test_basic_query():

    # Make the QuerySet for all the data we want
    q = bufr.QuerySet()
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


def test_invalid_query():
    with bufr.File(DATA_PATH) as f:
        q = bufr.QuerySet()

        try:
            q.add('latitude', '!!! */CLON')
        except Exception as e:
            return

    assert False, "Didn't throw exception for invalid query."


if __name__ == '__main__':
    test_basic_query()
    test_invalid_query()

