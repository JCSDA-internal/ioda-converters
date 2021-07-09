import numpy
from netCDF4 import Dataset
from numpy import ma


class Goes16LatLon:

    def __init__(self, source_file_path, latlon_file_path):
        self._source_file_path = source_file_path
        self._latlon_file_path = latlon_file_path

    def _calc_latlon(self):
        source_dataset = Dataset(self._source_file_path, 'r')
        source_dataset.set_auto_scale(True)

        x = ma.getdata(source_dataset['x'][:])
        y = ma.getdata(source_dataset['y'][:])
        grid_x, grid_y = numpy.meshgrid(x, y, indexing='xy')

        goes_imager_projection = source_dataset.variables['goes_imager_projection']
        r_eq = goes_imager_projection.getncattr('semi_major_axis')
        r_pol = goes_imager_projection.getncattr('semi_minor_axis')
        h = goes_imager_projection.getncattr('perspective_point_height') + \
            goes_imager_projection.getncattr('semi_major_axis')
        lon_0 = goes_imager_projection.getncattr('longitude_of_projection_origin')
        lon_0 = (lon_0 * numpy.pi) / 180.0

        h_sqr = numpy.power(h, 2.0)
        r_eq_sqr = numpy.power(r_eq, 2.0)
        r_pol_sqr = numpy.power(r_pol, 2.0)

        sin_x = numpy.sin(grid_x)
        cos_x = numpy.cos(grid_x)
        sin_y = numpy.sin(grid_y)
        cos_y = numpy.cos(grid_y)

        sin_x_sqr = numpy.power(sin_x, 2.0)
        cos_x_sqr = numpy.power(cos_x, 2.0)
        sin_y_sqr = numpy.power(sin_y, 2.0)
        cos_y_sqr = numpy.power(cos_y, 2.0)

        a = sin_x_sqr + cos_x_sqr * (cos_y_sqr + (r_eq_sqr / r_pol_sqr) * sin_y_sqr)
        b = -2.0 * h * cos_x * cos_y
        c = h_sqr - r_eq_sqr

        arg = numpy.power(b, 2.0) - (4.0 * a * c)
        r_s = ((-1.0 * b) - numpy.sqrt(arg)) / (2.0 * a)

        s_x = r_s * cos_x * cos_y
        s_y = (-1.0 * r_s) * sin_x
        s_z = r_s * cos_x * sin_y
        h_minus_s_x = h - s_x

        s_y_sqr = numpy.power(s_y, 2.0)
        h_minus_s_x_sqr = numpy.power(h_minus_s_x, 2.0)

        lat = numpy.arctan((r_eq_sqr / r_pol_sqr) * (s_z / numpy.sqrt(h_minus_s_x_sqr + s_y_sqr)))
        lon = lon_0 - numpy.arctan(s_y / h_minus_s_x)

        lat = lat * 180.0 / numpy.pi
        lon = lon * 180.0 / numpy.pi

        lat = lat.reshape(len(lat) * len(lat))
        lon = lon.reshape(len(lon) * len(lon))

        lat = numpy.nan_to_num(lat, nan=-999)
        lon = numpy.nan_to_num(lon, nan=-999)

        yaw_flip_flag = source_dataset.variables['yaw_flip_flag'][0]
        if not yaw_flip_flag:
            lat = lat[::-1]
            lon = lon[::-1]
        source_dataset.close()
        return lat, lon

    def _get_nadir_attributes(self):
        source_dataset = Dataset(self._source_file_path, 'r')
        lat_nadir = source_dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lat_nadir')
        lon_nadir = source_dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lon_nadir')
        source_dataset.close()
        return lat_nadir, lon_nadir

    def create(self):
        lat, lon = self._calc_latlon()
        nlocs = len(lat)
        latlon_dataset = Dataset(self._latlon_file_path, 'w')
        latlon_dataset.createDimension('nlocs', nlocs)
        latlon_dataset.createVariable('nlocs', 'i4', ('nlocs',))
        latlon_dataset.variables['nlocs'][:] = numpy.arange(1, nlocs + 1, 1, dtype='int32')
        latlon_dataset.createGroup('MetaData')
        latlon_dataset.createVariable('/MetaData/latitude', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/longitude', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset['/MetaData/latitude'][:] = lat
        latlon_dataset['/MetaData/longitude'][:] = lon
        lat_nadir, lon_nadir = self._get_nadir_attributes()
        latlon_dataset['/MetaData/latitude'].setncattr('lat_nadir', lat_nadir)
        latlon_dataset['/MetaData/longitude'].setncattr('lon_nadir', lon_nadir)
        latlon_dataset.close()
