#
# goes_latlon.py
#
# This class generates a single IODAv2 data file with the following groups, variables, attributes and
# dimensions. This program is designed to be executed from the GoesConverter class is the event that the
# nadir for GOES-16 or GOES-17 has changed. Once created, the GoesConverter class will consume this file for each
# conversion. The argument source_file_path must be a GOES-16 or GOES-17 file with 2km resolution. Calculations within
# this program utilize section 5.1.2.8.1 of the GOES R SERIES PRODUCT DEFINITION AND USERS' GUIDE Dec 17, 2019
# REVISION 2.2 416-R-PUG-L1B-0347 Vol 3
#
# /GROUP/VARIABLE -> ATTRIBUTE
#
# /MetaData/elevation_angle
# /MetaData/scan_angle
# /MetaData/latitude
# /MetaData/latitude -> lat_nadir
# /MetaData/longitude
# /MetaData/longitude -> lon_nadir
# /nlocs
#
from netCDF4 import Dataset
from numpy import ma
import numpy as np


class GoesLatLon:

    def __init__(self, source_file_path, latlon_file_path):
        """
        Constructor
        source_file_path - a GOES-16 or GOES-17 raw data file with 2km resolution
        latlon_file_path - The path to the resulting IODAv2 formatted data file
        """
        self._source_file_path = source_file_path
        self._latlon_file_path = latlon_file_path

    def _calc_latlon(self):
        """
        Calculates the latitude and longitude from source_file_path Dataset, reshapes the latitude and longitude
        data arrays to a single dimension, and returns a tuple containing these data arrays.
        """
        source_dataset = Dataset(self._source_file_path, 'r')
        source_dataset.set_auto_scale(True)
        x = ma.getdata(source_dataset['x'][:])
        y = ma.getdata(source_dataset['y'][:])
        grid_x, grid_y = np.meshgrid(x, y, indexing='xy')
        goes_imager_projection = source_dataset.variables['goes_imager_projection']
        r_eq = goes_imager_projection.getncattr('semi_major_axis')
        r_pol = goes_imager_projection.getncattr('semi_minor_axis')
        h = goes_imager_projection.getncattr('perspective_point_height') + \
            goes_imager_projection.getncattr('semi_major_axis')
        lon_0 = goes_imager_projection.getncattr('longitude_of_projection_origin')
        lon_0 = (lon_0 * np.pi) / 180.0
        h_sqr = np.power(h, 2.0)
        r_eq_sqr = np.power(r_eq, 2.0)
        r_pol_sqr = np.power(r_pol, 2.0)
        sin_x = np.sin(grid_x)
        cos_x = np.cos(grid_x)
        sin_y = np.sin(grid_y)
        cos_y = np.cos(grid_y)
        sin_x_sqr = np.power(sin_x, 2.0)
        cos_x_sqr = np.power(cos_x, 2.0)
        sin_y_sqr = np.power(sin_y, 2.0)
        cos_y_sqr = np.power(cos_y, 2.0)
        a = sin_x_sqr + cos_x_sqr * (cos_y_sqr + (r_eq_sqr / r_pol_sqr) * sin_y_sqr)
        b = -2.0 * h * cos_x * cos_y
        c = h_sqr - r_eq_sqr
        arg = np.power(b, 2.0) - (4.0 * a * c)
        r_s = ((-1.0 * b) - np.sqrt(arg)) / (2.0 * a)
        s_x = r_s * cos_x * cos_y
        s_y = (-1.0 * r_s) * sin_x
        s_z = r_s * cos_x * sin_y
        h_minus_s_x = h - s_x
        s_y_sqr = np.power(s_y, 2.0)
        h_minus_s_x_sqr = np.power(h_minus_s_x, 2.0)
        lat = np.arctan((r_eq_sqr / r_pol_sqr) * (s_z / np.sqrt(h_minus_s_x_sqr + s_y_sqr)))
        lon = lon_0 - np.arctan(s_y / h_minus_s_x)
        lat = lat * 180.0 / np.pi
        lon = lon * 180.0 / np.pi
        lat = lat.reshape(len(lat) * len(lat))
        lon = lon.reshape(len(lon) * len(lon))
        lat = np.nan_to_num(lat, nan=-999)
        lon = np.nan_to_num(lon, nan=-999)
        yaw_flip_flag = source_dataset.variables['yaw_flip_flag'][0]
        if not yaw_flip_flag:
            lat = lat[::-1]
            lon = lon[::-1]
        source_dataset.close()
        return lat, lon

    def _get_nadir_attributes(self):
        """
        Returns the latitude and longitude nadir attributes from the source_file_path Dataset.
        """
        source_dataset = Dataset(self._source_file_path, 'r')
        lat_nadir = source_dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lat_nadir')
        lon_nadir = source_dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lon_nadir')
        source_dataset.close()
        return lat_nadir, lon_nadir

    def _calc_angles(self):
        """
        Calculates the scan and elevation angles from source_file_path Dataset, reshapes the scan and elevation angle
        data arrays to a single dimension, and returns a tuple containing these data arrays.
        """
        source_dataset = Dataset(self._source_file_path, 'r')
        source_dataset.set_auto_scale(True)
        x = ma.getdata(source_dataset['x'][:])
        y = ma.getdata(source_dataset['y'][:])
        grid_x, grid_y = np.meshgrid(x, y, indexing='xy')
        scan_angle = grid_x * 180.0 / np.pi
        elevation_angle = grid_y * 180.0 / np.pi
        scan_angle = scan_angle.reshape(len(scan_angle) * len(scan_angle))
        elevation_angle = elevation_angle.reshape(len(elevation_angle) * len(elevation_angle))
        yaw_flip_flag = source_dataset.variables['yaw_flip_flag'][0]
        if not yaw_flip_flag:
            scan_angle = scan_angle[::-1]
            elevation_angle = elevation_angle[::-1]
        source_dataset.close()
        return scan_angle, elevation_angle

    def create(self):
        """
        Generates an IODAv2 formatted data file containing the groups, variables, attributes and dimensions listed in
        the class header documentation.
        """
        latitude, longitude = self._calc_latlon()
        scan_angle, elevation_angle = self._calc_angles()
        nlocs = len(latitude)
        latlon_dataset = Dataset(self._latlon_file_path, 'w')
        latlon_dataset.createDimension('nlocs', nlocs)
        latlon_dataset.createVariable('nlocs', 'i4', ('nlocs',))
        latlon_dataset.variables['nlocs'][:] = np.arange(1, nlocs + 1, 1, dtype='int32')
        latlon_dataset.createGroup('MetaData')
        latlon_dataset.createVariable('/MetaData/latitude', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/longitude', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/scan_angle', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/elevation_angle', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset['/MetaData/latitude'][:] = latitude
        latlon_dataset['/MetaData/longitude'][:] = longitude
        latlon_dataset['/MetaData/scan_angle'][:] = scan_angle
        latlon_dataset['/MetaData/elevation_angle'][:] = elevation_angle
        lat_nadir, lon_nadir = self._get_nadir_attributes()
        latlon_dataset['/MetaData/latitude'].setncattr('lat_nadir', lat_nadir)
        latlon_dataset['/MetaData/longitude'].setncattr('lon_nadir', lon_nadir)
        latlon_dataset.close()
