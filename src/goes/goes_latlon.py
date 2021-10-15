#
# goes_latlon.py
#
# This class generates a single IODAv2 data file with the following groups, variables, attributes and
# dimensions. This program is designed to be executed from the GoesConverter class is the event that the
# nadir for GOES-16 or GOES-17 has changed. Once created, the GoesConverter class will consume this file for each
# conversion. The argument source_file_path must be a GOES-16 or GOES-17 file with 2km resolution. Calculations within
# this program utilize section 5.1.2.8.1 of the "GOES R SERIES PRODUCT DEFINITION AND USERS' GUIDE" Dec 17, 2019
# REVISION 2.2 416-R-PUG-L1B-0347 Vol 3 (https://www.goes-r.gov/users/docs/PUG-L1b-vol3.pdf), "GOES-R Semi-Static
# Data README" (https://satepsanone.nesdis.noaa.gov/pub/GASP/documentations/GOESR/GOES-R_SemiStatic_Data_README2.docx),
# and "Calculating Zenith and Azimuth Angles for GridSat-B1"
# (https://www.ncdc.noaa.gov/gridsat/docs/Angle_Calculations.pdf).
#
# /GROUP/VARIABLE -> ATTRIBUTE
#
# /MetaData/elevation_angle
# /MetaData/scan_angle
# /MetaData/scan_position
# /MetaData/sensor_azimuth_angle -> units
# /MetaData/sensor_view_angle -> units
# /MetaData/sensor_zenith_angle -> units
# /MetaData/latitude
# /MetaData/latitude -> lat_nadir
# /MetaData/longitude
# /MetaData/longitude -> lon_nadir
# /nlocs
#
from netCDF4 import Dataset
from numpy import ma
import numpy as np
from goes_util import GoesUtil


class GoesLatLon:

    def __init__(self, source_file_path, latlon_file_path, goes_util: GoesUtil):
        """
        Constructor
        source_file_path - a GOES-16 or GOES-17 raw data file with 2km resolution
        latlon_file_path - the path to the resulting IODAv2 formatted data file
        goes_util - an instance of the GoesUtil class
        """
        self._source_file_path = source_file_path
        self._latlon_file_path = latlon_file_path
        self._goes_util = goes_util
        self._source_dataset = Dataset(self._source_file_path, 'r')
        self._source_dataset.set_auto_scale(True)
        self._lat_fill_value_index_array = None

    def _calc_latlon(self):
        """
        Calculates the latitude and longitude from source_file_path Dataset, reshapes the latitude and longitude
        data arrays to a single dimension, and returns a tuple containing these data arrays.
        """
        self._x = ma.getdata(self._source_dataset['x'][:]).real
        self._y = ma.getdata(self._source_dataset['y'][:]).real
        self._x = self._goes_util.subsample_1d(ma.getdata(self._source_dataset['x'][:]))
        self._y = self._goes_util.subsample_1d(ma.getdata(self._source_dataset['y'][:]))
        grid_x, grid_y = np.meshgrid(self._x, self._y, indexing='xy')
        goes_imager_projection = self._source_dataset.variables['goes_imager_projection']
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
        a = sin_x_sqr + (cos_x_sqr * (cos_y_sqr + ((r_eq_sqr / r_pol_sqr) * sin_y_sqr)))
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
        lat = self._goes_util.filter_data_array_by_yaw_flip_flag(lat)
        lon = self._goes_util.filter_data_array_by_yaw_flip_flag(lon)
        lat = np.nan_to_num(lat, nan=-999)
        lon = np.nan_to_num(lon, nan=-999)
        self._lat_fill_value_index_array = np.where(lat == -999)
        self._lon_fill_value_index_array = np.where(lon == -999)
        lat = np.delete(lat, self._lat_fill_value_index_array)
        lon = np.delete(lon, self._lon_fill_value_index_array)
        lon = np.where(lon <= -180.0, lon + 360.0, lon)
        return lat, lon

    def _get_nadir_attributes(self):
        """
        Returns the latitude and longitude nadir attributes from the source_file_path Dataset.
        """
        lat_nadir = self._source_dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lat_nadir')
        lon_nadir = self._source_dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lon_nadir')
        return lat_nadir, lon_nadir

    def _calc_scan_elevation_angles(self):
        """
        Calculates the scan and elevation angles from source_file_path Dataset, reshapes the scan and elevation angle
        data arrays to a single dimension, and returns a tuple containing these data arrays.
        """
        grid_x, grid_y = np.meshgrid(self._x, self._y, indexing='xy')
        scan_angle = grid_x * 180.0 / np.pi
        elevation_angle = grid_y * 180.0 / np.pi
        scan_angle = scan_angle.reshape(len(scan_angle) * len(scan_angle))
        elevation_angle = elevation_angle.reshape(len(elevation_angle) * len(elevation_angle))
        scan_angle = self._goes_util.filter_data_array_by_yaw_flip_flag(scan_angle)
        elevation_angle = self._goes_util.filter_data_array_by_yaw_flip_flag(elevation_angle)
        scan_angle = self._filter_by_fill_value(scan_angle)
        elevation_angle = self._filter_by_fill_value(elevation_angle)
        return scan_angle, elevation_angle

    @staticmethod
    def _calc_scan_position(latitude):
        """
        Calculates the scan position.
        latitude - the latitude data array
        """
        scan_position = np.full(latitude.shape, 1, dtype='int32')
        return scan_position

    def _calc_sensor_zenith_azimuth_view_angles(self, latitude, longitude):
        """
        Calculates the sensor zenith, azimuth, and view angles. These angles are limited to values between -80 and 80
        degrees due to a constraint in CRTM.
        latitude - the latitude data array
        longitude - the longitude data array
        """
        goes_imager_projection = self._source_dataset.variables['goes_imager_projection']
        r_eq = goes_imager_projection.getncattr('semi_major_axis')
        h = goes_imager_projection.getncattr('perspective_point_height') + \
            goes_imager_projection.getncattr('semi_major_axis')
        lat_0 = goes_imager_projection.getncattr('latitude_of_projection_origin')
        lon_0 = goes_imager_projection.getncattr('longitude_of_projection_origin')
        lat_0_rad = lat_0 * np.pi / 180.0
        lon_0_rad = lon_0 * np.pi / 180.0
        latitude_rad = latitude * np.pi / 180.0
        longitude_rad = longitude * np.pi / 180.0
        h_sqr = np.power(h, 2.0)
        r_eq_sqr = np.power(r_eq, 2.0)
        beta = np.arccos(np.cos(latitude_rad - lat_0_rad) * np.cos(longitude_rad - lon_0_rad))
        sqrt_comp = h_sqr + r_eq_sqr - (2.0 * h * r_eq * np.cos(beta))
        sensor_zenith_angle = np.arcsin((h * np.sin(beta)) / np.sqrt(sqrt_comp)) * 180.0 / np.pi
        sensor_azimuth_angle = np.arcsin(np.sin(lon_0_rad - longitude_rad) / np.sin(beta)) * 180.0 / np.pi
        sensor_view_angle = sensor_zenith_angle
        sensor_zenith_angle = self._goes_util.filter_data_array_by_yaw_flip_flag(sensor_zenith_angle)
        sensor_azimuth_angle = self._goes_util.filter_data_array_by_yaw_flip_flag(sensor_azimuth_angle)
        sensor_view_angle = self._goes_util.filter_data_array_by_yaw_flip_flag(sensor_view_angle)
        sensor_zenith_angle = np.nan_to_num(sensor_zenith_angle, nan=-999)
        sensor_azimuth_angle = np.nan_to_num(sensor_azimuth_angle, nan=-999)
        sensor_view_angle = np.nan_to_num(sensor_view_angle, nan=-999)
        sensor_zenith_angle = np.where(sensor_zenith_angle > 80, 80, sensor_zenith_angle)
        sensor_zenith_angle = np.where(sensor_zenith_angle < -80, -80, sensor_zenith_angle)
        sensor_azimuth_angle = np.where(sensor_azimuth_angle > 360, 360, sensor_azimuth_angle)
        sensor_azimuth_angle = np.where(sensor_azimuth_angle < 0, 0, sensor_azimuth_angle)
        sensor_view_angle = np.where(sensor_view_angle > 80, 80, sensor_view_angle)
        sensor_view_angle = np.where(sensor_view_angle < -80, -80, sensor_view_angle)
        return sensor_zenith_angle, sensor_azimuth_angle, sensor_view_angle

    def _filter_by_fill_value(self, data_array):
        """
        Returns a data array filtered by latitude.
        data_array - a one dimensional data array
        """
        return np.delete(data_array, self._lat_fill_value_index_array)

    def create(self):
        """
        Generates an IODAv2 formatted data file containing the groups, variables, attributes and dimensions listed in
        the class header documentation.
        """
        latitude, longitude = self._calc_latlon()
        scan_angle, elevation_angle = self._calc_scan_elevation_angles()
        scan_position = GoesLatLon._calc_scan_position(latitude)
        sensor_zenith_angle, sensor_azimuth_angle, sensor_view_angle = \
            self._calc_sensor_zenith_azimuth_view_angles(latitude, longitude)
        latlon_dataset = Dataset(self._latlon_file_path, 'w')
        nlocs = len(latitude)
        latlon_dataset.createDimension('nlocs', nlocs)
        latlon_dataset.createVariable('nlocs', 'i4', ('nlocs',))
        latlon_dataset.variables['nlocs'][:] = np.arange(1, nlocs + 1, 1, dtype='int32')
        nonexistent_indices = len(self._lat_fill_value_index_array[0])
        latlon_dataset.createDimension('nonexistent_indices', nonexistent_indices)
        latlon_dataset.createVariable('nonexistent_indices', 'i4', ('nonexistent_indices',))
        latlon_dataset.variables['nonexistent_indices'][:] = np.array(self._lat_fill_value_index_array[0])
        latlon_dataset.createGroup('MetaData')
        latlon_dataset.createVariable('/MetaData/latitude', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/longitude', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/scan_angle', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/scan_position', 'i4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/elevation_angle', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/sensor_zenith_angle', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/sensor_azimuth_angle', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset.createVariable('/MetaData/sensor_view_angle', 'f4', 'nlocs', fill_value=-999)
        latlon_dataset['/MetaData/latitude'][:] = latitude
        latlon_dataset['/MetaData/longitude'][:] = longitude
        latlon_dataset['/MetaData/scan_angle'][:] = scan_angle
        latlon_dataset['/MetaData/scan_position'][:] = scan_position
        latlon_dataset['/MetaData/elevation_angle'][:] = elevation_angle
        latlon_dataset['/MetaData/sensor_zenith_angle'][:] = sensor_zenith_angle
        latlon_dataset['/MetaData/sensor_azimuth_angle'][:] = sensor_azimuth_angle
        latlon_dataset['/MetaData/sensor_view_angle'][:] = sensor_view_angle
        lat_nadir, lon_nadir = self._get_nadir_attributes()
        latlon_dataset['/MetaData/latitude'].setncattr('lat_nadir', lat_nadir)
        latlon_dataset['/MetaData/longitude'].setncattr('lon_nadir', lon_nadir)
        latlon_dataset.close()
        self._source_dataset.close()
