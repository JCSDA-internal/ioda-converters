#
# goes_converter.py
#
# This class generates two IODAv2 data files from a group of raw data files for all 16 channels of GOES-16 or GOES-17
# LB1 products. This class works with the Goes and GoesLatLon classes. The final result of this class is two IODAv2
# formatted data files - one for Brightness Temperature and one for Reflectance Factor (if include_rf is set to True).
# The following groups, variables, dimensions, and attributes are created using this class. Calculations within this
# program utilize "Calculating Zenith and Azimuth Angles for GridSat-B1"
# (https://www.ncdc.noaa.gov/gridsat/docs/Angle_Calculations.pdf) and code from CRTM's Zeeman_Utility
# (https://github.com/JCSDA-internal/crtm/blob/develop/src/Zeeman/Zeeman_Utility.f90).
#
# /GROUP/VARIABLE -> ATTRIBUTE
#
# / -> date_time
# / -> _ioda_layout
# / -> _ioda_layout_version
# /MetaData/dateTime
# /MetaData/latitude -> units
# /MetaData/longitude -> units
# /MetaData/elevation_angle -> units
# /MetaData/scan_angle -> units
# /MetaData/scan_position
# /MetaData/sensor_azimuth_angle -> units
# /MetaData/sensor_view_angle -> units
# /MetaData/sensor_zenith_angle -> units
# /MetaData/solar_azimuth_angle -> units
# /MetaData/solar_zenith_angle -> units
# /MetaData/sensor_channel
# /ObsError/albedo or /ObsError/brightnessTemperature
# /ObsValue/albedo or /ObsValue/brightnessTemperature
# /ObsError/brightnessTemperature -> units
# /ObsValue/brightnessTemperature -> units
# /PreQC/albedo or /PreQC/brightnessTemperature
# /PreQC/albedo -> flag_values or /PreQC/brightnessTemperature -> flag_values
# /PreQC/albedo -> flag_meanings or /PreQC/brightnessTemperature -> flag_meanings
# /Channel
# /Location
#

import os
import sys
import numpy as np
from netCDF4 import Dataset
from numpy import ma
import datetime
from goes import Goes
from goes_latlon import GoesLatLon
from goes_util import GoesUtil


class GoesConverter:

    def __init__(self, input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt, include_rf=False,
                 resolution=8):
        """
        Constructor
        input_file_paths - A list of the absolute paths to all 16 ABI channels from the same hour
        latlon_file_path - The path to an existing GoesLatLon file or if it does not exist the path to write the file
        output_file_path_rf - The path to write the IODAv2 reflectance factor data file
        output_file_path_bt - The path to write the IODAv2 brightness temperature data file
        include_rf - Boolean value indicating whether to create the reflectance factor output data file: False (default)
        resolution - The resolution in km: 8 (default), 4, 8, 16, 32, 64
        """
        self._input_file_paths = input_file_paths
        self._latlon_file_path = latlon_file_path
        self._output_file_path_rf = output_file_path_rf
        self._output_file_path_bt = output_file_path_bt
        self._resolution = resolution
        self._include_rf = include_rf
        self._latlon_dataset = None
        self._check_arguments()

        # TODO: make use of default fill values from NetCDF
        # self._float_missing_value = nc.default_fillvals['f4']
        # self._int_missing_value = nc.default_fillvals['i4']
        # self._long_missing_value = nc.default_fillvals['i8']

    def _check_arguments(self):
        """
        Checks the input arguments.
        """
        good_args = True
        if len(self._input_file_paths) != 16:
            print("ERROR: input_file_paths must contain 16 GOES-16 or GOES-17 data files. One for each ABI channel.")
            good_args = False
        good_resolutions = [2, 4, 8, 16, 32, 64]
        if int(self._resolution) not in good_resolutions:
            print("ERROR: resolution (in km) must be one of the following values: 2, 4, 8 (default), 16, 32, 64.")
            good_args = False
        if not good_args:
            sys.exit(2)

    def _initialize(self):
        """
        Create two local dictionaries contained the Goes class instances for reflectance factor (ABI channels 1-6)
        and brightness temperature (ABI channels 7-16). This function also assigns the file path for a template GOES file
        from ABI channel 7.
        """
        self._goes_util = GoesUtil()
        self._input_file_paths.sort()
        self._goes_dict_rf = {}
        self._goes_dict_bt = {}
        for input_file_path in self._input_file_paths:
            goes = Goes(input_file_path, self._goes_util)
            abi_channel = goes.get_abi_channel()
            if abi_channel < 7:
                self._goes_dict_rf[abi_channel] = goes
            else:
                self._goes_dict_bt[abi_channel] = goes
        self._platform_id = goes.get_platform_id()
        self._start_date = goes.get_start_date()
        self._input_file_path_template = self._goes_dict_bt[7].get_input_file_path()
        self._goes_util.set_yaw_flip_flag(goes.get_yaw_flip_flag())
        self._goes_util.set_resolution(self._resolution)

    def _check_nadir(self):
        """
        Returns a boolean variable indicating whether the nadir has changed by comparing the lat_nadir and lon_nadir
        attributes extracted from the GoesLatLon data file and the Goes template data file.
        """
        lat_nadir_latlon, lon_nadir_latlon = self._get_nadir_attribute_latlon()
        lat_nadir_template, lon_nadir_template = self._get_nadir_attribute_template()
        return lat_nadir_latlon == lat_nadir_template and lon_nadir_latlon == lon_nadir_template

    def _get_nadir_attribute_latlon(self):
        """
        Returns the lat and lon nadir attribute from the GoesLatLon data file.
        """
        dataset = Dataset(self._latlon_file_path, 'r')
        lat_nadir_latlon = dataset['MetaData'].variables['latitude'].getncattr('lat_nadir')
        lon_nadir_latlon = dataset['MetaData'].variables['longitude'].getncattr('lon_nadir')
        dataset.close()
        return lat_nadir_latlon, lon_nadir_latlon

    def _get_nadir_attribute_template(self):
        """
        Returns the lat and lon nadir attribute from the Goes template data file.
        """
        dataset = Dataset(self._input_file_path_template, 'r')
        lat_nadir_template = dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lat_nadir')
        lon_nadir_template = dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lon_nadir')
        dataset.close()
        return lat_nadir_template, lon_nadir_template

    def _check_latlon_file_path(self):
        """
        Returns a boolean variable indicating whether the GoesLatLon file exists.
        """
        return os.path.exists(self._latlon_file_path)

    def _create_latlon_dataset(self):
        """
        Creates a new GoesLatLon data file using the GoesLatLon class.
        """
        self._goes_lat_lon = GoesLatLon(self._input_file_path_template, self._latlon_file_path, self._goes_util)
        self._goes_lat_lon.create()

    def _create_metadata_latitude_variable(self, output_dataset):
        """
        Creates the /MetaData/latitude variable in an output netCDF4 dataset.
        output_dataset - A netCDF Dataset object
        """
        latitude_data_array = self._latlon_dataset['MetaData'].variables['latitude'][:].real
        output_dataset.createVariable('/MetaData/latitude', 'f4', 'Location', fill_value=-999)
        output_dataset['/MetaData/latitude'][:] = latitude_data_array
        output_dataset['/MetaData/latitude'].setncattr('units', 'degrees_north')

    def _create_metadata_longitude_variable(self, output_dataset):
        """
        Creates the /MetaData/longitude variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        longitude_data_array = self._latlon_dataset['MetaData'].variables['longitude'][:].real
        output_dataset.createVariable('/MetaData/longitude', 'f4', 'Location', fill_value=-999)
        output_dataset['/MetaData/longitude'][:] = longitude_data_array
        output_dataset['/MetaData/longitude'].setncattr('units', 'degrees_east')

    def _create_metadata_scan_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/scan_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        scan_angle_data_array = self._latlon_dataset['MetaData'].variables['scan_angle'][:].real
        output_dataset.createVariable('/MetaData/scan_angle', 'f4', 'Location', fill_value=-999)
        output_dataset['/MetaData/scan_angle'][:] = scan_angle_data_array
        output_dataset['/MetaData/scan_angle'].setncattr('units', 'degrees')

    def _create_metadata_elevation_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/elevation_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        elevation_angle_data_array = self._latlon_dataset['MetaData'].variables['elevation_angle'][:].real
        output_dataset.createVariable('/MetaData/elevation_angle', 'f4', 'Location', fill_value=-999)
        output_dataset['/MetaData/elevation_angle'][:] = elevation_angle_data_array
        output_dataset['/MetaData/elevation_angle'].setncattr('units', 'degrees')

    def _create_metadata_scan_position_variable(self, output_dataset):
        """
        Creates the /MetaData/scan_position variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        scan_position_data_array = self._latlon_dataset['MetaData'].variables['scan_position'][:].real
        output_dataset.createVariable('/MetaData/scan_position', 'f4', 'Location', fill_value=-999)
        output_dataset['/MetaData/scan_position'][:] = scan_position_data_array

    def _create_metadata_sensor_zenith_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/sensor_zenith_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        sensor_zenith_angle_data_array = self._latlon_dataset['MetaData'].variables['sensor_zenith_angle'][:].real
        output_dataset.createVariable('/MetaData/sensor_zenith_angle', 'f4', 'Location', fill_value=-999)
        output_dataset['/MetaData/sensor_zenith_angle'][:] = sensor_zenith_angle_data_array
        output_dataset['/MetaData/sensor_zenith_angle'].setncattr('units', 'degrees')

    def _create_metadata_sensor_azimuth_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/sensor_azimuth_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        sensor_azimuth_angle_data_array = self._latlon_dataset['MetaData'].variables['sensor_azimuth_angle'][:].real
        output_dataset.createVariable('/MetaData/sensor_azimuth_angle', 'f4', 'Location', fill_value=-999)
        output_dataset['/MetaData/sensor_azimuth_angle'][:] = sensor_azimuth_angle_data_array
        output_dataset['/MetaData/sensor_azimuth_angle'].setncattr('units', 'degrees')

    def _create_metadata_sensor_view_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/sensor_view_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        sensor_view_angle_data_array = self._latlon_dataset['MetaData'].variables['sensor_view_angle'][:].real
        output_dataset.createVariable('/MetaData/sensor_view_angle', 'f4', 'Location', fill_value=-999)
        output_dataset['/MetaData/sensor_view_angle'][:] = sensor_view_angle_data_array
        output_dataset['/MetaData/sensor_view_angle'].setncattr('units', 'degrees')

    def _create_metadata_solar_zenith_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/solar_zenith_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        dataset = Dataset(self._input_file_path_template, 'r')
        dataset.set_auto_scale(True)
        dataset_latlon = Dataset(self._latlon_file_path, 'r')
        latitude = ma.getdata(dataset_latlon['/MetaData/latitude'][:].real)
        longitude = ma.getdata(dataset_latlon['/MetaData/longitude'][:].real)
        latitude_rad = latitude * np.pi / 180.0
        start_date = self._start_date
        start_date_tt = start_date.timetuple()
        julian_day = start_date_tt.tm_yday
        day_angle = (360.0 / 365.0) * (julian_day - 1.0) * np.pi / 180.0
        comps_eqt = [0.001868 * np.cos(day_angle), 0.032077 * np.sin(day_angle), 0.014615 * np.cos(2.0 * day_angle),
                     0.040890 * np.sin(2.0 * day_angle), 229.18 / 60.0]
        equation_of_time = (0.000075 + (comps_eqt[0]) - (comps_eqt[1]) - (comps_eqt[2]) - (comps_eqt[3])) * comps_eqt[4]
        universal_time = start_date.hour + start_date.minute / 60.0 + start_date.second / 3600.0
        local_sun_time = universal_time + equation_of_time + longitude / (360.0 / 24.0)
        hour_angle = (360.0 / 24.0) * np.mod(local_sun_time + 12.0, 24.0)
        hour_angle_rad = hour_angle * np.pi / 180.0
        comps_dec = [0.399912 * np.cos(day_angle), 0.070257 * np.sin(day_angle), 0.006758 * np.cos(2.0 * day_angle),
                     0.000907 * np.sin(2.0 * day_angle), 0.002697 * np.cos(3.0 * day_angle),
                     0.001480 * np.sin(3.0 * day_angle)]
        declin = 0.006918 - comps_dec[0] + comps_dec[1] - comps_dec[2] + comps_dec[3] - comps_dec[4] + comps_dec[5]
        comps_za = [np.arccos(np.sin(latitude_rad) * np.sin(declin)),
                    np.cos(latitude_rad) * np.cos(declin) * np.cos(hour_angle_rad)]
        solar_zenith_angle_data_array = comps_za[0] + comps_za[1]
        solar_zenith_angle_data_array = \
            self._goes_util.filter_data_array_by_yaw_flip_flag(solar_zenith_angle_data_array)
        solar_zenith_angle_data_array = solar_zenith_angle_data_array * 180.0 / np.pi
        dataset.close()
        dataset_latlon.close()
        solar_zenith_angle_data_array = np.nan_to_num(solar_zenith_angle_data_array, nan=-999)
        output_dataset.createVariable('/MetaData/solar_zenith_angle', 'f4', 'Location', fill_value=-999)
        output_dataset['/MetaData/solar_zenith_angle'][:] = solar_zenith_angle_data_array
        output_dataset['/MetaData/solar_zenith_angle'].setncattr('units', 'degrees')

    def _create_metadata_solar_azimuth_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/solar_azimuth_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        dataset = Dataset(self._input_file_path_template, 'r')
        dataset.set_auto_scale(True)
        goes_imager_projection = dataset.variables['goes_imager_projection']
        lon_0 = goes_imager_projection.getncattr('longitude_of_projection_origin')
        lon_0_rad = lon_0 * np.pi / 180.0
        dataset_latlon = Dataset(self._latlon_file_path, 'r')
        latitude = ma.getdata(dataset_latlon['/MetaData/latitude'][:].real)
        longitude = ma.getdata(dataset_latlon['/MetaData/longitude'][:].real)
        latitude_rad = latitude * np.pi / 180.0
        longitude_rad = longitude * np.pi / 180.0
        start_date = self._start_date
        t = start_date.hour + (start_date.minute / 60.0) + (start_date.second / 3600.0)
        h = -1.0 * ((t - 12.0) / 12.0)
        beta_0 = np.arccos(np.cos(latitude_rad) * np.cos(longitude_rad - lon_0_rad))
        solar_azimuth_angle_data_array = np.arcsin((np.sin(h - longitude_rad)) / (np.sin(beta_0)))
        solar_azimuth_angle_data_array = \
            self._goes_util.filter_data_array_by_yaw_flip_flag(solar_azimuth_angle_data_array)
        solar_azimuth_angle_data_array = solar_azimuth_angle_data_array * 180.0 / np.pi
        dataset.close()
        dataset_latlon.close()
        solar_azimuth_angle_data_array = np.nan_to_num(solar_azimuth_angle_data_array, nan=-999)
        output_dataset.createVariable('/MetaData/solar_azimuth_angle', 'f4', 'Location', fill_value=-999)
        output_dataset['/MetaData/solar_azimuth_angle'][:] = solar_azimuth_angle_data_array
        output_dataset['/MetaData/solar_azimuth_angle'].setncattr('units', 'degrees')

    def _create_location_dimension(self, output_dataset):
        """
        Creates the Location dimension in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        Location = self._latlon_dataset.dimensions['Location'].size
        output_dataset.createDimension('Location', Location)
        output_dataset.createVariable('Location', 'i4', 'Location')
        output_dataset.variables['Location'].setncattr('suggested_chunk_dim', Location)
        output_dataset.variables['Location'][:] = np.arange(1, Location + 1, 1, dtype='int32')

    @staticmethod
    def _create_groups(output_dataset):
        """
        Creates the required groups in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        output_dataset.createGroup('MetaData')
        output_dataset.createGroup('ObsError')
        output_dataset.createGroup('ObsValue')
        output_dataset.createGroup('PreQC')

    @staticmethod
    def _create_channel_dimension(output_dataset, Channel):
        """
        Creates the Channel dimension in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        Channel - An integer indicating the number of Channel: 6 (for reflectance factor)
                                                          or 10 (for brightness temperature)
        """
        output_dataset.createDimension('Channel', Channel)
        output_dataset.createVariable('Channel', 'i4', 'Channel')
        if Channel == 6:
            output_dataset.variables['Channel'][:] = [1, 2, 3, 4, 5, 6]
        elif Channel == 10:
            output_dataset.variables['Channel'][:] = [7, 8, 9, 10, 11, 12, 13, 14, 15, 16]

    @staticmethod
    def _create_metadata_sensor_channel_variable(output_dataset):
        """
        Creates the /MetaData/sensor_channel variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        output_dataset.createVariable('/MetaData/sensor_channel', 'i4', 'Channel')
        output_dataset['/MetaData/sensor_channel'][:] = output_dataset['Channel'][:]

    @staticmethod
    def _create_root_group_attributes(output_dataset, resolution, platform_id):
        """
        Creates several root group attributes in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        output_dataset.setncattr('_ioda_layout', 'ObsGroup')
        output_dataset.setncattr('_ioda_layout_version', '0')
        output_dataset.setncattr('subsampled_resolution (km)', resolution)
        output_dataset.setncattr('platform_identifier', platform_id)

    @staticmethod
    def _get_Location(dataset):
        """
        Returns the Location dimension size for the provided netCDF4 Dataset.
        dataset - the dataset to extract the Location size
        """
        return dataset.dimensions['Location'].size

    def _create_preqc_reflectance_factor_variable(self, output_dataset):
        """
        Creates the /PreQC/albedo variable variable and associated attributes in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        temp_dict = {}
        counter = 0
        for key in self._goes_dict_rf.keys():
            goes = self._goes_dict_rf[key]
            temp_dict[counter] = ma.getdata(goes.get_preqc_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = np.column_stack((data_array, temp_dict[i]))
        output_dataset.createVariable('/PreQC/albedo', 'f4', ('Location', 'Channel'), fill_value=-999)
        output_dataset['/PreQC/albedo'][:] = data_array
        output_dataset['/PreQC/albedo'].setncattr('flag_values', '0,1,2,3')
        output_dataset['/PreQC/albedo'].setncattr('flag_meanings',
                                                              'good_pixel_qf,conditionally_usable_pixel_qf,'
                                                              'out_of_range_pixel_qf,no_value_pixel_qf')

    def _create_preqc_brightness_temperature_variable(self, output_dataset):
        """
        Creates the /PreQC/brightnessTemperature variable and associated attributes in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        temp_dict = {}
        counter = 0
        for key in self._goes_dict_bt.keys():
            goes = self._goes_dict_bt[key]
            temp_dict[counter] = ma.getdata(goes.get_preqc_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = np.column_stack((data_array, temp_dict[i]))
        output_dataset.createVariable('/PreQC/brightnessTemperature', 'f4', ('Location', 'Channel'),
                                      fill_value=-999)
        output_dataset['/PreQC/brightnessTemperature'][:] = data_array
        output_dataset['/PreQC/brightnessTemperature'].setncattr('flag_values', '0,1,2,3')
        output_dataset['/PreQC/brightnessTemperature'].setncattr('flag_meanings',
                                                                  'good_pixel_qf,conditionally_usable_pixel_qf,'
                                                                  'out_of_range_pixel_qf,no_value_pixel_qf')

    def _create_obsvalue_reflectance_factor_variable(self, output_dataset):
        """
        Creates the /ObsValue/albedo variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        temp_dict = {}
        counter = 0
        for key in self._goes_dict_rf.keys():
            goes = self._goes_dict_rf[key]
            temp_dict[counter] = ma.getdata(goes.get_obsvalue_rf_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = np.column_stack((data_array, temp_dict[i]))
        output_dataset.createVariable('/ObsValue/albedo', 'f4', ('Location', 'Channel'),
                                      fill_value=-999)
        output_dataset['/ObsValue/albedo'][:] = data_array

    def _create_obsvalue_brightness_temperature_variable(self, output_dataset):
        """
        Creates the /ObsValue/brightnessTemperature variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        temp_dict = {}
        counter = 0
        for key in self._goes_dict_bt.keys():
            goes = self._goes_dict_bt[key]
            temp_dict[counter] = ma.getdata(goes.get_obsvalue_bt_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = np.column_stack((data_array, temp_dict[i]))
        output_dataset.createVariable('/ObsValue/brightnessTemperature', 'f4', ('Location', 'Channel'),
                                      fill_value=-999)
        output_dataset['/ObsValue/brightnessTemperature'][:] = data_array
        output_dataset['/ObsValue/brightnessTemperature'].setncattr('units', 'K')

    def _create_obserror_reflectance_factor_variable(self, output_dataset):
        """
        Creates the /ObsError/albedo variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        temp_dict = {}
        counter = 0
        for key in self._goes_dict_rf.keys():
            goes = self._goes_dict_rf[key]
            temp_dict[counter] = ma.getdata(goes.get_obserror_rf_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = np.column_stack((data_array, temp_dict[i]))
        output_dataset.createVariable('/ObsError/albedo', 'f4', ('Location', 'Channel'),
                                      fill_value=-999)
        output_dataset['/ObsError/albedo'][:] = data_array

    def _create_obserror_brightness_temperature_variable(self, output_dataset):
        """
        Creates the /ObsError/brightnessTemperature variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        temp_dict = {}
        counter = 0
        for key in self._goes_dict_bt.keys():
            goes = self._goes_dict_bt[key]
            temp_dict[counter] = ma.getdata(goes.get_obserror_bt_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = np.column_stack((data_array, temp_dict[i]))
        output_dataset.createVariable('/ObsError/brightnessTemperature', 'f4', ('Location', 'Channel'),
                                      fill_value=-999)
        output_dataset['/ObsError/brightnessTemperature'][:] = data_array
        output_dataset['/ObsError/brightnessTemperature'].setncattr('units', 'K')

    def _create_metadata_time_variable(self, output_dataset):
        """
        Creates the /MetaData/dateTime variable and /date_time attribute in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        iso8601_string = 'seconds since 1970-01-01T00:00:00Z'
        epoch = datetime.datetime.fromisoformat(iso8601_string[14:-1])
        time_offset = round((self._start_date - epoch).total_seconds())   # seconds since epoch.
        start_date = self._start_date.strftime('%Y-%m-%dT%H:%M:%SZ')
        output_dataset.setncattr("date_time", start_date)
        datetime_array = np.full(self._get_Location(output_dataset), np.int64(time_offset))

        output_dataset.createVariable('/MetaData/dateTime', 'i8', 'Location')
        output_dataset['/MetaData/dateTime'][:] = datetime_array
        output_dataset['/MetaData/dateTime'].setncattr('units', iso8601_string)

    def _load_all_goes(self):
        """
        Calls the Goes load method on all input data files.
        """
        for key in self._goes_dict_bt.keys():
            self._goes_dict_bt[key].load()
        if self._include_rf:
            for key in self._goes_dict_rf.keys():
                self._goes_dict_rf[key].load()

    def convert(self):
        """
        Creates the reflectance factor (if include_rf is True) and brightness temperature IODAv2 data files.
        This functions also checks for the existence and nadir change of the GoesLatLon data file.
        """
        self._initialize()
        if self._check_latlon_file_path():
            if not self._check_nadir():
                self._create_latlon_dataset()
        else:
            self._create_latlon_dataset()
        self._latlon_dataset = Dataset(self._latlon_file_path, 'r')
        nonexistent_indices_data_array = ma.getdata(self._latlon_dataset.variables['nonexistent_indices']).real
        self._goes_util.set_nonexistent_indices_data_array(nonexistent_indices_data_array)
        self._load_all_goes()
        self._convert_bt()
        if self._include_rf:
            self._convert_rf()
        self._latlon_dataset.close()

    def _convert_bt(self):
        """
        Creates the brightness temperature IODAv2 data file.
        """
        dataset = Dataset(self._output_file_path_bt, 'w')
        GoesConverter._create_groups(dataset)
        GoesConverter._create_root_group_attributes(dataset, self._resolution, self._platform_id)
        GoesConverter._create_channel_dimension(dataset, 10)
        self._create_location_dimension(dataset)
        GoesConverter._create_metadata_sensor_channel_variable(dataset)
        self._create_metadata_latitude_variable(dataset)
        self._create_metadata_longitude_variable(dataset)
        self._create_metadata_elevation_angle_variable(dataset)
        self._create_metadata_scan_angle_variable(dataset)
        self._create_metadata_scan_position_variable(dataset)
        self._create_metadata_sensor_azimuth_angle_variable(dataset)
        self._create_metadata_sensor_view_angle_variable(dataset)
        self._create_metadata_sensor_zenith_angle_variable(dataset)
        self._create_metadata_solar_zenith_angle_variable(dataset)
        self._create_metadata_solar_azimuth_angle_variable(dataset)
        self._create_metadata_time_variable(dataset)
        self._create_obsvalue_brightness_temperature_variable(dataset)
        self._create_obserror_brightness_temperature_variable(dataset)
        self._create_preqc_brightness_temperature_variable(dataset)
        dataset.close()

    def _convert_rf(self):
        """
        Creates the reflectance factor IODAv2 data file.
        """
        dataset = Dataset(self._output_file_path_rf, 'w')
        GoesConverter._create_groups(dataset)
        GoesConverter._create_root_group_attributes(dataset, self._resolution, self._platform_id)
        GoesConverter._create_channel_dimension(dataset, 6)
        self._create_location_dimension(dataset)
        GoesConverter._create_metadata_sensor_channel_variable(dataset)
        self._create_metadata_latitude_variable(dataset)
        self._create_metadata_longitude_variable(dataset)
        self._create_metadata_elevation_angle_variable(dataset)
        self._create_metadata_scan_angle_variable(dataset)
        self._create_metadata_scan_position_variable(dataset)
        self._create_metadata_sensor_azimuth_angle_variable(dataset)
        self._create_metadata_sensor_view_angle_variable(dataset)
        self._create_metadata_sensor_zenith_angle_variable(dataset)
        self._create_metadata_solar_zenith_angle_variable(dataset)
        self._create_metadata_solar_azimuth_angle_variable(dataset)
        self._create_metadata_time_variable(dataset)
        self._create_obsvalue_reflectance_factor_variable(dataset)
        self._create_obserror_reflectance_factor_variable(dataset)
        self._create_preqc_reflectance_factor_variable(dataset)
        dataset.close()
