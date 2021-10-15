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
# /MetaData/datetime
# /MetaData/elevation_angle
# /MetaData/latitude
# /MetaData/longitude
# /MetaData/scan_angle
# /MetaData/scan_position
# /MetaData/sensor_azimuth_angle -> units
# /MetaData/sensor_view_angle -> units
# /MetaData/sensor_zenith_angle -> units
# /MetaData/solar_azimuth_angle -> units
# /MetaData/solar_zenith_angle -> units
# /MetaData/time
# /ObsError/reflectance_factor or /ObsError/brightness_temperature
# /ObsValue/reflectance_factor or /ObsValue/brightness_temperature
# /ObsError/brightness_temperature -> units
# /ObsValue/brightness_temperature -> units
# /PreQC/reflectance_factor or /PreQC/brightness_temperature
# /PreQC/reflectance_factor -> flag_values or /PreQC/brightness_temperature -> flag_values
# /PreQC/reflectance_factor -> flag_meanings or /PreQC/brightness_temperature -> flag_meanings
# /VarMetaData/sensor_channel
# /VarMetaData/variable_names
# /nchans
# /ndatetime
# /nlocs
# /nstring
# /nvars
#

import os
import sys
import numpy as np
from netCDF4 import Dataset
from numpy import ma
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

    def _check_arguments(self):
        """
        Checks the input arguments.
        """
        good_args = True
        if len(self._input_file_paths) != 16:
            print("ERROR: input_file_paths must contain 16 GOES-16 or GOES-17 data files. One for each ABI channel.")
            good_args = False
        good_resolutions = [2, 4, 8, 16, 32, 64]
        if self._resolution not in good_resolutions:
            print("ERROR: resolution (in km) must be one of the following values: 2 (default), 4, 8, 16, 32, 64.")
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
            abi_channel = int(goes.get_abi_channel())
            if abi_channel < 7:
                self._goes_dict_rf[abi_channel] = goes
            else:
                self._goes_dict_bt[abi_channel] = goes
        self._day_of_year = self._goes_dict_bt[7].get_day_of_year()
        self._start_date = self._goes_dict_bt[7].get_start_date()
        self._input_file_path_template = self._goes_dict_bt[7].get_input_file_path()
        self._goes_util.set_yaw_flip_flag(self._get_yaw_flip_flag())
        self._goes_util.set_resolution(self._resolution)

    def _get_yaw_flip_flag(self):
        """
        Returns the yaw_flip_flag from channel 1
        """
        dataset = Dataset(self._input_file_paths[0], 'r')
        yaw_flip_flag = dataset.variables['yaw_flip_flag'][0]
        dataset.close()
        return yaw_flip_flag

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
        output_dataset.createVariable('/MetaData/latitude', 'f4', 'nlocs', fill_value=-999)
        output_dataset['/MetaData/latitude'][:] = latitude_data_array

    def _create_metadata_longitude_variable(self, output_dataset):
        """
        Creates the /MetaData/longitude variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        longitude_data_array = self._latlon_dataset['MetaData'].variables['longitude'][:].real
        output_dataset.createVariable('/MetaData/longitude', 'f4', 'nlocs', fill_value=-999)
        output_dataset['/MetaData/longitude'][:] = longitude_data_array

    def _create_metadata_scan_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/scan_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        scan_angle_data_array = self._latlon_dataset['MetaData'].variables['scan_angle'][:].real
        output_dataset.createVariable('/MetaData/scan_angle', 'f4', 'nlocs', fill_value=-999)
        output_dataset['/MetaData/scan_angle'][:] = scan_angle_data_array

    def _create_metadata_elevation_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/elevation_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        elevation_angle_data_array = self._latlon_dataset['MetaData'].variables['elevation_angle'][:].real
        output_dataset.createVariable('/MetaData/elevation_angle', 'f4', 'nlocs', fill_value=-999)
        output_dataset['/MetaData/elevation_angle'][:] = elevation_angle_data_array

    def _create_metadata_scan_position_variable(self, output_dataset):
        """
        Creates the /MetaData/scan_position variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        scan_position_data_array = self._latlon_dataset['MetaData'].variables['scan_position'][:].real
        output_dataset.createVariable('/MetaData/scan_position', 'f4', 'nlocs', fill_value=-999)
        output_dataset['/MetaData/scan_position'][:] = scan_position_data_array

    def _create_metadata_sensor_zenith_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/sensor_zenith_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        sensor_zenith_angle_data_array = self._latlon_dataset['MetaData'].variables['sensor_zenith_angle'][:].real
        output_dataset.createVariable('/MetaData/sensor_zenith_angle', 'f4', 'nlocs', fill_value=-999)
        output_dataset['/MetaData/sensor_zenith_angle'][:] = sensor_zenith_angle_data_array
        output_dataset['/MetaData/sensor_zenith_angle'].setncattr('units', 'degrees')

    def _create_metadata_sensor_azimuth_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/sensor_azimuth_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        sensor_azimuth_angle_data_array = self._latlon_dataset['MetaData'].variables['sensor_azimuth_angle'][:].real
        output_dataset.createVariable('/MetaData/sensor_azimuth_angle', 'f4', 'nlocs', fill_value=-999)
        output_dataset['/MetaData/sensor_azimuth_angle'][:] = sensor_azimuth_angle_data_array
        output_dataset['/MetaData/sensor_azimuth_angle'].setncattr('units', 'degrees')

    def _create_metadata_sensor_view_angle_variable(self, output_dataset):
        """
        Creates the /MetaData/sensor_view_angle variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        sensor_view_angle_data_array = self._latlon_dataset['MetaData'].variables['sensor_view_angle'][:].real
        output_dataset.createVariable('/MetaData/sensor_view_angle', 'f4', 'nlocs', fill_value=-999)
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
        comps_eqt = []
        comps_eqt[0] = 0.001868 * np.cos(day_angle)
        comps_eqt[1] = 0.032077 * np.sin(day_angle)
        comps_eqt[2] = 0.014615 * np.cos(2.0 * day_angle)
        comps_eqt[3] = 0.040890 * np.sin(2.0 * day_angle)
        comps_eqt[4] = 229.18 / 60.0
        equation_of_time = (0.000075 + (comps_eqt[0]) - (comps_eqt[1]) - (comps_eqt[2]) - (comps_eqt[3])) * comps_eqt[4]
        universal_time = start_date.hour + start_date.minute / 60.0 + start_date.second / 3600.0
        local_sun_time = universal_time + equation_of_time + longitude / (360.0 / 24.0)
        hour_angle = (360.0 / 24.0) * np.mod(local_sun_time + 12.0, 24.0)
        hour_angle_rad = hour_angle * np.pi / 180.0
        comps_dec = []
        comps_dec[0] = 0.399912 * np.cos(day_angle)
        comps_dec[1] = 0.070257 * np.sin(day_angle)
        comps_dec[2] = 0.006758 * np.cos(2.0 * day_angle)
        comps_dec[3] = 0.000907 * np.sin(2.0 * day_angle)
        comps_dec[4] = 0.002697 * np.cos(3.0 * day_angle)
        comps_dec[5] = 0.001480 * np.sin(3.0 * day_angle)
        declin = 0.006918 - comps_dec[0] + comps_dec[1] - comps_dec[2] + comps_dec[3] - comps_dec[4] + comps_dec[5]
        comps_za = []
        comps_za[0] = np.arccos(np.sin(latitude_rad) * np.sin(declin))
        comps_za[1] = np.cos(latitude_rad) * np.cos(declin) * np.cos(hour_angle_rad)
        solar_zenith_angle_data_array = comps_za[0] + comps_za[1]
        solar_zenith_angle_data_array = \
            self._goes_util.filter_data_array_by_yaw_flip_flag(solar_zenith_angle_data_array)
        solar_zenith_angle_data_array = solar_zenith_angle_data_array * 180.0 / np.pi
        dataset.close()
        dataset_latlon.close()
        solar_zenith_angle_data_array = np.nan_to_num(solar_zenith_angle_data_array, nan=-999)
        output_dataset.createVariable('/MetaData/solar_zenith_angle', 'f4', 'nlocs', fill_value=-999)
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
        output_dataset.createVariable('/MetaData/solar_azimuth_angle', 'f4', 'nlocs', fill_value=-999)
        output_dataset['/MetaData/solar_azimuth_angle'][:] = solar_azimuth_angle_data_array
        output_dataset['/MetaData/solar_azimuth_angle'].setncattr('units', 'degrees')

    def _create_nlocs_dimension(self, output_dataset):
        """
        Creates the nlocs dimension in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        nlocs = self._latlon_dataset.dimensions['nlocs'].size
        output_dataset.createDimension('nlocs', nlocs)
        output_dataset.createVariable('nlocs', 'i4', 'nlocs')
        output_dataset.variables['nlocs'].setncattr('suggested_chunk_dim', nlocs)
        output_dataset.variables['nlocs'][:] = np.arange(1, nlocs + 1, 1, dtype='int32')

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
        output_dataset.createGroup('VarMetaData')

    @staticmethod
    def _create_nchans_dimension(output_dataset, nchans):
        """
        Creates the nchans dimension in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        nchans - An integer indicating the number of nchans: 6 (for reflectance factor)
                                                          or 10 (for brightness temperature)
        """
        output_dataset.createDimension('nchans', nchans)
        output_dataset.createVariable('nchans', 'i4', 'nchans')
        if nchans == 6:
            output_dataset.variables['nchans'][:] = [1, 2, 3, 4, 5, 6]
        elif nchans == 10:
            output_dataset.variables['nchans'][:] = [7, 8, 9, 10, 11, 12, 13, 14, 15, 16]

    @staticmethod
    def _create_nvars_dimension(output_dataset, nvars):
        """
        Creates the nvars dimension in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        nvars - An integer indicating the number of nvars: 6 (for reflectance factor)
                                                        or 10 (for brightness temperature)
        """
        output_dataset.createDimension('nvars', nvars)
        output_dataset.createVariable('nvars', 'i4', 'nvars')
        output_dataset.variables['nvars'][:] = np.arange(1, nvars + 1, 1, dtype='int32')

    @staticmethod
    def _create_ndatetime_dimension(output_dataset):
        """
        Creates the ndatetime dimension in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        ndatetime = 20
        output_dataset.createDimension('ndatetime', ndatetime)
        output_dataset.createVariable('ndatetime', 'i4', 'ndatetime')
        output_dataset.variables['ndatetime'][:] = np.arange(1, ndatetime + 1, 1, dtype='int32')

    @staticmethod
    def _create_nstring_dimension(output_dataset):
        """
        Creates the nstring dimension in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        nstring = 50
        output_dataset.createDimension('nstring', nstring)
        output_dataset.createVariable('nstring', 'i4', 'nstring')
        output_dataset.variables['nstring'][:] = np.arange(1, nstring + 1, 1, dtype='int32')

    @staticmethod
    def _create_varmetadata_sensor_channel_variable(output_dataset):
        """
        Creates the /VarMetaData/sensor_channel variable in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        output_dataset.createVariable('/VarMetaData/sensor_channel', 'i4', 'nchans')
        output_dataset['/VarMetaData/sensor_channel'][:] = output_dataset['nchans'][:]

    @staticmethod
    def _create_varmetadata_variable_names_reflectance_factor_variable(output_dataset):
        """
        Creates the /VarMetaData/variable_names variable for reflectance factor in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        output_dataset.createVariable('/VarMetaData/variable_names', 'str', 'nchans')
        temp_data_array = ['reflectance_factor_1', 'reflectance_factor_2', 'reflectance_factor_3',
                           'reflectance_factor_4', 'reflectance_factor_5', 'reflectance_factor_6']
        output_dataset['/VarMetaData/variable_names'][:] = np.array(temp_data_array)

    @staticmethod
    def _create_varmetadata_variable_names_brightness_temperature_variable(output_dataset):
        """
        Creates the /VarMetaData/variable_names variable for brightness temperature in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        output_dataset.createVariable('/VarMetaData/variable_names', 'str', 'nchans')
        temp_data_array = ['brightness_temperature_7', 'brightness_temperature_8', 'brightness_temperature_9',
                           'brightness_temperature_10', 'brightness_temperature_11', 'brightness_temperature_12',
                           'brightness_temperature_13', 'brightness_temperature_14', 'brightness_temperature_15',
                           'brightness_temperature_16']
        output_dataset['/VarMetaData/variable_names'][:] = np.array(temp_data_array)

    @staticmethod
    def _create_root_group_attributes(output_dataset):
        """
        Creates several root group attributes in an output netCDF4 dataset.
        output_dataset - A netCDF4 Dataset object
        """
        output_dataset.setncattr('_ioda_layout', 'ObsGroup')
        output_dataset.setncattr('_ioda_layout_version', '0')

    @staticmethod
    def _get_nlocs(dataset):
        """
        Returns the nlocs dimension size for the provided netCDF4 Dataset.
        dataset - the dataset to extract the nlocs size
        """
        return dataset.dimensions['nlocs'].size

    def _create_preqc_reflectance_factor_variable(self, output_dataset):
        """
        Creates the /PreQC/reflectance_factor variable variable and associated attributes in an output netCDF4 dataset.
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
        output_dataset.createVariable('/PreQC/reflectance_factor', 'f4', ('nlocs', 'nchans'), fill_value=-999)
        output_dataset['/PreQC/reflectance_factor'][:] = data_array
        output_dataset['/PreQC/reflectance_factor'].setncattr('flag_values', '0,1,2,3')
        output_dataset['/PreQC/reflectance_factor'].setncattr('flag_meanings',
                                                              'good_pixel_qf,conditionally_usable_pixel_qf,'
                                                              'out_of_range_pixel_qf,no_value_pixel_qf')

    def _create_preqc_brightness_temperature_variable(self, output_dataset):
        """
        Creates the /PreQC/brightness_temperature variable and associated attributes in an output netCDF4 dataset.
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
        output_dataset.createVariable('/PreQC/brightness_temperature', 'f4', ('nlocs', 'nchans'),
                                      fill_value=-999)
        output_dataset['/PreQC/brightness_temperature'][:] = data_array
        output_dataset['/PreQC/brightness_temperature'].setncattr('flag_values', '0,1,2,3')
        output_dataset['/PreQC/brightness_temperature'].setncattr('flag_meanings',
                                                                  'good_pixel_qf,conditionally_usable_pixel_qf,'
                                                                  'out_of_range_pixel_qf,no_value_pixel_qf')

    def _create_obsvalue_reflectance_factor_variable(self, output_dataset):
        """
        Creates the /ObsValue/reflectance_factor variable in an output netCDF4 dataset.
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
        output_dataset.createVariable('/ObsValue/reflectance_factor', 'f4', ('nlocs', 'nchans'),
                                      fill_value=-999)
        output_dataset['/ObsValue/reflectance_factor'][:] = data_array

    def _create_obsvalue_brightness_temperature_variable(self, output_dataset):
        """
        Creates the /ObsValue/brightness_temperature variable in an output netCDF4 dataset.
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
        output_dataset.createVariable('/ObsValue/brightness_temperature', 'f4', ('nlocs', 'nchans'),
                                      fill_value=-999)
        output_dataset['/ObsValue/brightness_temperature'][:] = data_array
        output_dataset['/ObsValue/brightness_temperature'].setncattr('units', 'K')

    def _create_obserror_reflectance_factor_variable(self, output_dataset):
        """
        Creates the /ObsError/reflectance_factor variable in an output netCDF4 dataset.
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
        output_dataset.createVariable('/ObsError/reflectance_factor', 'f4', ('nlocs', 'nchans'),
                                      fill_value=-999)
        output_dataset['/ObsError/reflectance_factor'][:] = data_array

    def _create_obserror_brightness_temperature_variable(self, output_dataset):
        """
        Creates the /ObsError/brightness_temperature variable in an output netCDF4 dataset.
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
        output_dataset.createVariable('/ObsError/brightness_temperature', 'f4', ('nlocs', 'nchans'),
                                      fill_value=-999)
        output_dataset['/ObsError/brightness_temperature'][:] = data_array
        output_dataset['/ObsError/brightness_temperature'].setncattr('units', 'K')

    def _create_metadata_time_variable(self, output_dataset):
        """
        Creates the /MetaData/datetime and MetaData/time variables and /date_time attribute in an output netCDF4
        dataset.
        output_dataset - A netCDF4 Dataset object
        """
        start_date = self._start_date.strftime('%Y-%m-%dT%H:%M:%SZ')
        datetime_array = np.full(self._get_nlocs(output_dataset), start_date)
        time_array = np.full(self._get_nlocs(output_dataset), 0.0)
        output_dataset.createVariable('/MetaData/datetime', 'str', 'nlocs')
        output_dataset['/MetaData/datetime'][:] = datetime_array
        output_dataset.createVariable('/MetaData/time', 'f4', 'nlocs')
        output_dataset['/MetaData/time'][:] = time_array
        output_dataset.setncattr("date_time", start_date)

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
        GoesConverter._create_root_group_attributes(dataset)
        GoesConverter._create_nchans_dimension(dataset, 10)
        GoesConverter._create_nvars_dimension(dataset, 10)
        GoesConverter._create_ndatetime_dimension(dataset)
        GoesConverter._create_nstring_dimension(dataset)
        self._create_nlocs_dimension(dataset)
        GoesConverter._create_varmetadata_sensor_channel_variable(dataset)
        GoesConverter._create_varmetadata_variable_names_brightness_temperature_variable(dataset)
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
        GoesConverter._create_root_group_attributes(dataset)
        GoesConverter._create_nchans_dimension(dataset, 6)
        GoesConverter._create_nvars_dimension(dataset, 6)
        GoesConverter._create_ndatetime_dimension(dataset)
        GoesConverter._create_nstring_dimension(dataset)
        self._create_nlocs_dimension(dataset)
        GoesConverter._create_varmetadata_sensor_channel_variable(dataset)
        GoesConverter._create_varmetadata_variable_names_reflectance_factor_variable(dataset)
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
