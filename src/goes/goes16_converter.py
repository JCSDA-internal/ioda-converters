#
# goes16_converter.py
#
# This class generates two IODAv2 data files from a group of raw data files for all 16 channels of GOES-16 LB1 products.
# This class works with the Goes16 and Goes16LatLon classes. The final result of this class is two IODAv2 formatted
# data files - one for Brightness Temperature and one for Reflectance Factor. The following groups, variables,
# dimensions, and attributes are created using this class.
#
# /GROUP/VARIABLE -> ATTRIBUTE
#
# /MetaData/datetime
# /MetaData/elevation_angle
# /MetaData/latitude
# /MetaData/longitude
# /MetaData/scan_angle
# /ObsError/reflectance_factor or /ObsError/brightness_temperature
# /ObsValue/reflectance_factor or /ObsValue/brightness_temperature
# /PreQC/reflectance_factor or /PreQC/brightness_temperature
# /VarMetaData/sensor_channel
# /VarMetaData/variable_names
# /nchans
# /ndatetime
# /nlocs
# /nstring
# /nvars
#
import datetime
import os
import numpy
import pytz
from netCDF4 import Dataset
from numpy import ma
from solo.date import JediDate, CoreDate
from goes16 import Goes16
from goes16_latlon import Goes16LatLon


class Goes16Converter:

    def __init__(self, input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt):
        self._input_file_paths = input_file_paths
        self._latlon_file_path = latlon_file_path
        self._output_file_path_rf = output_file_path_rf
        self._output_file_path_bt = output_file_path_bt
        self._latlon_dataset = None
        self._output_dataset_bt = None
        self._output_dataset_rf = None
        self._check_arguments()

    def _check_arguments(self):
        good_args = True
        # if not os.path.exists(self._input_file_paths):
        #    print("ERROR: Input GOES16 files do not exist: " + self._input_files_path)
        #    good_args = False
        # if not good_args:
        #    sys.exit(2)

    def _create_input_data_file_dicts(self):
        self._input_file_paths.sort()
        self._goes16_dict_rf = {}
        self._goes16_dict_bt = {}
        for input_file_path in self._input_file_paths:
            goes16 = Goes16(input_file_path)
            goes16.load()
            abi_channel = int(goes16.get_abi_channel())
            if abi_channel < 7:
                self._goes16_dict_rf[abi_channel] = goes16
            else:
                self._goes16_dict_bt[abi_channel] = goes16
        self._template_input_file_path = self._goes16_dict_bt[7].get_input_file_path()

    def _check_nadir(self):
        lat_nadir_latlon, lon_nadir_latlon = self._get_nadir_attribute_latlon()
        lat_nadir_template, lon_nadir_template = self._get_nadir_attribute_template()
        return lat_nadir_latlon == lat_nadir_template and lon_nadir_latlon == lon_nadir_template

    def _get_nadir_attribute_latlon(self):
        dataset = Dataset(self._latlon_file_path, 'r')
        lat_nadir_latlon = dataset['MetaData'].variables['latitude'].getncattr('lat_nadir')
        lon_nadir_latlon = dataset['MetaData'].variables['longitude'].getncattr('lon_nadir')
        dataset.close()
        return lat_nadir_latlon, lon_nadir_latlon

    def _get_nadir_attribute_template(self):
        dataset = Dataset(self._template_input_file_path, 'r')
        lat_nadir_template = dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lat_nadir')
        lon_nadir_template = dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lon_nadir')
        dataset.close()
        return lat_nadir_template, lon_nadir_template

    def _check_latlon_file_path(self):
        return os.path.exists(self._latlon_file_path)

    def _create_latlon_dataset(self):
        self._goes16_lat_lon = Goes16LatLon(self._template_input_file_path, self._latlon_file_path)
        self._goes16_lat_lon.create()

    def _close_datasets(self):
        self._output_dataset_bt.close()
        self._output_dataset_rf.close()
        self._latlon_dataset.close()

    def _create_metadata_latitude_variables(self):
        latitude_data_array = self._latlon_dataset['MetaData'].variables['latitude'][:].real
        self._output_dataset_rf.createVariable('/MetaData/latitude', 'f4', 'nlocs', fill_value=-999)
        self._output_dataset_rf['/MetaData/latitude'][:] = latitude_data_array
        self._output_dataset_bt.createVariable('/MetaData/latitude', 'f4', 'nlocs', fill_value=-999)
        self._output_dataset_bt['/MetaData/latitude'][:] = latitude_data_array

    def _create_metadata_longitude_variables(self):
        longitude_data_array = self._latlon_dataset['MetaData'].variables['longitude'][:].real
        self._output_dataset_rf.createVariable('/MetaData/longitude', 'f4', 'nlocs', fill_value=-999)
        self._output_dataset_rf['/MetaData/longitude'][:] = longitude_data_array
        self._output_dataset_bt.createVariable('/MetaData/longitude', 'f4', 'nlocs', fill_value=-999)
        self._output_dataset_bt['/MetaData/longitude'][:] = longitude_data_array

    def _create_metadata_scan_angle_variables(self):
        scan_angle_data_array = self._latlon_dataset['MetaData'].variables['scan_angle'][:].real
        self._output_dataset_rf.createVariable('/MetaData/scan_angle', 'f4', 'nlocs', fill_value=-999)
        self._output_dataset_rf['/MetaData/scan_angle'][:] = scan_angle_data_array
        self._output_dataset_bt.createVariable('/MetaData/scan_angle', 'f4', 'nlocs', fill_value=-999)
        self._output_dataset_bt['/MetaData/scan_angle'][:] = scan_angle_data_array

    def _create_metadata_elevation_angle_variables(self):
        elevation_angle_data_array = self._latlon_dataset['MetaData'].variables['elevation_angle'][:].real
        self._output_dataset_rf.createVariable('/MetaData/elevation_angle', 'f4', 'nlocs', fill_value=-999)
        self._output_dataset_rf['/MetaData/elevation_angle'][:] = elevation_angle_data_array
        self._output_dataset_bt.createVariable('/MetaData/elevation_angle', 'f4', 'nlocs', fill_value=-999)
        self._output_dataset_bt['/MetaData/elevation_angle'][:] = elevation_angle_data_array

    def _create_groups(self):
        self._output_dataset_rf.createGroup('MetaData')
        self._output_dataset_rf.createGroup('ObsError')
        self._output_dataset_rf.createGroup('ObsValue')
        self._output_dataset_rf.createGroup('PreQC')
        self._output_dataset_rf.createGroup('VarMetaData')
        self._output_dataset_bt.createGroup('MetaData')
        self._output_dataset_bt.createGroup('ObsError')
        self._output_dataset_bt.createGroup('ObsValue')
        self._output_dataset_bt.createGroup('PreQC')
        self._output_dataset_bt.createGroup('VarMetaData')

    def _create_nlocs_dimensions(self):
        nlocs = self._latlon_dataset.dimensions['nlocs'].size
        self._output_dataset_rf.createDimension('nlocs', nlocs)
        self._output_dataset_rf.createVariable('nlocs', 'i4', 'nlocs')
        self._output_dataset_rf.variables['nlocs'].setncattr('suggested_chunk_dim', nlocs)
        self._output_dataset_rf.variables['nlocs'][:] = numpy.arange(1, nlocs + 1, 1, dtype='int32')
        self._output_dataset_bt.createDimension('nlocs', nlocs)
        self._output_dataset_bt.createVariable('nlocs', 'i4', 'nlocs')
        self._output_dataset_bt.variables['nlocs'].setncattr('suggested_chunk_dim', nlocs)
        self._output_dataset_bt.variables['nlocs'][:] = numpy.arange(1, nlocs + 1, 1, dtype='int32')

    def _create_nchans_dimensions(self):
        nchans_rf = 6
        nchans_bt = 10
        self._output_dataset_rf.createDimension('nchans', nchans_rf)
        self._output_dataset_rf.createVariable('nchans', 'i4', 'nchans')
        self._output_dataset_rf.variables['nchans'][:] = [1, 2, 3, 4, 5, 6]
        self._output_dataset_bt.createDimension('nchans', nchans_bt)
        self._output_dataset_bt.createVariable('nchans', 'i4', 'nchans')
        self._output_dataset_bt.variables['nchans'][:] = [7, 8, 9, 10, 11, 12, 13, 14, 15, 16]

    def _create_nvars_dimensions(self):
        nvars_rf = 6
        nvars_bt = 10
        self._output_dataset_rf.createDimension('nvars', nvars_rf)
        self._output_dataset_rf.createVariable('nvars', 'i4', 'nvars')
        self._output_dataset_rf.variables['nvars'][:] = numpy.arange(1, nvars_rf + 1, 1, dtype='int32')
        self._output_dataset_bt.createDimension('nvars', nvars_bt)
        self._output_dataset_bt.createVariable('nvars', 'i4', 'nvars')
        self._output_dataset_bt.variables['nvars'][:] = numpy.arange(1, nvars_bt + 1, 1, dtype='int32')

    def _create_ndatetime_dimensions(self):
        ndatetime = 20
        self._output_dataset_rf.createDimension('ndatetime', ndatetime)
        self._output_dataset_rf.createVariable('ndatetime', 'i4', 'ndatetime')
        self._output_dataset_rf.variables['ndatetime'][:] = numpy.arange(1, ndatetime + 1, 1, dtype='int32')
        self._output_dataset_bt.createDimension('ndatetime', ndatetime)
        self._output_dataset_bt.createVariable('ndatetime', 'i4', 'ndatetime')
        self._output_dataset_bt.variables['ndatetime'][:] = numpy.arange(1, ndatetime + 1, 1, dtype='int32')

    def _create_nstring_dimensions(self):
        nstring = 50
        self._output_dataset_rf.createDimension('nstring', nstring)
        self._output_dataset_rf.createVariable('nstring', 'i4', 'nstring')
        self._output_dataset_rf.variables['nstring'][:] = numpy.arange(1, nstring + 1, 1, dtype='int32')
        self._output_dataset_bt.createDimension('nstring', nstring)
        self._output_dataset_bt.createVariable('nstring', 'i4', 'nstring')
        self._output_dataset_bt.variables['nstring'][:] = numpy.arange(1, nstring + 1, 1, dtype='int32')

    @staticmethod
    def _get_nlocs(dataset):
        return dataset.dimensions['nlocs'].size

    def _create_preqc_reflectance_factor_variable(self):
        temp_dict = {}
        counter = 0
        for key in self._goes16_dict_rf.keys():
            goes16 = self._goes16_dict_rf[key]
            temp_dict[counter] = ma.getdata(goes16.get_preqc_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = numpy.column_stack((data_array, temp_dict[i]))
        self._output_dataset_rf.createVariable('/PreQC/reflectance_factor', 'f4', ('nlocs', 'nchans'), fill_value=-999)
        self._output_dataset_rf['/PreQC/reflectance_factor'][:] = data_array

    def _create_preqc_brightness_temperature_variable(self):
        temp_dict = {}
        counter = 0
        for key in self._goes16_dict_bt.keys():
            goes16 = self._goes16_dict_bt[key]
            temp_dict[counter] = ma.getdata(goes16.get_preqc_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = numpy.column_stack((data_array, temp_dict[i]))
        self._output_dataset_bt.createVariable('/PreQC/brightness_temperature', 'f4', ('nlocs', 'nchans'),
                                               fill_value=-999)
        self._output_dataset_bt['/PreQC/brightness_temperature'][:] = data_array

    def _create_obsvalue_reflectance_factor_variable(self):
        temp_dict = {}
        counter = 0
        for key in self._goes16_dict_rf.keys():
            goes16 = self._goes16_dict_rf[key]
            temp_dict[counter] = ma.getdata(goes16.get_obsvalue_rf_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = numpy.column_stack((data_array, temp_dict[i]))
        self._output_dataset_rf.createVariable('/ObsValue/reflectance_factor', 'f4', ('nlocs', 'nchans'),
                                               fill_value=-999)
        self._output_dataset_rf['/ObsValue/reflectance_factor'][:] = data_array

    def _create_obsvalue_brightness_temperature_variable(self):
        temp_dict = {}
        counter = 0
        for key in self._goes16_dict_bt.keys():
            goes16 = self._goes16_dict_bt[key]
            temp_dict[counter] = ma.getdata(goes16.get_obsvalue_bt_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = numpy.column_stack((data_array, temp_dict[i]))
        self._output_dataset_bt.createVariable('/ObsValue/brightness_temperature', 'f4', ('nlocs', 'nchans'),
                                               fill_value=-999)
        self._output_dataset_bt['/ObsValue/brightness_temperature'][:] = data_array

    def _create_obserror_reflectance_factor_variable(self):
        temp_dict = {}
        counter = 0
        for key in self._goes16_dict_rf.keys():
            goes16 = self._goes16_dict_rf[key]
            temp_dict[counter] = ma.getdata(goes16.get_obserror_rf_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = numpy.column_stack((data_array, temp_dict[i]))
        self._output_dataset_rf.createVariable('/ObsError/reflectance_factor', 'f4', ('nlocs', 'nchans'),
                                               fill_value=-999)
        self._output_dataset_rf['/ObsError/reflectance_factor'][:] = data_array

    def _create_obserror_brightness_temperature_variable(self):
        temp_dict = {}
        counter = 0
        for key in self._goes16_dict_bt.keys():
            goes16 = self._goes16_dict_bt[key]
            temp_dict[counter] = ma.getdata(goes16.get_obserror_bt_data_array())
            counter += 1
        data_array = temp_dict[0]
        for i in range(1, counter):
            data_array = numpy.column_stack((data_array, temp_dict[i]))
        self._output_dataset_bt.createVariable('/ObsError/brightness_temperature', 'f4', ('nlocs', 'nchans'),
                                               fill_value=-999)
        self._output_dataset_bt['/ObsError/brightness_temperature'][:] = data_array

    def _create_metadata_time_variables(self):
        dataset = Dataset(self._template_input_file_path, 'r')
        t_epoch = datetime.datetime(2000, 1, 1, 12, 0, 0, 0, pytz.UTC)
        t_mid = t_epoch + datetime.timedelta(seconds=int(round(float(dataset.variables['t'][0]))))
        t_refdate = t_mid.replace(minute=0, second=0, microsecond=0)
        if t_mid.minute >= 30:
            t_refdate = t_refdate + datetime.timedelta(hours=1)
        datetime_str = str(JediDate(t_refdate))
        datetime_array = numpy.full(self._get_nlocs(self._output_dataset_rf), datetime_str)
        self._output_dataset_rf.createVariable('/MetaData/datetime', 'str', 'nlocs')
        self._output_dataset_rf['/MetaData/datetime'][:] = datetime_array
        self._output_dataset_bt.createVariable('/MetaData/datetime', 'str', 'nlocs')
        self._output_dataset_bt['/MetaData/datetime'][:] = datetime_array
        date_time = f'{CoreDate(t_refdate).year()}{CoreDate(t_refdate).month()}{CoreDate(t_refdate).day()}{CoreDate(t_refdate).hour()}'
        self._output_dataset_rf.setncattr("date_time", date_time)
        self._output_dataset_bt.setncattr("date_time", date_time)
        dataset.close()

    def _create_varmetadata_sensor_channel_variables(self):
        self._output_dataset_rf.createVariable('/VarMetaData/sensor_channel', 'i4', 'nchans')
        self._output_dataset_rf['/VarMetaData/sensor_channel'][:] = self._output_dataset_rf['nchans'][:]
        self._output_dataset_bt.createVariable('/VarMetaData/sensor_channel', 'i4', 'nchans')
        self._output_dataset_bt['/VarMetaData/sensor_channel'][:] = self._output_dataset_bt['nchans'][:]

    def _create_varmetadata_variable_names_variables(self):
        self._output_dataset_rf.createVariable('/VarMetaData/variable_names', 'str', 'nchans')
        temp_data_array = ['reflectance_factor_1', 'reflectance_factor_2', 'reflectance_factor_3',
                           'reflectance_factor_4', 'reflectance_factor_5', 'reflectance_factor_6']
        self._output_dataset_rf['/VarMetaData/variable_names'][:] = numpy.array(temp_data_array)
        self._output_dataset_bt.createVariable('/VarMetaData/variable_names', 'str', 'nchans')
        temp_data_array = ['brightness_temperature_7', 'brightness_temperature_8', 'brightness_temperature_9',
                           'brightness_temperature_10', 'brightness_temperature_11', 'brightness_temperature_12',
                           'brightness_temperature_13', 'brightness_temperature_14', 'brightness_temperature_15',
                           'brightness_temperature_16']
        self._output_dataset_bt['/VarMetaData/variable_names'][:] = numpy.array(temp_data_array)

    def convert(self):
        self._create_input_data_file_dicts()
        if self._check_latlon_file_path():
            if not self._check_nadir():
                self._create_latlon_dataset()
        else:
            self._create_latlon_dataset()
        self._output_dataset_rf = Dataset(self._output_file_path_rf, 'w')
        self._output_dataset_bt = Dataset(self._output_file_path_bt, 'w')
        self._latlon_dataset = Dataset(self._latlon_file_path, 'r')
        self._create_groups()
        self._create_nlocs_dimensions()
        self._create_nchans_dimensions()
        self._create_nvars_dimensions()
        self._create_ndatetime_dimensions()
        self._create_nstring_dimensions()
        self._create_metadata_latitude_variables()
        self._create_metadata_longitude_variables()
        self._create_metadata_elevation_angle_variables()
        self._create_metadata_scan_angle_variables()
        self._create_obsvalue_reflectance_factor_variable()
        self._create_obsvalue_brightness_temperature_variable()
        self._create_obserror_reflectance_factor_variable()
        self._create_obserror_brightness_temperature_variable()
        self._create_preqc_reflectance_factor_variable()
        self._create_preqc_brightness_temperature_variable()
        self._create_metadata_time_variables()
        self._create_varmetadata_sensor_channel_variables()
        self._create_varmetadata_variable_names_variables()
        self._close_datasets()
