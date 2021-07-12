#!/usr/bin/env python

import datetime
import os
import sys
import numpy
import pytz
from netCDF4 import Dataset
from numpy import ma
from solo.date import JediDate, CoreDate
from goes16 import Goes16
from goes16_latlon import Goes16LatLon


class Goes16Converter:

    def __init__(self, input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt):
        self._input_files_paths = input_file_paths
        self._latlon_file_path = latlon_file_path
        self._output_file_path_rf = output_file_path_rf
        self._output_file_path_bt = output_file_path_bt
        self._latlon_dataset = None
        self._output_dataset_bt = None
        self._output_dataset_rf = None
        self._check_arguments()

    def _check_arguments(self):
        good_args = True
        if not os.path.exists(self._input_files_path):
            print("ERROR: Input GOES16 files do not exist: " + self._input_files_path)
            good_args = False
        if not good_args:
            sys.exit(2)

    def _create_input_data_file_dicts(self):
        self._input_file_paths.sort()
        self._goes16_dict_rf = {}
        self._goes16_dict_bt = {}
        for input_file_path in self._input_file_paths:
            goes16 = Goes16(input_file_path)
            abi_channel = int(goes16.get_abi_channel())
            if abi_channel < 7:
                self._goes16_dict_rf[abi_channel] = goes16
            else:
                self._goes16_dict_bt[abi_channel] = goes16
        self._template_input_file_path_rf = self._goes16_dict_rf[1].get_input_file_path()
        self._template_input_file_path_bt = self._goes16_dict_bt[7].get_input_file_path()

    def _check_nadir(self):
        lat_nadir_latlon, lon_nadir_latlon = self._get_nadir_latlon()
        lat_nadir_template, lon_nadir_template = self._get_nadir_template()
        return lat_nadir_latlon == lat_nadir_template and lon_nadir_latlon == lon_nadir_template

    def _get_nadir_latlon(self):
        dataset = Dataset(self._latlon_file_path, 'r')
        lat_nadir_latlon = dataset['MetaData'].variables['latitude'].getncattr('lat_nadir')
        lon_nadir_latlon = dataset['MetaData'].variables['longitude'].getncattr('lon_nadir')
        dataset.close()
        return lat_nadir_latlon, lon_nadir_latlon

    def _get_nadir_template(self):
        dataset = Dataset(self._template_input_file_path_rf, 'r')
        lat_nadir_template = dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lat_nadir')
        lon_nadir_template = dataset.variables['geospatial_lat_lon_extent'].getncattr('geospatial_lon_nadir')
        dataset.close()
        return lat_nadir_template, lon_nadir_template

    def _check_latlon_file_path(self):
        return os.path.exists(self._latlon_file_path)

    def _create_latlon_dataset(self):
        self._goes16_lat_lon = Goes16LatLon(self._template_input_file_path_rf, self._latlon_file_path, )
        self._goes16_lat_lon.create()

    def _get_yaw_flip_flag(self):
        dataset = Dataset(self._template_input_file_path_rf, 'r')
        self._yaw_flip_flag = dataset.variables['yaw_flip_flag'][0]
        dataset.close()

    def _get_kappa0(self):
        dataset = Dataset(self._template_input_file_path_rf, 'r')
        self._kappa0 = dataset.variables['kappa0'][0]
        dataset.close()

    def _get_planck_constants(self):
        dataset = Dataset(self._template_input_file_path_bt, 'r')
        self._planck_bc1 = dataset.variables['planck_bc1'][0]
        self._planck_bc2 = dataset.variables['planck_bc2'][0]
        self._planck_fk1 = dataset.variables['planck_fk1'][0]
        self._planck_fk2 = dataset.variables['planck_fk2'][0]
        dataset.close()

    def _close_output_datasets(self):
        self._output_dataset_bt.close()
        self._output_dataset_rf.close()

    def _import_latlon_dataset(self, output_dataset):
        self._latlon_dataset = Dataset(self._latlon_file_path, 'r')
        lat_data_array = self._latlon_dataset['MetaData'].variables['latitude'][:].real
        lon_data_array = self._latlon_dataset['MetaData'].variables['longitude'][:].real
        nlocs = len(lat_data_array)
        output_dataset.createDimension('nlocs', nlocs)
        output_dataset.createVariable('nlocs', 'i4', 'nlocs')
        output_dataset.variables['nlocs'].setncattr('suggested_chunk_dim', nlocs)
        output_dataset.variables['nlocs'][:] = numpy.arange(1, nlocs + 1, 1, dtype='int32')
        output_dataset.createVariable('/MetaData/latitude', 'f4', 'nlocs', fill_value=-999)
        output_dataset.createVariable('/MetaData/longitude', 'f4', 'nlocs', fill_value=-999)
        output_dataset['/MetaData/latitude'][:] = lat_data_array
        output_dataset['/MetaData/longitude'][:] = lon_data_array
        self._latlon_dataset.close()

    @staticmethod
    def _create_groups(dataset):
        dataset.createGroup('EffectiveError')
        dataset.createGroup('EffectiveQC')
        dataset.createGroup('MetaData')
        dataset.createGroup('ObsBias')
        dataset.createGroup('ObsError')
        dataset.createGroup('ObsValue')
        dataset.createGroup('PreQC')
        dataset.createGroup('VarMetaData')

    def _create_nchans_dimension(self):
        nchans_rf = 6
        nchans_bt = 10
        self._output_dataset_rf.createDimension('nchans', nchans_rf)
        self._output_dataset_rf.createVariable('nchans', 'i4', 'nchans')
        self._output_dataset_rf.variables['nchans'][:] = [1, 2, 3, 4, 5, 6]
        self._output_dataset_bt.createDimension('nchans', nchans_bt)
        self._output_dataset_bt.createVariable('nchans', 'i4', 'nchans')
        self._output_dataset_bt.variables['nchans'][:] = [7, 8, 9, 10, 11, 12, 13, 14, 15, 16]

    @staticmethod
    def _get_nlocs(dataset):
        return dataset.dimensions['nlocs'].size

    def _create_rad_data_arrays(self):
        self._rad_data_array_rf_dict = {}
        counter = 0
        for key in self._goes16_dict_rf.keys():
            goes16 = self._goes16_dict_rf[key]
            goes16.load()
            self._rad_data_array_rf_dict[counter] = goes16.get_rad_data_array()
            counter += 1
        self._rad_data_array_rf = self._rad_data_array_rf_dict[0]
        for i in range(1, counter):
            self._rad_data_array_rf = numpy.column_stack((self._rad_data_array_rf, self._rad_data_array_rf_dict[i]))

        self._rad_data_array_bt_dict = {}
        counter = 0
        for key in self._goes16_dict_bt.keys():
            goes16 = self._goes16_dict_bt[key]
            goes16.load()
            self._rad_data_array_bt_dict[counter] = goes16.get_rad_data_array()
            counter += 1
        self._rad_data_array_bt = self._rad_data_array_bt_dict[0]
        for i in range(1, counter):
            self._rad_data_array_bt = numpy.column_stack((self._rad_data_array_bt, self._rad_data_array_bt_dict[i]))
        self._rad_data_array_bt = numpy.array(self._rad_data_array_bt)

    def _create_rf_variable(self):
        self._get_kappa0()
        self._rf_data_array = ma.getdata(self._rad_data_array_rf) * self._kappa0
        if self._yaw_flip_flag:
            self._rf_data_array = self._rf_data_array[::-1]
        self._output_dataset_rf.createVariable('/ObsValue/reflectance_factor', 'f4', ('nlocs', 'nchans'),
                                               fill_value=-999)
        self._output_dataset_rf['/ObsValue/reflectance_factor'][:] = self._rf_data_array

    def _create_bt_variable(self):
        self._get_planck_constants()
        log_comp = numpy.log((self._planck_fk1 / self._rad_data_array_bt) + 1)
        self._bt_data_array = ((self._planck_fk2 / log_comp) - self._planck_bc1) / self._planck_bc2
        if self._yaw_flip_flag:
            self._bt_data_array = self._bt_data_array[::-1]
        self._output_dataset_bt.createVariable('/ObsValue/brightness_temperature', 'f4', ('nlocs', 'nchans'),
                                               fill_value=-999)
        self._output_dataset_bt['/ObsValue/brightness_temperature'][:] = self._bt_data_array

    def _create_metadata_time_variable_rf(self):
        dataset = Dataset(self._template_input_file_path_rf, 'r')
        t_epoch = datetime.datetime(2000, 1, 1, 12, 0, 0, 0, pytz.UTC)
        t_mid = t_epoch + datetime.timedelta(seconds=int(round(float(dataset.variables['t'][0]))))
        t_refdate = t_mid.replace(minute=0, second=0, microsecond=0)
        if t_mid.minute >= 30:
            t_refdate = t_refdate + datetime.timedelta(hours=1)
        datetime_str = str(JediDate(t_refdate))
        datetime_array = numpy.full(self._get_nlocs(self._output_dataset_rf), datetime_str)
        self._output_dataset_rf.createVariable('/MetaData/datetime', 'str', 'nlocs')
        self._output_dataset_rf['/MetaData/datetime'][:] = datetime_array
        date_time = f'{CoreDate(t_refdate).year()}{CoreDate(t_refdate).month()}{CoreDate(t_refdate).day()}{CoreDate(t_refdate).hour()}'
        self._output_dataset_rf.setncattr("date_time", date_time)
        dataset.close()

    def _create_metadata_time_variable_bt(self):
        dataset = Dataset(self._template_input_file_path_bt, 'r')
        t_epoch = datetime.datetime(2000, 1, 1, 12, 0, 0, 0, pytz.UTC)
        t_mid = t_epoch + datetime.timedelta(seconds=int(round(float(dataset.variables['t'][0]))))
        t_refdate = t_mid.replace(minute=0, second=0, microsecond=0)
        if t_mid.minute >= 30:
            t_refdate = t_refdate + datetime.timedelta(hours=1)
        datetime_str = str(JediDate(t_refdate))
        datetime_array = numpy.full(self._get_nlocs(self._output_dataset_bt), datetime_str)
        self._output_dataset_bt.createVariable('/MetaData/datetime', 'str', 'nlocs')
        self._output_dataset_bt['/MetaData/datetime'][:] = datetime_array
        date_time = f'{CoreDate(t_refdate).year()}{CoreDate(t_refdate).month()}{CoreDate(t_refdate).day()}{CoreDate(t_refdate).hour()}'
        self._output_dataset_bt.setncattr("date_time", date_time)
        dataset.close()

    def convert(self):
        self._create_input_data_file_dicts()
        if self._check_latlon_file_path():
            if not self._check_nadir():
                self._create_latlon_dataset()
        else:
            self._create_latlon_dataset()
        self._get_yaw_flip_flag()
        self._output_dataset_rf = Dataset(self._output_file_path_rf, 'w')
        self._output_dataset_bt = Dataset(self._output_file_path_bt, 'w')
        Goes16Converter._create_groups(self._output_dataset_bt)
        Goes16Converter._create_groups(self._output_dataset_rf)
        self._import_latlon_dataset(self._output_dataset_rf)
        self._import_latlon_dataset(self._output_dataset_bt)
        self._create_nchans_dimension()
        self._create_rad_data_arrays()
        self._create_rf_variable()
        self._create_bt_variable()
        self._create_metadata_time_variable_rf()
        self._create_metadata_time_variable_bt()
        self._close_output_datasets()
