import datetime
import os
import sys
from enum import Enum
import numpy
import pytz
from netCDF4 import Dataset
from numpy import ma
from solo.date import Date


class Goes16:

    def __init__(self, input_file_path, output_file_path):
        self._input_file_path = input_file_path
        self._output_file_path = output_file_path
        self._check_paths()
        self._metadata_dict = {'instrument': 'ABI',
                               'processing_level': 'L1b',
                               'product_acronym': 'Rad',
                               'platform_identifier': 'G16'}
        self._get_metadata_from_input_file_path()
        self._open_datasets()
        self._input_dataset.set_auto_scale(True)
        self._latitude_data_array = None
        self._longitude_data_array = None

    def _check_paths(self):
        good_paths = True
        if not os.path.exists(self._input_file_path):
            print("ERROR: Input GOES16 file does not exist: " + self._input_file_path)
            good_paths = False
        if not good_paths:
            sys.exit(2)

    def _get_metadata_from_input_file_path(self):
        metadata_array = os.path.splitext(os.path.basename(self._input_file_path))[0].split('_')
        self._metadata_dict['system_environment'] = metadata_array[0]
        self._metadata_dict['abi_sector_type'] = Goes16._string_to_abisectortype(metadata_array[1])
        self._metadata_dict['abi_mode'] = Goes16._string_to_abimode(metadata_array[1])
        self._metadata_dict['abi_channel'] = int(metadata_array[1][-2:])
        self._metadata_dict['start_date'] = Date(metadata_array[3][1:-1])
        self._metadata_dict['end_date'] = Date(metadata_array[4][1:-1])
        self._metadata_dict['creation_date'] = Date(metadata_array[5][1:-1])

    def _open_datasets(self):
        self._input_dataset = Dataset(self._input_file_path, 'r')
        self._output_dataset = Dataset(self._output_file_path, 'w', format='NETCDF4', engine='netcdf4')

    def _create_reflectance_variable(self):
        rad_data_array = ma.getdata(self._input_dataset.variables['Rad'][:].real)
        kappa0 = ma.getdata(self._input_dataset.variables['kappa0'][:].real)
        reflectance_data_array = rad_data_array * kappa0
        if self._yaw_flip_flag:
            reflectance_data_array = reflectance_data_array[::-1]

    def _create_brightness_temp_variable(self):
        rad_data_array = ma.getdata(self._input_dataset.variables['Rad'][:].real)
        planck_bc1 = ma.getdata(self._input_dataset.variables['planck_bc1'][:].real)
        planck_bc2 = ma.getdata(self._input_dataset.variables['planck_bc2'][:].real)
        planck_fk1 = ma.getdata(self._input_dataset.variables['planck_fk1'][:].real)
        planck_fk2 = ma.getdata(self._input_dataset.variables['planck_fk2'][:].real)
        print(planck_bc1, planck_bc2, planck_fk1, planck_fk2)
        log_comp = numpy.log((planck_fk1 / rad_data_array) + 1)
        brightness_temp_data_array = ((planck_fk2 / log_comp) - planck_bc1) / planck_bc2 - 273.15
        print(brightness_temp_data_array)
        if self._yaw_flip_flag:
            brightness_temp_data_array = brightness_temp_data_array[::-1]

    def _create_earth_locs(self):
        self._earth_locs = ~numpy.ma.getmaskarray(self._input_dataset.variables['DQF'][:])

    def _create_yaw_flip_flag(self):
        self._yaw_flip_flag = self._input_dataset.variables['yaw_flip_flag'][0]

    def _create_group(self, group):
        self._output_dataset.createGroup(group)

    def _create_groups(self):
        self._create_group('EffectiveError')
        self._create_group('EffectiveQC')
        self._create_group('MetaData')
        self._create_group('ObsBias')
        self._create_group('ObsError')
        self._create_group('ObsValue')
        self._create_group('PreQC')
        self._create_group('VarMetaData')

    def _create_dimensions(self):
        nlocs = self._output_dataset.dimensions['nlocs'].size
        nvars = 1
        nrecs = 1
        nobs = nvars * nlocs
        self._output_dataset.createDimension('nvars', nvars)
        self._output_dataset.createDimension('nrecs', nrecs)
        self._output_dataset.createDimension('nobs', nobs)

    def _create_time_variable(self):
        t_epoch = datetime.datetime(2000, 1, 1, 12, 0, 0, 0, pytz.UTC)
        t_start = t_epoch + datetime.timedelta(
            seconds=int(round(float(self._input_dataset.variables['time_bounds'][0]))))
        t_mid = t_epoch + datetime.timedelta(seconds=int(round(float(self._input_dataset.variables['t'][0]))))
        t_end = t_epoch + datetime.timedelta(seconds=int(round(float(self._input_dataset.variables['time_bounds'][1]))))
        t_refdate = t_mid.replace(minute=0, second=0, microsecond=0)

        if t_mid.minute >= 30:
            t_refdate = t_refdate + datetime.timedelta(hours=1)

        t_s = (t_start - t_refdate).total_seconds()
        t_e = (t_end - t_refdate).total_seconds()

        timesteps = numpy.linspace(t_s, t_e, num=self._earth_locs.size, dtype=int).reshape(self._earth_locs.shape)
        timesteps = timesteps[self._earth_locs]
        offset_time = timesteps.astype(float) / 3600.0

        date_time = (t_refdate.year * 1000000 + t_refdate.month * 10000 + t_refdate.day * 100 + t_refdate.hour)
        self._output_dataset.setncattr("date_time", date_time)
        self._create_netcdf_variable("/MetaData/time", 'nlocs', offset_time)

    def _create_netcdf_variable(self, name, dims, values):
        dtype = Goes16._numpy_to_netcdf_dtype(values.dtype)
        self._output_dataset.createVariable(name, dtype, dims)
        self._output_dataset[name][:] = values

    def _create_lat_lon_variables(self):
        self._output_dataset.createDimension('nlocs', len(self._latitude_data_array))
        self._output_dataset.createVariable('nlocs', 'int32', ('nlocs',))
        self._output_dataset.createVariable('/MetaData/latitude', 'f4', 'nlocs')
        self._output_dataset.createVariable('/MetaData/longitude', 'f4', 'nlocs')
        self._output_dataset['/MetaData/latitude'][:] = self._latitude_data_array
        self._output_dataset['/MetaData/longitude'][:] = self._longitude_data_array

    @staticmethod
    def _numpy_to_netcdf_dtype(numpy_dtype):
        numpy_to_netcdf_dtype = {numpy.dtype('float64'): 'f8',
                                 numpy.dtype('float32'): 'f4',
                                 numpy.dtype('int64'): 'i8',
                                 numpy.dtype('int32'): 'i4',
                                 numpy.dtype('S1'): 'c',
                                 numpy.dtype('object'): 'str'}
        return numpy_to_netcdf_dtype[numpy_dtype]

    @staticmethod
    def _string_to_abimode(string):
        if 'M4' in string:
            return ABIMode.ABI_SCAN_MODE_4
        if 'M6' in string:
            return ABIMode.ABI_SCAN_MODE_6

    @staticmethod
    def _string_to_abisectortype(string):
        if 'F' in string:
            return ABISectorType.FULL_DISK
        if 'C' in string:
            return ABISectorType.CONUS
        if 'M1' in string:
            return ABISectorType.MESOSCALE_REGION_1
        if 'M2' in string:
            return ABISectorType.MESOSCALE_REGION_2

    def set_lat_lon_arrays(self, latitude_data_array, longitude_data_array):
        self._latitude_data_array = latitude_data_array
        self._longitude_data_array = longitude_data_array

    def close_datasets(self):
        self._input_dataset.close()
        self._output_dataset.close()

    def get_abi_channel(self):
        return self._metadata_dict['abi_channel']

    def convert(self):
        self._create_earth_locs()
        self._create_yaw_flip_flag()
        self._create_groups()
        self._create_lat_lon_variables()
        self._create_dimensions()
        self._create_time_variable()
        if self._metadata_dict['abi_channel'] < 7:
            self._create_reflectance_variable()
        else:
            self._create_brightness_temp_variable()


class ABIMode(Enum):
    ABI_SCAN_MODE_4 = 1
    ABI_SCAN_MODE_6 = 2


class ABISectorType(Enum):
    FULL_DISK = 1
    CONUS = 2
    MESOSCALE_REGION_1 = 3
    MESOSCALE_REGION_2 = 4
