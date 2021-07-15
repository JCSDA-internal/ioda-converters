import os
from enum import Enum
from statistics import fmean
import numpy
from netCDF4 import Dataset
from numpy import ma
from solo.date import Date


class Goes16:

    def __init__(self, input_file_path):
        self._input_file_path = input_file_path
        self._get_metadata_from_input_file_path()
        self._rad_data_array = None
        self._dqf_data_array = None
        self._obsvalue_rf_data_array = None
        self._obsvalue_bt_data_array = None
        self._obserror_rf_data_array = None
        self._obserror_bt_data_array = None

    def _get_metadata_from_input_file_path(self):
        self._metadata_dict = {'instrument': 'ABI',
                               'processing_level': 'L1b',
                               'product_acronym': 'Rad',
                               'platform_identifier': 'G16'}
        metadata_array = os.path.splitext(os.path.basename(self._input_file_path))[0].split('_')
        self._metadata_dict['system_environment'] = metadata_array[0]
        self._metadata_dict['abi_sector_type'] = Goes16._string_to_abisectortype(metadata_array[1])
        self._metadata_dict['abi_mode'] = Goes16._string_to_abimode(metadata_array[1])
        self._metadata_dict['abi_channel'] = int(metadata_array[1][-2:])
        self._metadata_dict['start_date'] = Date(metadata_array[3][1:-1])
        self._metadata_dict['end_date'] = Date(metadata_array[4][1:-1])
        self._metadata_dict['creation_date'] = Date(metadata_array[5][1:-1])

    def _open(self):
        self._input_dataset = Dataset(self._input_file_path, 'r')

    def _load_yaw_flip_flag_variable(self):
        self._yaw_flip_flag = self._input_dataset.variables['yaw_flip_flag'][0]

    def _load_kappa0_variable(self):
        self._kappa0 = ma.getdata(self._input_dataset.variables['kappa0'][0])

    def _load_planck_variables(self):
        self._planck_bc1 = self._input_dataset.variables['planck_bc1'][0]
        self._planck_bc2 = self._input_dataset.variables['planck_bc2'][0]
        self._planck_fk1 = self._input_dataset.variables['planck_fk1'][0]
        self._planck_fk2 = self._input_dataset.variables['planck_fk2'][0]

    def _load_std_dev_radiance_value_of_valid_pixels_variable(self):
        self._std_dev_radiance_value_of_valid_pixels = \
            self._input_dataset.variables['std_dev_radiance_value_of_valid_pixels'][0]

    def _load_valid_pixel_count_variable(self):
        self._valid_pixel_count = self._input_dataset.variables['valid_pixel_count'][0]

    def _load_dqf_data_array(self):
        self._dqf_data_array = ma.getdata(self._input_dataset.variables['DQF'][:].real)

    def _load_rad_data_array(self):
        self._rad_data_array = ma.getdata(self._input_dataset.variables['Rad'][:].real)

    @staticmethod
    def _subsample(rad_data_array, dqf_data_array, increment):
        current_dim = len(rad_data_array)
        rad_data_array = numpy.asarray(rad_data_array)
        new_rad_data_array = rad_data_array[0:current_dim:increment, 0:current_dim:increment]
        dqf_data_array = numpy.asarray(dqf_data_array)
        new_dqf_data_array = dqf_data_array[0:current_dim:increment, 0:current_dim:increment]
        return new_rad_data_array, new_dqf_data_array

    @staticmethod
    def _subsample2(rad_data_array, dqf_data_array, increment):
        current_dim = len(rad_data_array)
        new_dim = int(current_dim / increment)
        new_rad_data_array = [[0] * new_dim] * new_dim
        new_dqf_data_array = [[0] * new_dim] * new_dim

        k, l = 0, 0
        for i in range(0, current_dim, increment):
            for j in range(0, current_dim, increment):
                new_rad_data_array[k][l] = rad_data_array[i][j]
                new_dqf_data_array[k][l] = dqf_data_array[i][j]
                l += 1
                if l == new_dim:
                    l = 0
                    k += 1

        return new_rad_data_array, new_dqf_data_array

    @staticmethod
    def _downscale_1km_to_2km(rad_data_array, dqf_data_array):
        current_dim = len(rad_data_array)
        increment = 2
        new_dim = int(current_dim / increment)
        new_rad_data_array = [[0] * new_dim] * new_dim
        new_dqf_data_array = [[0] * new_dim] * new_dim

        k, l = 0, 0
        for i in range(0, current_dim, increment):
            for j in range(0, current_dim, increment):
                point_array = [(i, j), (i + 1, j),
                               (i, j + 1), (i + 1, j + 1)]
                mean_array = []
                for point in point_array:
                    dqf = dqf_data_array[point[0]][point[1]]
                    if dqf == 0 or dqf == 1:
                        mean_array.append(rad_data_array[point[0]][point[1]])
                if len(mean_array) != 0:
                    new_rad_data_array[k][l] = fmean(mean_array)
                    new_dqf_data_array[k][l] = 0
                else:
                    new_rad_data_array[k][l] = -999
                    new_dqf_data_array[k][l] = -1
                l += 1
                if l == new_dim:
                    l = 0
                    k += 1

        return new_rad_data_array, new_dqf_data_array

    @staticmethod
    def _downscale_05km_to_2km(rad_data_array, dqf_data_array):
        current_dim = len(rad_data_array)
        increment = 4
        new_dim = int(current_dim / increment)
        new_rad_data_array = [[0] * new_dim] * new_dim
        new_dqf_data_array = [[0] * new_dim] * new_dim

        k, l = 0, 0
        for i in range(0, current_dim, increment):
            for j in range(0, current_dim, increment):
                point_array = [(i, j), (i + 1, j), (i + 2, j), (i + 3, j),
                               (i, j + 1), (i + 1, j + 1), (i + 2, j + 1), (i + 3, j + 1),
                               (i, j + 2), (i + 1, j + 2), (i + 2, j + 2), (i + 3, j + 2),
                               (i, j + 3), (i + 1, j + 3), (i + 2, j + 3), (i + 3, j + 3)]
                mean_array = []
                for point in point_array:
                    dqf = dqf_data_array[point[0]][point[1]]
                    if dqf == 0 or dqf == 1:
                        mean_array.append(rad_data_array[point[0]][point[1]])
                if len(mean_array) != 0:
                    new_rad_data_array[k][l] = fmean(mean_array)
                    new_dqf_data_array[k][l] = 0
                else:
                    new_rad_data_array[k][l] = -999
                    new_dqf_data_array[k][l] = -1
                l += 1
                if l == new_dim:
                    l = 0
                    k += 1

        return new_rad_data_array, new_dqf_data_array

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

    def _filter_data_array_by_yaw_flip_flag(self, data_array):
        if not self._yaw_flip_flag:
            return data_array[::-1]
        else:
            return data_array

    def _create_obsvalue_rf_data_array(self):
        temp_data_array = self._rad_data_array * self._kappa0
        self._obsvalue_rf_data_array = numpy.where(self._dqf_data_array == -999, -999, temp_data_array)

    def _create_obsvalue_bt_data_array(self):
        log_comp = numpy.log((self._planck_fk1 / self._rad_data_array) + 1)
        temp_data_array = ((self._planck_fk2 / log_comp) - self._planck_bc1) / self._planck_bc2
        self._obsvalue_bt_data_array = numpy.where(self._dqf_data_array == -999, -999, temp_data_array)

    def _create_obserror_rf_data_array(self):
        sqrt_comp = numpy.power(self._kappa0, 2) * numpy.power(self._std_dev_radiance_value_of_valid_pixels, 2)
        temp_data_array = numpy.sqrt(sqrt_comp) / numpy.sqrt(self._valid_pixel_count)
        self._obserror_rf_data_array = numpy.where(self._dqf_data_array == -999, -999, temp_data_array)

    def _create_obserror_bt_data_array(self):
        sqrt_comp_1 = (-1.0 * self._planck_fk2) / \
                      (self._planck_bc2 * numpy.power(numpy.log((self._planck_fk1 / self._rad_data_array) + 1), 2))
        sqrt_comp_2 = 1 / (self._planck_fk1 + self._rad_data_array) - 1 / self._rad_data_array
        sqrt_comp = numpy.power(sqrt_comp_1 * sqrt_comp_2, 2) * \
                    numpy.power(self._std_dev_radiance_value_of_valid_pixels, 2)
        temp_data_array = numpy.sqrt(sqrt_comp) / numpy.sqrt(self._valid_pixel_count)
        self._obserror_bt_data_array = numpy.where(self._dqf_data_array == -999, -999, temp_data_array)

    def get_abi_channel(self):
        return self._metadata_dict['abi_channel']

    def get_input_file_path(self):
        return self._input_file_path

    def get_obsvalue_rf_data_array(self):
        return self._obsvalue_rf_data_array

    def get_obsvalue_bt_data_array(self):
        return self._obsvalue_bt_data_array

    def get_obserror_rf_data_array(self):
        return self._obserror_rf_data_array

    def get_obserror_bt_data_array(self):
        return self._obserror_bt_data_array

    def get_preqc_data_array(self):
        return self._dqf_data_array

    def close(self):
        self._input_dataset.close()

    def load(self):

        self._open()
        self._input_dataset.set_auto_scale(True)
        self._load_yaw_flip_flag_variable()
        self._load_kappa0_variable()
        self._load_planck_variables()
        self._load_std_dev_radiance_value_of_valid_pixels_variable()
        self._load_valid_pixel_count_variable()
        self._load_dqf_data_array()
        self._load_rad_data_array()

        if self._metadata_dict['abi_channel'] == 1 or \
                self._metadata_dict['abi_channel'] == 3 or \
                self._metadata_dict['abi_channel'] == 5:
            self._rad_data_array, self._dqf_data_array = Goes16._subsample(self._rad_data_array,
                                                                           self._dqf_data_array, 2)
        if self._metadata_dict['abi_channel'] == 2:
            self._rad_data_array, self._dqf_data_array = Goes16._subsample(self._rad_data_array,
                                                                           self._dqf_data_array, 4)

        shape = len(self._rad_data_array) * len(self._rad_data_array)

        self._dqf_data_array = numpy.array(self._dqf_data_array)
        self._dqf_data_array = self._dqf_data_array.reshape(shape)
        self._dqf_data_array = self._filter_data_array_by_yaw_flip_flag(self._dqf_data_array)
        self._dqf_data_array = numpy.where(self._dqf_data_array == 255, -999, self._dqf_data_array)

        self._rad_data_array = numpy.array(self._rad_data_array)
        self._rad_data_array = self._rad_data_array.reshape(shape)
        self._rad_data_array = self._filter_data_array_by_yaw_flip_flag(self._rad_data_array)
        self._rad_data_array = numpy.where(self._dqf_data_array == -999, -999, self._rad_data_array)

        if self._metadata_dict['abi_channel'] < 7:
            self._create_obsvalue_rf_data_array()
            self._create_obserror_rf_data_array()
        else:
            self._create_obsvalue_bt_data_array()
            self._create_obserror_bt_data_array()

        self.close()


class ABIMode(Enum):
    ABI_SCAN_MODE_4 = 1
    ABI_SCAN_MODE_6 = 2


class ABISectorType(Enum):
    FULL_DISK = 1
    CONUS = 2
    MESOSCALE_REGION_1 = 3
    MESOSCALE_REGION_2 = 4
