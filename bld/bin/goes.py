#
# goes.py
#
# This class loads, calculates, filters, and makes accessible the variables and attributes required by the
# GoesConverter class for a single GOES-16 or GOES-17 LB1 ABI channel (1-16). The brightness temperature and reflectance
# factor calculations used in this class are derived from sections 3.4.1.2 and 3.4.1.3 in the
# "GOES-R Advanced Baseline Imager (ABI) Algorithm Theoretical Basis Document For Cloud and Moisture Imagery Product
# (CMIP)" Version 3.0 July 30, 2012 (https://www.star.nesdis.noaa.gov/goesr/docs/ATBD/Imagery.pdf). The calculations for
# the propagation of standard error are from section 2.5.5 of the "NIST/SEMATECH e-Handbook of Statistical Methods"
# (https://www.itl.nist.gov/div898/handbook/mpc/section5/mpc55.htm). GOES ABI channels 1, 3, and 5 are sub-sampled from
# 1km to 2km resolution and ABI channel 2 is sub-sampled from 0.5km to 2km resolution prior to sub-sampling to the
# requested resolution using methods in this class.
#
import os
import datetime
from enum import Enum
import numpy as np
from netCDF4 import Dataset
from numpy import ma
from goes_util import GoesUtil


class Goes:

    def __init__(self, input_file_path, goes_util: GoesUtil):
        """
        Constructor
        input_file_path - a GOES-16 or GOES-17 raw data file for a single ABI channel
        """
        self._input_file_path = input_file_path
        self._goes_util = goes_util
        self._get_metadata_from_input_file_path()
        self._rad_data_array = None
        self._dqf_data_array = None
        self._lat_fill_value_index_array = None
        self._obsvalue_rf_data_array = None
        self._obsvalue_bt_data_array = None
        self._obserror_rf_data_array = None
        self._obserror_bt_data_array = None

    def _get_metadata_from_input_file_path(self):
        """
        Creates a dictionary of file metadata from input_file_path
        """
        self._metadata_dict = {'instrument': 'ABI',
                               'processing_level': 'L1b',
                               'product_acronym': 'Rad'}
        metadata_array = os.path.splitext(os.path.basename(self._input_file_path))[0].split('_')
        self._metadata_dict['system_environment'] = metadata_array[0]
        self._metadata_dict['abi_sector_type'] = Goes._string_to_abisectortype(metadata_array[1])
        self._metadata_dict['abi_mode'] = Goes._string_to_abimode(metadata_array[1])
        self._metadata_dict['abi_channel'] = int(metadata_array[1][-2:])
        self._metadata_dict['platform_identifier'] = metadata_array[2]
        year = int(metadata_array[3][1:-1][0:4])
        day_of_year = int(metadata_array[3][1:-1][4:7])
        hour = int(metadata_array[3][1:-1][7:9])
        minute = int(metadata_array[3][1:-1][9:11])
        second = int(metadata_array[3][1:-1][11:13])
        scan_start_datetime = datetime.datetime(year, 1, 1, hour, minute, second) + datetime.timedelta(day_of_year - 1)
        self._metadata_dict['day_of_year'] = day_of_year
        self._metadata_dict['start_date'] = scan_start_datetime

    def _open(self):
        """
        Opens a netCDF4 dataset using input_file_path.
        """
        self._input_dataset = Dataset(self._input_file_path, 'r')

    def _load_kappa0_variable(self):
        """
        Creates a local kappa0 variable.
        """
        self._kappa0 = ma.getdata(self._input_dataset.variables['kappa0'][0])

    def _load_planck_variables(self):
        """
        Creates a local variables for the four Planck constants.
        """
        self._planck_bc1 = self._input_dataset.variables['planck_bc1'][0]
        self._planck_bc2 = self._input_dataset.variables['planck_bc2'][0]
        self._planck_fk1 = self._input_dataset.variables['planck_fk1'][0]
        self._planck_fk2 = self._input_dataset.variables['planck_fk2'][0]

    def _load_std_dev_radiance_value_of_valid_pixels_variable(self):
        """
        Creates a local variable for the standard deviation of radiance for only valid pixels.
        """
        self._std_dev_radiance_value_of_valid_pixels = \
            self._input_dataset.variables['std_dev_radiance_value_of_valid_pixels'][0]

    def _load_valid_pixel_count_variable(self):
        """
        Creates a local variable of valid pixel counts.
        """
        self._valid_pixel_count = self._input_dataset.variables['valid_pixel_count'][0]

    def _load_dqf_data_array(self):
        """
        Creates a local data array for the DQF variable.
        """
        self._dqf_data_array = ma.getdata(self._input_dataset.variables['DQF'][:].real)

    def _load_rad_data_array(self):
        """
        Creates a local data array for the Rad variable.
        """
        self._rad_data_array = ma.getdata(self._input_dataset.variables['Rad'][:].real)

    @staticmethod
    def _string_to_abimode(string):
        """
        Selects the ABI Mode Enum constant from string.
        string - the string used for the match
        """
        if 'M4' in string:
            return ABIMode.ABI_SCAN_MODE_4
        if 'M6' in string:
            return ABIMode.ABI_SCAN_MODE_6

    @staticmethod
    def _string_to_abisectortype(string):
        """
        Selects the ABI Sector Type constant from string.
        string - the string used for the match
        """
        if 'F' in string:
            return ABISectorType.FULL_DISK
        if 'C' in string:
            return ABISectorType.CONUS
        if 'M1' in string:
            return ABISectorType.MESOSCALE_REGION_1
        if 'M2' in string:
            return ABISectorType.MESOSCALE_REGION_2

    def _create_obsvalue_rf_data_array(self):
        """
        Creates a local data array variable containing the calculated obsvalue reflectance factor data
        after fill value filtering by the DQF flags.
        """
        self._obsvalue_rf_data_array = self._rad_data_array * self._kappa0

    def _create_obsvalue_bt_data_array(self):
        """
        Creates a local data array variable containing the calculated obsvalue brightness temperature data
        after fill value filtering by the DQF flags.
        """
        log_comp = np.log((self._planck_fk1 / self._rad_data_array) + 1)
        self._obsvalue_bt_data_array = ((self._planck_fk2 / log_comp) - self._planck_bc1) / self._planck_bc2

    def _create_obserror_rf_data_array(self):
        """
        Creates a local data array variable containing the calculated obserror reflectance factor data
        after fill value filtering by the DQF flags.
        """
        sqrt_comp = np.power(self._kappa0, 2) * np.power(self._std_dev_radiance_value_of_valid_pixels, 2)
        temp_data = np.sqrt(sqrt_comp) / np.sqrt(self._valid_pixel_count)
        self._obserror_rf_data_array = np.full(len(self._rad_data_array), temp_data)

    def _create_obserror_bt_data_array(self):
        """
        Creates a local data array variable containing the calculated obserror brightness temperature data
        after fill value filtering by the DQF flags.
        """
        sqrt_comp_1 = (-1.0 * self._planck_fk2) / \
                      (self._planck_bc2 * np.power(np.log((self._planck_fk1 / self._rad_data_array) + 1), 2))
        sqrt_comp_2 = 1 / (self._planck_fk1 + self._rad_data_array) - 1 / self._rad_data_array
        sqrt_comp = np.power(sqrt_comp_1 * sqrt_comp_2, 2) * np.power(self._std_dev_radiance_value_of_valid_pixels, 2)
        self._obserror_bt_data_array = np.sqrt(sqrt_comp) / np.sqrt(self._valid_pixel_count)

    def get_abi_channel(self):
        """
        Returns the ABI channel.
        """
        return self._metadata_dict['abi_channel']

    def get_platform_identifier(self):
        """
        Returns the platform identifier.
        """
        return self._metadata_dict['platform_identifier']

    def get_start_date(self):
        """
        Returns the scan's start date as a datetime object
        """
        return self._metadata_dict['start_date']

    def get_day_of_year(self):
        """
        Returns the scan's day of year.
        """
        return self._metadata_dict['day_of_year']

    def get_input_file_path(self):
        """
        Returns the input_file_path.
        """
        return self._input_file_path

    def get_obsvalue_rf_data_array(self):
        """
        Returns the obsvalue reflectance factor data array.
        """
        return self._obsvalue_rf_data_array

    def get_obsvalue_bt_data_array(self):
        """
        Returns the obsvalue brightness temperature data array.
        """
        return self._obsvalue_bt_data_array

    def get_obserror_rf_data_array(self):
        """
        Returns the obserror reflectance factor data array.
        """
        return self._obserror_rf_data_array

    def get_obserror_bt_data_array(self):
        """
        Returns the obserror brightness temperature data array.
        """
        return self._obserror_bt_data_array

    def get_preqc_data_array(self):
        """
        Returns the preqc data array.
        """
        return self._dqf_data_array

    def close(self):
        """
        Closes this netCDF4 Dataset.
        """
        self._input_dataset.close()

    def set_lat_fill_value_index_array(self, lat_fill_value_index_array):
        """
        Sets the self._lat_fill_value_index_array variable
        lat_fill_value_index_array - array of bad latitude coordinate indices
        """
        self._lat_fill_value_index_array = lat_fill_value_index_array

    def load(self):
        """
        Loads, calculates, sub-samples, reshapes, and filters all data arrays required by the GoesConverter class.
        """
        self._open()
        self._input_dataset.set_auto_scale(True)
        self._load_kappa0_variable()
        self._load_planck_variables()
        self._load_std_dev_radiance_value_of_valid_pixels_variable()
        self._load_valid_pixel_count_variable()
        self._load_dqf_data_array()
        self._load_rad_data_array()

        if self._metadata_dict['abi_channel'] == 1 or \
                self._metadata_dict['abi_channel'] == 3 or \
                self._metadata_dict['abi_channel'] == 5:
            self._dqf_data_array = GoesUtil.subsample_2d_inc(self._dqf_data_array, 2)
            self._rad_data_array = GoesUtil.subsample_2d_inc(self._rad_data_array, 2)
        if self._metadata_dict['abi_channel'] == 2:
            self._dqf_data_array = GoesUtil.subsample_2d_inc(self._dqf_data_array, 4)
            self._rad_data_array = GoesUtil.subsample_2d_inc(self._rad_data_array, 4)

        self._dqf_data_array = self._goes_util.subsample_2d(self._dqf_data_array)
        self._rad_data_array = self._goes_util.subsample_2d(self._rad_data_array)

        shape = len(self._rad_data_array) * len(self._rad_data_array)

        self._dqf_data_array = np.array(self._dqf_data_array)
        self._dqf_data_array = self._dqf_data_array.reshape(shape)
        self._dqf_data_array = self._goes_util.filter_data_array_by_yaw_flip_flag(self._dqf_data_array)
        self._dqf_data_array = np.where(self._dqf_data_array == 255, -999, self._dqf_data_array)
        self._dqf_data_array = self._goes_util.filter_data_array_by_nonexistent_indices(self._dqf_data_array)

        self._rad_data_array = np.array(self._rad_data_array)
        self._rad_data_array = self._rad_data_array.reshape(shape)
        self._rad_data_array = self._goes_util.filter_data_array_by_yaw_flip_flag(self._rad_data_array)
        self._rad_data_array = self._goes_util.filter_data_array_by_nonexistent_indices(self._rad_data_array)

        if self._metadata_dict['abi_channel'] < 7:
            self._create_obsvalue_rf_data_array()
            self._create_obserror_rf_data_array()
        else:
            self._create_obsvalue_bt_data_array()
            self._create_obserror_bt_data_array()

        self.close()


#
# This enumeration is for the ABI Mode.
#
class ABIMode(Enum):
    ABI_SCAN_MODE_4 = 1
    ABI_SCAN_MODE_6 = 2


#
# This enumeration is for the ABI Sector Type
#
class ABISectorType(Enum):
    FULL_DISK = 1
    CONUS = 2
    MESOSCALE_REGION_1 = 3
    MESOSCALE_REGION_2 = 4
