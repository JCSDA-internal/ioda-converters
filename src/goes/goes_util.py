import numpy as np


#
# goes_util.py
#
# This class contains methods for sub-sampling data arrays and filtering data arrays. The
# sub-sampling techniques used in this class are derived from section 3.4.3 in the
# "GOES-R Advanced Baseline Imager (ABI) Algorithm Theoretical Basis Document For Cloud
# and Moisture Imagery Product (CMIP)" Version 3.0 July 30, 2012
# (https://www.star.nesdis.noaa.gov/goesr/docs/ATBD/Imagery.pdf).
#
class GoesUtil:

    def __init__(self):
        """
        Constructor
        """
        self._increment = None
        self._yaw_flip_flag = None
        self._resolution = None
        self._fill_value_index_array = None

    def set_yaw_flip_flag(self, yaw_flip_flag):
        """
        Sets the yaw_flip_flag variable
        yaw_flip_flag - the yaw_flip_flag
        """
        self._yaw_flip_flag = yaw_flip_flag

    def set_resolution(self, resolution):
        """
        Sets the resolution variable and array slicing increment for sub_sampling
        resolution - the resolution
        """
        self._resolution = resolution
        self._increment = int(self._resolution / 2)

    def set_fill_value_index_array(self, fill_value_index_array):
        """
        Sets the fill_value_index_array
        fill_value_index_array - an array of indices to remove
        """
        self._fill_value_index_array = fill_value_index_array

    def subsample_1d(self, data_array):
        """
        Returns a data array that has been sub-sampled by increment. This method uses python array slicing and is the
        default method.
        data_array - a one dimensional data array
        """
        if self._increment == 1:
            return data_array
        current_dim = len(data_array)
        data_array = np.asarray(data_array)
        data_array = data_array[0:current_dim:self._increment]
        return data_array

    def subsample_2d(self, data_array):
        """
        Returns a data array that has been sub-sampled by increment. This method uses python array slicing and is the
        default method.
        data_array - a two dimensional data array
        """
        if self._increment == 1:
            return data_array
        current_dim = len(data_array)
        data_array = np.asarray(data_array)
        data_array = data_array[0:current_dim:self._increment, 0:current_dim:self._increment]
        return data_array

    @staticmethod
    def subsample_1d_inc(data_array, increment):
        """
        Returns a data array that has been sub-sampled by increment. This method uses python array slicing and is the
        default method.
        data_array - a one dimensional data array
        increment - the increment used in the python array slicing
        """
        if increment == 1:
            return data_array
        current_dim = len(data_array)
        data_array = np.asarray(data_array)
        data_array = data_array[0:current_dim:increment]
        return data_array

    @staticmethod
    def subsample_2d_inc(data_array, increment):
        """
        Returns a data array that has been sub-sampled by increment. This method uses python array slicing and is the
        default method.
        data_array - a two dimensional data array
        increment - the increment used in the python array slicing
        """
        if increment == 1:
            return data_array
        current_dim = len(data_array)
        data_array = np.asarray(data_array)
        data_array = data_array[0:current_dim:increment, 0:current_dim:increment]
        return data_array

    def filter_data_array_by_yaw_flip_flag(self, data_array):
        """
        Returns data_array filtered by the yaw_flip_flag.
        data_array - the data array to filter
        """
        if self._yaw_flip_flag == 2:
            return data_array[::-1]
        else:
            return data_array

    def filter_data_array_by_fill_value(self, data_array):
        """
        Returns a data array filtered by bad latitude indices.
        data_array - a one dimensional data array
        """
        return np.delete(data_array, self._fill_value_index_array)
