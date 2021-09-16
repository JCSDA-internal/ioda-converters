import numpy as np


#
# goes_util.py
#
# This class ...
#
class GoesUtil:

    def __init__(self, yaw_flip_flag, resolution):
        """
        Constructor
        yaw_flip_flag - the yaw_flip_flag attribute value
        resolution - the resolution in km for subsampling: 2 (default), 4, 8, 16, 32, 64, 128, 256
        """
        self._yaw_flip_flag = yaw_flip_flag
        self._resolution = resolution
        self._increment = int(self._resolution / 2)

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
        Returns data_array after filtering by the yaw_flip_flag.
        data_array - the data array to filter
        """
        if self._yaw_flip_flag == 2:
            return data_array[::-1]
        else:
            return data_array
