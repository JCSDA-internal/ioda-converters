#!/usr/bin/env python

import argparse
import os
import sys

from netCDF4 import Dataset
from solo.basic_files import tree
from goes16 import Goes16


class Goes16Converter:

    def __init__(self, args):
        self._input_files_path = args.input_files_path
        self._latlon_file_path = args.latlon_file_path
        self._output_file_path = args.output_file_path
        self._check_arguments()
        self._create_input_data_file_dict()
        self._latlon_dataset = Dataset(self._latlon_file_path, 'r')

    def _create_input_data_file_dict(self):
        self._input_data_file_dict = {}
        for path, root, input_file_name in tree(self._input_files_path):
            input_file_path = os.path.join(path, root, input_file_name)
            output_file_name = f'{os.path.splitext(input_file_name)[0]}_iodav2{os.path.splitext(input_file_name)[1]}'
            output_file_path = os.path.join(self._output_file_path, output_file_name)
            data_file = Goes16(input_file_path, output_file_path)
            self._input_data_file_dict[data_file.get_abi_channel()] = data_file

    def _check_arguments(self):
        good_args = True
        if not os.path.exists(self._input_files_path):
            print("ERROR: Input GOES16 files do not exist: " + self._input_files_path)
            good_args = False
        if not good_args:
            sys.exit(2)

    def _close_datasets(self):
        for input_data_file in self._input_data_file_dict:
            input_data_file.close_datasets()

    def convert_goes16(self):
        latitude_data_array = self._latlon_dataset['MetaData'].variables['latitude'][:].real
        longitude_data_array = self._latlon_dataset['MetaData'].variables['longitude'][:].real
        for input_data_file in self._input_data_file_dict.values():
            input_data_file.set_lat_lon_arrays(latitude_data_array, longitude_data_array)
            input_data_file.convert()
        self._close_datasets()


def parse_arguments():
    ap = argparse.ArgumentParser()
    ap.add_argument("input_files_path", type=str, help="Path to Input GOES16 Files")
    ap.add_argument("latlon_file_path", type=str, help="Path to Input GOES16 Lat/Lon File")
    ap.add_argument("output_file_path", type=str, help="Path to Output IODAv2 File")
    return ap.parse_args()


if __name__ == '__main__':
    args = parse_arguments()
    goes16Converter = Goes16Converter(args)
    goes16Converter.convert_goes16()
