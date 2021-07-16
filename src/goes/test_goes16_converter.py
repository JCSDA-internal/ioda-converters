#
# test_goes16_converter.py
#
# This script is a driver for the Goes16Converter class.
#
#
import os
import time

from solo.basic_files import tree
from goes16_converter import Goes16Converter


def test_goes16_converter():
    input_files_path = "/work/noaa/da/eric/GOES16/ABI-L1b-RadF/2021/132/00"
    latlon_file_path = "/work/noaa/da/eric/GOES16/IODAv2/LATLON/goes16_latlon_2km.nc"
    output_file_path_rf = "/work/noaa/da/eric/GOES16/IODAv2/OUTPUT/goes16_rf.nc"
    output_file_path_bt = "/work/noaa/da/eric/GOES16/IODAv2/OUTPUT/goes16_bt.nc"

    input_file_paths = []
    for path, root, filename in tree(input_files_path):
        if not filename.startswith("."):
            filepath = os.path.join(path, root, filename)
            input_file_paths.append(filepath)
    goes16_converter = Goes16Converter(input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt)
    goes16_converter.convert()


if __name__ == '__main__':
    start_time = time.time()
    test_goes16_converter()
    elapsed_time = time.time() - start_time
    print(f'elapsed time:{elapsed_time:.3g}s')
