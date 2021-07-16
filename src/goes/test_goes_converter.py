#
# test_goes_converter.py
#
# This script is a driver for the GoesConverter class.
#
#
import os
import time

from solo.basic_files import tree
from goes_converter import GoesConverter


def test_goes_converter():
    input_files_path = "/Users/eric2/PROJECTS/GOES16_TEST_FILES/input/12_15_2020/00"
    latlon_file_path = "/Users/eric2/PROJECTS/GOES16_TEST_FILES/latlon/goes16_latlon_2km.nc"
    output_file_path_rf = "/Users/eric2/PROJECTS/GOES16_TEST_FILES/output/12_15_2020/00/goes16_rf.nc"
    output_file_path_bt = "/Users/eric2/PROJECTS/GOES16_TEST_FILES/output/12_15_2020/00/goes16_bt.nc"

    input_file_paths = []
    for path, root, filename in tree(input_files_path):
        if not filename.startswith("."):
            filepath = os.path.join(path, root, filename)
            input_file_paths.append(filepath)
    goes_converter = GoesConverter(input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt)
    goes_converter.convert()


if __name__ == '__main__':
    start_time = time.time()
    test_goes_converter()
    elapsed_time = time.time() - start_time
    print(f'elapsed time:{elapsed_time:.3g}s')
