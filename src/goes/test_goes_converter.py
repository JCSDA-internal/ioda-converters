#
# test_goes_converter.py
#
# This script is a test driver for the GoesConverter class.
#
#
import time
from goes_converter import GoesConverter


def test_goes_converter(input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt):
    """
    Test driver for the GoesConverter class.
    input_file_paths - a list containing 16 absolute paths corresponding to the output files for each channel of a
        single GOERS_16 or GOES-17 scan
    latlon_file_path - The path to an existing Goes LatLon file or if it does not exist the path to write the file
    output_file_path_rf - The path to write the IODAv2 reflectance factor data file
    output_file_path_bt - The path to write the IODAv2 brightness temperature data file
    """
    goes_converter = GoesConverter(input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt)
    goes_converter.convert()


if __name__ == '__main__':
    start_time = time.time()
    test_goes_converter()
    elapsed_time = time.time() - start_time
    print(f'elapsed time:{elapsed_time:.3g}s')
