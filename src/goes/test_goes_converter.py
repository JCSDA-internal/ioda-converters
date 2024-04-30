#
# test_goes_converter.py
#
# This script is a test driver for the GoesConverter class.
#
#
import sys
import time
from goes_converter import GoesConverter


def test_goes_converter(input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt,
                        include_rf, resolution):

    goes_converter = \
        GoesConverter(input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt,
                      include_rf=include_rf, resolution=resolution)
    goes_converter.convert()


if __name__ == '__main__':
    """
    Test driver for the GoesConverter class.
    All filenames in argument list MUST be absolute paths, not relative.

    The 1st argument is a comma-separated list of 16 incoming ABI channel files (absolute paths).
    The 2nd argument is the reference lat/lon file (absolute path).  If already existing, it is
        cross-checked for the same nadir longitude in the reference file compared to the files
        being processed. If this file does not exist, it gets created for usage thereafter.
    The 3rd argument is the output reflectance (first 6 channels) data file (absolute path).
    The 4th argument is the output brightness temperature (channels 7-16) data file (absolute path).
    The 5th argument is the target resolution (2, 4, 8, 16, 32, or 64 km).
    The 6th argument should be True to output reflectances or False to skip, however argument 3
        must be provided regardless.
    """

    start_time = time.time()
    input_file_paths = sys.argv[1].split(',')
    latlon_file_path = sys.argv[2]
    output_file_path_rf = sys.argv[3]
    output_file_path_bt = sys.argv[4]
    resolution = sys.argv[5]
    include_rf = sys.argv[6]

    test_goes_converter(input_file_paths, latlon_file_path, output_file_path_rf, output_file_path_bt,
                        include_rf, resolution)
    elapsed_time = time.time() - start_time
    print(f'elapsed time:{elapsed_time:.3g}s')
