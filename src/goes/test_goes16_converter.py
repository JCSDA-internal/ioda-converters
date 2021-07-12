import os
from solo.basic_files import tree
from goes16_converter import Goes16Converter


def test_goes16_converter():
    input_files_path = "/Users/eric2/PROJECTS/GOES16_TEST_FILES/input/12_15_2020/00"
    latlon_file_path = "/Users/eric2/PROJECTS/GOES16_TEST_FILES/latlon/goes16_latlon_2km.nc"
    output_file_path_rf = "/Users/eric2/PROJECTS/GOES16_TEST_FILES/output/12_15_2020/00/goes16_rf.nc"
    output_file_path_bt = "/Users/eric2/PROJECTS/GOES16_TEST_FILES/output/12_15_2020/00/goes16_bt.nc"

    input_file_paths = []
    for path, root, filename in tree(input_files_path):
        if not filename.startswith("."):
            filepath = os.path.join(path, root, filename)
            input_file_paths.append(filepath)
    goes16_converter = Goes16Converter
    goes16_converter.convert(input_files_path, latlon_file_path, output_file_path_rf, output_file_path_bt)


if __name__ == '__main__':
    test_goes16_converter()