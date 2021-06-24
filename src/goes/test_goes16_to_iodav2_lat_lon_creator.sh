#!/bin/bash

INPUT_FILE_PATH=/Users/eric2/PROJECTS/GOES16_TEST_FILES/test_files/OR_ABI-L1b-RadF-M6C01_G16_s20211320000191_e20211320009499_c20211320009539.nc
OUTPUT_FILE_PATH=/Users/eric2/PROJECTS/GOES16_TEST_FILES/test_files/goes16_lat_lon_output.nc

python goes16_to_iodav2_lat_lon_creator.py --overwrite $INPUT_FILE_PATH $OUTPUT_FILE_PATH
