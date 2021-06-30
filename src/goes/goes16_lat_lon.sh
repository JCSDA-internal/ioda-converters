#!/bin/bash

INPUT_FILE_PATH=/Users/eric2/PROJECTS/GOES16_TEST_FILES/input/00/OR_ABI-L1b-RadF-M6C01_G16_s20211320000191_e20211320009499_c20211320009539.nc
OUTPUT_FILE_PATH=/Users/eric2/PROJECTS/GOES16_TEST_FILES/latlon/goes16_lat_lon.nc

python goes16_lat_lon.py --overwrite $INPUT_FILE_PATH $OUTPUT_FILE_PATH
