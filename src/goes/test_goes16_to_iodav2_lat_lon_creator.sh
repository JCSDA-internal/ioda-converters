#!/bin/bash

INPUT_FILE_PATH=OR_ABI-L1b-RadF-M6C01_G16_s20211320000191_e20211320009499_c20211320009539.nc
OUTPUT_FILE_PATH=goes16_lat_lon_output.nc

python goes16_to_iodav2_lat_lon_creator.py --overwrite $INPUT_FILE_PATH $OUTPUT_FILE_PATH
