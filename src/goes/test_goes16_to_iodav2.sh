#!/bin/bash

INPUT_FILE_PATH=/Users/eric2/PROJECTS/IODA_CONVERTERS/ioda-converters/src/goes16/OR_ABI-L1b-RadF-M6C01_G16_s20211320000191_e20211320009499_c20211320009539.nc
LATLON_FILE_PATH=/Users/eric2/PROJECTS/IODA_CONVERTERS/ioda-converters/src/goes16/goes16_lat_lon_output.nc
OUTPUT_FILE_PATH=/Users/eric2/PROJECTS/IODA_CONVERTERS/ioda-converters/src/goes16/output.nc

python goes16_to_iodav2.py --overwrite $INPUT_FILE_PATH $LATLON_FILE_PATH $OUTPUT_FILE_PATH
