#!/bin/bash

# use nccmp to compare the output of a ioda-converter
#
# argument 1: the command to run the ioda converter
# argument 2: the filename to test

export LD_LIBRARY_PATH=${CTEST_LIBRARY_PATH}

$1 && \
nccmp testrun/$2 testoutput/$2 -d -m -g -f -S
