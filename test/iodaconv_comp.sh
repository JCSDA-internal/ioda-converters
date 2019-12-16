#!/bin/bash

# use nccmp or odc to compare the output of a ioda-converter
#
# argument 1: the command to run the ioda converter
# argument 2: the filename to test

export LD_LIBRARY_PATH=${CTEST_LIBRARY_PATH}:${LD_LIBRARY_PATH}

file_type=$1
cmd=$2
file_name=$3

case $file_type in
  netcdf)
    $cmd && \
    nccmp testrun/$file_name testoutput/$file_name -d -m -g -f -S
    ;;
   odb)
    $cmd && \
    odc compare testrun/$file_name testoutput/$file_name
    ;;
esac
