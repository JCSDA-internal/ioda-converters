#!/bin/bash

# use nccmp or odc to compare the output of a ioda-converter
#
# argument 1: the command to run the ioda converter
# argument 2: the filename to test

export LD_LIBRARY_PATH=${CTEST_LIBRARY_PATH}:${LD_LIBRARY_PATH}

file_type=$1
cmd=$2
file_name=$3

rc="-1"
case $file_type in
  netcdf)
    $cmd && \
    nccmp testrun/$file_name testoutput/$file_name -d -m -g -f -S
    rc=${?}
    ;;
   odb)
    $cmd && \
    odc compare testrun/$file_name testoutput/$file_name
    rc=${?}
    ;;
   *)
    echo "ERROR: iodaconv_comp.sh: Unrecognized file type: ${file_type}"
    rc="-2"
    ;;
esac

exit $rc
