#!/bin/bash

# use nccmp or odc to compare the output of a ioda-converter
#
# argument 1: what type of file to compare; netcdf or odb
# argument 2: the command to run the ioda converter
# argument 3: testrun filename to test
# argument 4: testoutput filename to compare
set -eu

file_type=$1
cmd=$2
run_file_name=$3
output_file_name=$4
tol=${5:-"0.0"}
verbose=${6:-${VERBOSE:-"N"}}

[[ $verbose == [YyTt] || \
   $verbose == [Yy][Ee][Ss] || \
   $verbose == [Tt][Rr][Uu][Ee] ]] && set -x

rc="-1"
case $file_type in
  netcdf)
    $cmd && \
    nccmp testrun/$run_file_name testoutput/$output_file_name -d -m -g -f -S -T ${tol}
    rc=${?}
    ;;
   odb)
    $cmd && \
    odc compare testrun/$run_file_name testoutput/$output_file_name
    rc=${?}
    ;;
  ascii)
    $cmd && \
    diff testrun/$run_file_name testoutput/$output_file_name
    rc=${?}
    ;;
   *)
    echo "ERROR: iodaconv_comp.sh: Unrecognized file type: ${file_type}"
    rc="-2"
    ;;
esac

exit $rc
