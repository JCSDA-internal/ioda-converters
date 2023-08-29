#!/bin/bash

# use nccmp or odc to compare the output of a ioda-converter
#
# argument 1: what type of file to compare; netcdf or odb
# argument 2: the command to run the ioda converter
# argument 3: the filename to test

set -eu

file_type=$1
cmd=$2
file_name=$3
tol=${4:-"0.0"}
verbose=${5:-${VERBOSE:-"N"}}

[[ $verbose == [YyTt] || \
   $verbose == [Yy][Ee][Ss] || \
   $verbose == [Tt][Rr][Uu][Ee] ]] && set -x

rc="-1"
case $file_type in
  netcdf)
    $cmd && \
    nccmp testrun/$file_name testoutput/$file_name -d -m -g -f -S -T ${tol}
    rc=${?}
    ;;
   odb)
    $cmd && \
    odc compare testrun/$file_name testoutput/$file_name
    rc=${?}
    ;;
  ascii)
    $cmd && \
    diff testrun/$file_name testoutput/$file_name
    rc=${?}
    ;;
   *)
    echo "ERROR: iodaconv_comp.sh: Unrecognized file type: ${file_type}"
    rc="-2"
    ;;
esac

exit $rc
