#!/usr/bin/env bash
#
# This script will walk though a given top-level directory, and convert all netcdf
# files to odb equivalents. The new files will be placed alongside the original
# netcdf files.

# Accept a list of directories, which will be processed sequentially.
for TopDir in ${@}
do
  if [[ -d ${TopDir} ]]
  then
    echo "Converting all netcdf files in directory: ${TopDir}"
    echo

    # Find all netcdf files (identified by .nc4 or .nc suffixes) and convert them to ODB.
    for NcFile in $(find ${TopDir} | grep -e "\.nc4$" -e "\.nc$")
    do
      echo "  *********** ${NcFile} ***********"
      echo
      OdbFile=$(echo ${NcFile} | sed -e's/\.nc[4]*$/\.odb/')
      nc2odb.py ${NcFile} ${OdbFile}
      echo
    done
  fi
done
