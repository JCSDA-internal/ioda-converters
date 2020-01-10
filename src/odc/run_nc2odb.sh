#!/usr/bin/env bash
#
# This script will walk though a given top-level directory, and convert all netcdf
# files to odb equivalents. The new files will be placed alongside the original
# netcdf files.

# Check for clobber option (overwrite existing file)
NcToOdbOpts=""
BadArgs="F"
while getopts "c" option
do
  case "${option}" in
    c)
      NcToOdbOpts="${NcToOdbOpts} -c"
      ;;
    :)
      echo "Invalid option: $OPTARG requires an argument" 1>&2
      ;;
    ?)
      BadArgs="T"
      ;;
  esac
done
shift "$(($OPTIND - 1))"

if [[ "${BadArgs}" == "T" ]]
then
  echo "USAGE: $(basename ${0}) [-c] <list_of_directories>" 1>&2
  exit 1
fi

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
      nc2odb.py ${NcToOdbOpts} ${NcFile} ${OdbFile}
      echo
    done
  fi
done
