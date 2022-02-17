#!/bin/bash

################################################################################
# (C) Copyright 2018-2020 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
################################################################################
# 
#  PURPOSE:
#       retrieve BUFR data from NOAA NOMADS server
#         optionally provide a data type
#         will default to AMSU-A, MHS, GNSS-RO and IASI
#
#      NOTE: NOMADS keeps ~10days of data online
#
#   REQUIREMENTS:
#       wget
#
################################################################################

nomads_address='https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod'

usage="usage bash ${0} yyyymmddhh [user_input_data_type]"
example="example:  bash ${0} 2021091500 [adpupa]"

if (( ${#@} == 0 ))  ||  (( ${#@} > 2 )) || [[ $1 == [hH]elp ]]; then
    echo "${usage}"
    echo "${example}"
    exit 1
elif (( ${#1} != 10 )); then
    echo "unrecognized year-month-day: ${1}"
    echo "${usage}"
    echo "${example}"
    exit 1
fi


# set date-time group to ingest data
dtg=${1}

# files to ingest either default or user specified
if (( ${#@} == 2 )); then
    data_types=(
        ${2}
        )
else
    data_types=(
        1bamua
        1bmhs
        esamua
        esmhs
        gpsro
        mtiasi
        )
fi

get_files() {

    # the gdas is the late cut and gfs is early
    for data_cut in gdas gfs; do
        # retrieve the files
        gfile=${nomads_address}/${data_cut}.${dtg:0:8}/${dtg:8:2}/atmos/${data_cut}.t${dtg:8:2}z.${atype}.tm00.bufr_d
        nfile=${nomads_address}/${data_cut}.${dtg:0:8}/${dtg:8:2}/atmos/${data_cut}.t${dtg:8:2}z.${atype}.tm00.bufr_d.nr
        # check both "normal" name and one with restricted data stripped ( appended with "nr" )
        for afile in ${gfile} ${nfile}; do
            out_file=${afile##*/}
            # optional rename
            # out_file="${out_file%.bufr_d*}.bfr"
            # does the file exist on the server
            if curl --output /dev/null --silent --head --fail "${afile}"; then
                wget -nc ${afile} -O ${out_file}
                echo " ... wget exit status: ${?}"
                # remove if file is zero length
                if [[ -e ${out_file} && ! -s ${out_file} ]]; then
                    rm ${out_file}
                fi
            else
                echo "  .. file does not exist on server: ${afile}"
            fi
        done
    done
}

for atype in ${data_types[@]}; do
    get_files
done
