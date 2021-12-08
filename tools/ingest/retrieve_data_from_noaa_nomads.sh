#!/bin/bash

##################################################################
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
##################################################################

nomads_address='https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod'

usage='usage bash $0 yyyymmdd [end_yyyymmdd]'
example=='example:  bash $0 2021091500 [2021100100]'

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
    # retrieve the files
    gfile=${nomads_address}/gfs.${dtg:0:8}/${dtg:8:2}/atmos/gfs.t${dtg:8:2}z.${atype}.tm00.bufr_d
    nfile=${nomads_address}/gfs.${dtg:0:8}/${dtg:8:2}/atmos/gfs.t${dtg:8:2}z.${atype}.tm00.bufr_d.nr
    out_file=${gfile##*/}
    # optional rename
    #out_file="${out_file%.bufr_d*}.bfr"
    #echo "wget ${gfile} -O ${out_file}"
    wget -nc ${gfile} -O ${out_file}
    if [[ -e ${out_file} && ! -s ${out_file} ]]; then
        rm ${out_file}
        out_file=${nfile##*/}
        wget -nc ${nfile} -O ${out_file}
        [[ ! -s ${out_file} ]]  && rm -f ${out_file}
    fi
}

for atype in ${data_types[@]}; do
    get_files
done
