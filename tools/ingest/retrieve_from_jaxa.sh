#!/bin/sh

if (( ${#@} == 0 ))  ||  (( ${#@} > 2 )) || [[ $1 == [hH]elp ]]; then
    echo "usage bash $0 yyyymmddhh [end_yyyymmddhh]"
    echo "example:  bash $0 2017091500 [2017100112]"
    exit 1
elif (( ${#@} == 1 && ${#1} != 10 )); then
    echo "unrecognized year-month-day: ${1}"
    echo "usage bash $0 yyyymmddhh [end_yyyymmddhh]"
    echo "example:  bash $0 2017091500 [2017100112]"
    exit 1
elif (( ${#@} == 2 && ${#1} != 10 )) || (( ${#@} == 2 && ${#2} != 10 )); then
    echo "unrecognized year-month-day: ${1}  ${2}"
    echo "usage bash $0 yyyymmddhh [end_yyyymmddhh]"
    echo "example:  bash $0 2017091500 [2017100112]"
    exit 1
fi

# set time to start and end retrieval
dtg=${1}
dtg_end=${2:-${dtg}}

# user should register for free with JAXA G-portal:
# https://gportal.jaxa.jp

# credentials
user=${USER}
pass=anonymous

address="ftp://${user}@ftp.gportal.jaxa.jp"
product='L1B'
platform='GCOM-W'
sensor='AMSR2'
version='2'
destination_folder="standard/${platform}/${platform}.${sensor}/${product}/${version}"

# local destination
data_repos=${DATA_REPOS:-${UWORKDIR}/data_repos}

local_directory() {
    adir=${data_repos}/amsr2/${dtg:0:8}
    if [[ ! -d ${adir} ]]; then
        echo "mkdir -p ${adir}"
        mkdir -p ${adir}
    fi
    cd ${adir}
}

while (( dtg <= dtg_end )); do
    local_directory
    # GW1AM2_202202142308_165A_L1SGBTBR_2220220.h5
    path="${address}/${destination_folder}/${dtg:0:4}/${dtg:4:2}"
    data_file="*_${dtg:0:8}*.h5"
    cmd="wget -nc --user=${user} --password=${pass} ${path}/${data_file}"
    echo "${cmd}"
    ${cmd}

    # standard Linux date
    dtg=$( date -u --date="+1 day ${dtg0:4}-${dtg:4:2}-${dtg:6:2}" +%Y%m%d%H )
    # MacOS command
    # dtg=$( date -u -j -v +1d -f "%Y%m%d%H" "${dtg}" +%Y%m%d%H )
done
