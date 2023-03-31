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

dtg=${1}
final_dtg=${2:-${dtg}}

assim_freq=${assim_freq:-6}

workdir=${UWORKDIR:-/glade/p/univ/ucos0003/${USER}}
RDAdir=/glade/collections/rda/data
data_type=${data_type:-amsua}
data_gdas_type=${data_gdas_type:-1bamua}
data_repos=${DATA_REPOS:-${workdir}/data_repos}


make_path() {
    if [[ ! -d ${new_dir} ]]; then
        echo "link ... making: ${new_dir}"
        mkdir -p ${new_dir}
    fi
}

# if data_type 'radiosonde' or other prepend 'conventional'

# prepBUFR - ds337.0/prepnr/2022/prepbufr.gdas.2022022800.nr
# AMSU-A ds735.0/1bamua/2022/prepbufr.gdas.2022022800.nr
# 1bmhs
# 1bmhs
# mtiasi
# cris (cris - NSR; crisf4 - FSR)

while (( dtg <= final_dtg )); do
    # check_error
    afile="${RDAdir}/ds735.0/${data_gdas_type}/${dtg:0:4}/${data_gdas_type}.${dtg:0:8}.tar.gz"
    tfile="gdas.${data_gdas_type}.t${dtg:8:2}z.${dtg:0:8}.bufr"
    if [[ -s ${afile} ]]; then
        new_dir=${data_repos}/${data_type}/${dtg:0:6}
        make_path
        if [[ ! -s ${new_dir}/${tfile} ]]; then
            ln -sf $afile ${new_dir}/.
            cwd=$(pwd)
            cd ${new_dir}
            tar xvf ${afile}
            cd ${cwd}
        fi
    else
        echo "MISSING: ${afile}"
    fi
    dtg=$( date -u --date="+${assim_freq} hours ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} ${dtg:8:2}" +%Y%m%d%H )
done
