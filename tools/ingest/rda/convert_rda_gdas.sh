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
window_length=$( printf "%d" ${assim_freq} )

workdir=${UWORKDIR:-/glade/p/univ/ucos0003/${USER}}
data_type=${data_type:-amsua}
data_gdas_type=${data_gdas_type:-1bamua}
data_repos=${DATA_REPOS:-${workdir}/data_repos}
ioda_bundle=${IODA_BUNDLE:-${WORKDIR}/work/JCSDA/JEDI/ioda-bundle}
ncar2ioda_bundle=${NCAR2IODA_BUNDLE:-${WORKDIR}/work/JCSDA/JEDI/ncar2ioda/obs2ioda/obs2ioda-v2/src}
# can use for all ?
# ncar2ioda_bundle=${NCAR2IODA_BUNDLE:-${WORKDIR}/work/JCSDA/JEDI/ncar2ioda/obs2ioda/obs2ioda-prepBUFR/src}


gen_ioda() {
    cd ${output_temp_dir}
    wild_string=${data_type}_*_obs_${dtg}.h5
    nfiles=$(ls -1 ${wild_string}  2>/dev/null | wc -l)
    if (( nfiles == 0 )); then
        echo " ... WARNING no files found: ${output_temp_dir}/${wild_string}"
        return 1
    fi
    for afile in ${wild_string}; do
        platform=${afile#${data_type}_}
        platform=${platform%_obs*}
        echo "   .... ahhh ... platform ... ${platform}"
        output_file="${output_dir}/${atime}_${data_type}_${platform}.nc4"
        cmd="mpirun -np 1 ${ioda_bundle}/build/bin/ioda-upgrade.x ${afile} ${output_file}"
        echo "${cmd}"
        ${cmd}
        # path to utility does not work
        python3 ${UWORKDIR}/data_repos/fix_nchans.py --input ${output_file} --sensor ${data_type}
    done
}

gen_bufr2nc() {
    # input file and input file path
    adir=${data_repos}/${data_type}/${dtg:0:6}
    afile=gdas.${data_gdas_type}.t${dtg:8:2}z.${dtg:0:8}.bufr
    # did not find any files
    if [[ ! -s ${adir}/${afile} ]]; then
        # path to utility does not work
        ${data_repos}/${data_type}/link_RDA.sh ${dtg}
    fi
    if [[ ! -s ${adir}/${afile} ]]; then
        echo "  ... WARNING missing file: ${adir}/${afile}"
        return 1
    fi
    make_temp_output_path
    ### cmd="${ioda_bundle}/build/bin/bufr2nc_fortran.x -i ${adir} -o ${output_temp_dir}  ${afile}"
    cmd="mpirun -n 1 ${ncar2ioda_bundle}/obs2ioda-v2.x -i ${adir} -o ${output_temp_dir}  ${afile}"
    echo "${cmd}"
    ${cmd}
}

make_path() {
    if [[ ! -d ${new_dir} ]]; then
        echo "convert... making: ${new_dir}"
        mkdir -p ${new_dir}
    fi
}

make_output_path() {
    # output path
    new_dir=${data_repos}/ioda/${data_type}
    output_dir=${new_dir}
    make_path
}

make_temp_output_path() {
    # output temporary path this can be deleted
    new_dir=${data_repos}/ioda/${data_type}/${dtg:0:4}/${dtg}
    output_temp_dir=${new_dir}
    make_path
}

check_error() {
    if (( $? != 0 )); then
        echo "   ... ERROR conversion failed at: ${dtg}"
        exit 1
        # dtg=$( date -u --date="+6 hours ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} ${dtg:8:2}" +%Y%m%d%H )
    fi
}


################
###   main   ###
################
make_output_path
while (( dtg <= final_dtg )); do
    DTG_begin=$( date -u --date="-3 hours ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} ${dtg:8:2}" +%Y%m%d%H )
    atime=${DTG_begin:0:8}T${DTG_begin:8:2}Z_PT${window_length}H
    gen_bufr2nc
    check_error
    gen_ioda
    check_error
    dtg=$( date -u --date="+6 hours ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} ${dtg:8:2}" +%Y%m%d%H )
done
