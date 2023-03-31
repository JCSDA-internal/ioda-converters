#!/bin/sh

if (( ${#@} == 0 ))  ||  (( ${#@} > 2 )) || [[ $1 == [hH]elp ]]; then
    echo "usage bash $0 yyyymmddhh [end_yyyymmddhh]"
    echo "example:  bash $0 2017091500 [2017100100]"
    exit 1
elif (( ${#@} == 1 && ${#1} != 10 )) || (( ${#@} == 2 && ${#1} != 10 )) || (( ${#@} == 2 && ${#2} != 10 )); then
    echo "unrecognized year-month-day: ${1}  ${2}"
    echo "usage bash $0 yyyymmddhh [end_yyyymmddhh]"
    echo "example:  bash $0 2017091500 [2017100100]"
    exit 1
fi

# set up start and end date
dtg=$1
end_dtg=$dtg
if (( ${#@} == 2 )); then
  end_dtg=$2
fi

UWORKDIR=${UWORKDIR:-${PWD}}
data_repos=${DATA_REPOS:-${UWORKDIR}/data_repos}

address=${SSEC_ARCHIVE_ADDRESS:-"http://tropic.ssec.wisc.edu/archive/data"}

ztypes=(
Australia
EuropeAfrica
Indian
NEPacific
NWAtlantic
NWPacific
SEPacific
)

#QI2022020918SHMTH > Australia
#QI2022020918WVM7H > EuropeAfrica
#QI2022020918WVM5H > Indian
#MET8QI2022020918WVM5H (same files) > Indian
#AMV202202091800GOESW > NEPacific
#AMV202202091800GOESE > NWAtlantic
#QI2022020918WVMTH  > NWPacific
#QI2022020918TPARC-ALL (same files) > NWPacific
#QI2022020918WG10SH > SEPacific

change_dir() {
dest_dir=${data_repos}/conventional/satwinds/${dtg:0:8}
if [[ ! -d ${dest_dir} ]]; then
    mkdir -p ${dest_dir}
fi
cd ${dest_dir}
}

adjust_dtg() {
# adjust date time group to nearest floor 3 hour
hh=${dtg:8:2}
while (( 10#${hh} % 3 > 0 )); do
  dtg=$( date -u --date="-1 hours ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} ${dtg:8:2}" +%Y%m%d%H )
  hh=${dtg:8:2}
done
}

# loop by hour from start to end
adjust_dtg
while (( dtg <= end_dtg )); do
  for atype in ${ztypes[@]}; do
    change_dir
    hh=${dtg:8:2}
    afile=${dtg:0:8}.${hh}.${atype}.AllWindsQIText
    echo "retrieving:  ${address}/${atype}/${dtg:0:8}/AllWindsQIText/${afile}"
    wget -nc ${address}/${atype}/${dtg:0:8}/AllWindsQIText/${afile}
    # remove zero length files (exist but not non-zero)
    if [[ -e ${afile} && ! -s ${afile} ]]; then
        rm ${afile}
    fi
  done
  # advance date
  dtg=$( date -u --date="+3 hours ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} ${dtg:8:2}" +%Y%m%d%H )
done
