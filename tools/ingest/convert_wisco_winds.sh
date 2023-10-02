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

source ${HOME}/setup_jedi_gnu.sh
data_repos=${DATA_REPOS:-${UWORKDIR}/data_repos}
mypy=python3
ioda_bundle=${IODA_BUNDLE:-${WORKDIR}/work/JCSDA/JEDI/ioda-bundle-gnu}
export PYTHONPATH=${IODA_BUNDLE}/build/lib/python3.9/:${PYTHONPATH}
assim_freq=${ASSIM_FREQ:-6}
window_length=$( printf "%d" ${assim_freq} )
outpath=${UWORKDIR}/data_repos/ioda/satwinds

ztypes=(
Australia
EuropeAfrica
Indian
NEPacific
NWAtlantic
NWPacific
SEPacific
)

change_dir() {
dest_dir=${data_repos}/conventional/satwinds/${dtg:0:8}
if [[ ! -d ${dest_dir} ]]; then
    echo " no directory or files:  ${dest_dir}"
    return
fi
cd ${dest_dir}
}


if [[ ! -d ${outpath} ]]; then
    echo "creating:  ${outpath}"
    mkdir -p ${outpath}
fi

# loop by hour from start to end
while (( dtg <= end_dtg )); do
    change_dir
    DTG_begin=$( date -u --date="-3 hours ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} ${dtg:8:2}" +%Y%m%d%H )
    atime=${DTG_begin:0:8}T${DTG_begin:8:2}Z_PT${window_length}H
    hh=${dtg:8:2}
    wild_search=${dtg:0:8}.${hh}.*.AllWindsQIText
    nfiles=$( ls -1 ${dtg:0:8}.${hh}.*.AllWindsQIText 2>/dev/null | wc -l )
    outfile=${outpath}/${atime}_satwinds_ssec_amv.nc4
    if (( nfiles > 0 )); then
      echo "creating: ${outfile}"
      cmd="nice -n 19 ${mypy} ${ioda_bundle}/iodaconv/src/conventional/amv_ssec_ascii2ioda.py -i ${wild_search} -o ${outfile} -d ${dtg}"
      echo "${cmd}"
      ${cmd}
      # mpirun -np 1 ${mypy} ${ioda_bundle}/iodaconv/src/conventional/amv_ssec_ascii2ioda.py -i ${wild_search} -o ${outfile} -d ${dtg}
    fi
  # advance date
  dtg=$( date -u --date="+6 hours ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} ${dtg:8:2}" +%Y%m%d%H )
done
