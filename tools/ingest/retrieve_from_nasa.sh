#!/bin/sh

##################################################################
# 
#  PURPOSE:
#       retrieve data from NASA GES DISC for single day
#         or optionally a range of days
#         does not clobber so can be re-run 
#
#   REQUIREMENTS:
#       wget
#       gnu date
#       NASA EARTHDATA account (https://disc.gsfc.nasa.gov/data-access)
#         - must have account and add server and user/pass to ~/.netrc
#         - follow instructions on NASA EARTHDATA access page above
#
##################################################################

disc_address='https://gpm1.gesdisc.eosdis.nasa.gov'

if (( ${#@} == 0 ))  ||  (( ${#@} > 2 )) || [[ $1 == [hH]elp ]]; then
    echo "usage bash $0 yyyymmdd [end_yyyymmdd]"
    echo "example:  bash $0 20170915 [20171001]"
    exit 1
elif (( ${#@} == 1 && ${#1} != 8 )); then
    echo "unrecognized year-month-day: ${1}"
    echo "usage bash $0 yyyymmdd [end_yyyymmdd]"
    echo "example:  bash $0 20170915 [20171001]"
    exit 1
elif (( ${#@} == 2 && ${#1} != 8 )) || (( ${#@} == 2 && ${#2} != 8 )); then
    echo "unrecognized year-month-day: ${1}  ${2}"
    echo "usage bash $0 yyyymmdd [end_yyyymmdd]"
    echo "example:  bash $0 20170915 [20171001]"
    exit 1
fi

dtg=${1}
end_dtg=${2:-${dtg}}

get_listing() {
    if [[ -s index.html ]]; then
        rm -rf index.html
    fi
    wget ${apath}
}

get_file_names() {
    zfiles=$(grep HDF5 index.html |  grep -v xml | awk '{print $3}' | cut -d '"' -f 2)
}

get_files() {
    for afile in ${zfiles[@]}; do
        echo wget -nc ${apath}/${afile}
        # wget -nc ${apath}/${afile}
        exit 0
    done
}

get_julian_day() {
    if [[ ${macOS} == False ]]; then
      jjj=$( date -u --date="${dtg:0:4}-${dtg:4:2}-${dtg:6:2} 00" +%j )
    else
      jjj=$( date -u -j -f "%Y%m%d" "${dtg}" +%j )
    fi
}

advance_date() {
    if [[ ${macOS} == False ]]; then
      dtg=$( date -u --date="+1 day ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} 00" +%Y%m%d )
    else
      dtg=$( date -u -j -v +1d -f "%Y%m%d" "${dtg}" +%Y%m%d )
    fi
}


get_macOS() {
macOS=False
    if [[ "${OSTYPE}" == "darwin"* ]]; then
        macOS=True
    fi
}

get_instru_path() {
    # instru_path=GPM_L1B/GPM_1BGMI.07              # GMI   - level 1B
    instru_path=GPM_L1C/GPM_1CGCOMW1AMSR2.07        # AMSR2 - level 1C
}

get_instrument() {
    # instrument=gmi
    instrument=amsr2
}

get_macOS
get_instrument
get_instru_path
while (( dtg <= end_dtg )); do
    get_julian_day
    apath=${disc_address}/data/${instru_path}/${dtg:0:4}/${jjj}/
    get_listing
    get_file_names
    get_files
    advance_date
done
