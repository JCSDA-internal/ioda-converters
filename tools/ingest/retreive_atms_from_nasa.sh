#!/bin/sh

# retrieve ATMS data from NASA GES DISC

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

macOS=False
if [[ "${OSTYPE}" == "darwin"* ]]; then
  macOS=True
fi

dtg=${1}
end_dtg=${2:-${dtg}}

get_contents() {
    # get list of potential files to retrieve
    if [[ -f contents.html ]]; then
      rm -f contents.html
    fi
    wget https://sounder.gesdisc.eosdis.nasa.gov/opendap/SNPP_Sounder_Level1/SNPPATMSL1B.3/${yyyy}/${jjj}/contents.html
}

get_files() {
    # retrieve the files
    zfiles=$( grep SNDR contents.html | grep nc | grep sameAs | awk '{print $2}' )
    for afile in ${zfiles[@]}; do
        gfile=${afile#?}
        gfile=${gfile%?}
        gfile=$( echo ${gfile} | sed -e 's/html/nc4/' )
        out_file=${gfile##*/}
        out_file="${out_file%.*.*}.nc4"
        echo "wget ${gfile} -O ${out_file}"
        wget -nc ${gfile} -O ${out_file}
        exit 0
    done
}

while (( dtg <= ${end_dtg} )); do
    yyyy=${dtg:0:4}
    if [[ ${macOS} == False ]]; then
      jjj=$( date -u --date="${dtg:0:4}-${dtg:4:2}-${dtg:6:2} 00" +%j )
    else
      jjj=$( date -u -j -f "%Y%m%d" "${dtg}" +%j )
    fi
    get_contents
    get_files
    if [[ ${macOS} == False ]]; then
      dtg=$( date -u --date="+1 day ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} 00" +%Y%m%d )
    else
      dtg=$( date -u -j -v +1d -f "%Y%m%d" "${dtg}" +%Y%m%d )
    fi
done
