#!/bin/sh

##################################################################
# 
#  PURPOSE:
#       retrieve ATMS data from NASA GES DISC for single day
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

nasa_gesdisc_address='https://sounder.gesdisc.eosdis.nasa.gov/opendap'
noaa_class_address='http://ftp-jpss.avl.class.noaa.gov'   # not implemented yet

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
    if [[ -f atms_noaa20.html ]]; then
      rm -f atms_noaa20.html
    fi
    wget ${nasa_gesdisc_address}/hyrax/JPSS1_Sounder_Level1/SNDRJ1ATMSL1B.3/${yyyy}/${jjj}/contents.html -O atms_noaa20.html
    if [[ -f atms_snpp.html ]]; then
      rm -f atms_snpp.html
    fi
    wget ${nasa_gesdisc_address}/SNPP_Sounder_Level1/SNPPATMSL1B.3/${yyyy}/${jjj}/contents.html -O atms_snpp.html
}

get_files() {
    # retrieve the files
    for sat in snpp noaa20; do
      zfiles=$( grep SNDR atms_${sat}.html | grep nc | grep sameAs | awk '{print $2}' )
      for afile in ${zfiles[@]}; do
        gfile=${afile#?}
        gfile=${gfile%?}
        gfile=$( echo ${gfile} | sed -e 's/html/nc4/' )
        out_file=${gfile##*/}
        out_file="${out_file%.*.*}.nc4"
        #echo "wget ${gfile} -O ${out_file}"
        wget -nc ${gfile} -O ${out_file}
      done
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


while (( dtg <= ${end_dtg} )); do
    yyyy=${dtg:0:4}
    get_julian_day
    get_contents
    get_files
    advance_date
done
