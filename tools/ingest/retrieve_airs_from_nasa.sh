#!/bin/sh

if (( ${#@} == 0 ))  ||  (( ${#@} > 2 )) || [[ $1 == [hH]elp ]]; then
    echo "usage bash $0 yyyymmdd [end_yyyymmdd]"
    echo "example:  bash $0 20170915 [20171001]"
    exit 1
elif (( ${#@} == 1 && ${#1} != 8 )) || (( ${#@} == 2 && ${#1} != 8 )) || (( ${#@} == 2 && ${#2} != 8 )); then
    echo "unrecognized year-month-day: ${1}  ${2}"
    echo "usage bash $0 yyyymmdd [end_yyyymmdd]"
    echo "example:  bash $0 20170915 [20171001]"
    exit 1
fi

# set up start and end date
dtg=$1
end_dtg=$dtg
if (( ${#@} == 2 )); then
  end_dtg=$2
fi

UDATA_REPOS=${UDATA_REPOS:-${PWD}}
data_repos=${UDATA_REPOS:-/glade/p/univ/ucos0003/${USER}/data_repos}

# loop by day from start to end
while (( dtg <= end_dtg )); do
  dest_dir=${data_repos}/airs/${dtg:0:8}
  if [[ ! -d ${dest_dir} ]]; then
    mkdir -p ${dest_dir}
  fi
  cd ${dest_dir}
  # variable for year and julian day (day of year)
  yyyy=${dtg:0:4}
  jjj=$( date -u --date="${dtg:0:4}-${dtg:4:2}-${dtg:6:2} 00" +%j )
  # AIRS.2021.04.02.203.L1B.AIRS_Rad.v5.0.25.0.R21092182132.bufr
  zfiles=$( wget -q -nH -nd  https://discnrt1.gesdisc.eosdis.nasa.gov/data/Aqua_NRT/AIRIBRAD_NRT_BUFR.005/${yyyy}/${jjj} -O - | grep "AIRS" | grep -v xml | cut -f4 -d\" 2>/dev/null )

  for afile in ${zfiles[@]}; do
    if [[ ! -s $afile ]]; then
      echo "wget -q -nH -nd https://discnrt1.gesdisc.eosdis.nasa.gov/data/Aqua_NRT/AIRIBRAD_NRT_BUFR.005/${yyyy}/${jjj}/${afile}"
      wget -q -nH -nd "https://discnrt1.gesdisc.eosdis.nasa.gov/data/Aqua_NRT/AIRIBRAD_NRT_BUFR.005/${yyyy}/${jjj}/${afile}"
      chmod 664 ${afile}
    fi
  done

  dtg=$( date -u --date="+1 day ${dtg:0:4}-${dtg:4:2}-${dtg:6:2} 00" +%Y%m%d )
done
