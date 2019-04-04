#!/bin/sh
################################
module unload NetCDF
module load NetCDF/4.2/serial
################################
set -x
echo "*** Started script $0"
if [ $# -lt 2 ] ; then
   echo USAGE="Usage: IODA_SST_prep_nc.sh YYYYMMDDHH Time_Window_hrs " 
   exit 2
fi

#OBS=${1:?"arg1: satellite ABI, AHI, 19_G, MTA, or VIIRS  required"}
anldate=${1:?"arg1: analysis date in YYYYMMDDHH is required"} 
time_window=${2:?"arg2:  time window in hours is required"}

echo "Start date= '$anldate' and Temporal Window of analysis = +/- '$time_window' (hours)"

#######################################################################
# Output Directory on theia: To be changed by the $USER 
export SSTDIR=/scratch3/NCEPDEV/marine/save/Shastri.Paturi/JEDI-SOCA/NCEP_data/JEDI-SOCA/src
# Data Directory on theia: mapped from WCOSS
export DATADIR=/scratch4/NCEPDEV/ocean/scrub/Stylianos.Flampouris/marine_observations/dcomdev/us007003/

for OBS in ABI AHI 19_G MTA VIIRS; do
    if [ $OBS == 'ABI' ] ; then
       OBS1=goes16
       OBS2=_G16
       DATA0=''
       DATA1=$OBS1
       NAM1=STAR
       V=V2.50-v02.0-fv01.0
       nfilesday=24
#       HINT=1
       maxsec=3600
       nchan=4
       sid=16
       echo $OBS $OBS1

    elif [ $OBS == 'AHI' ] ; then
       OBS1=himawari8
       OBS2=_H08
       DATA0=''
       DATA1=$OBS1
       NAM1=STAR
       V=V2.50-v02.0-fv01.0
       nfilesday=24
#       HINT=1
       maxsec=3600
       nchan=4
       sid=8
       echo $OBS $OBS1

     elif [ $OBS == '19_G' ] ; then
       OBS1=19
       OBS2=''
       DATA0='AVHRR'
       DATA1=avhrr$OBS1
       NAM1=OSPO
       V=V2.41-v02.0-fv01.0
       nfilesday=24
#       HINT=1
       maxsec=3600
       nchan=3
       sid=19
       echo $OBS $OBS1

    elif [ $OBS == 'MTA' ] ; then
       OBS1=mta
       OBS2=''
       DATA0=AVHRR
       DATA1=avhrr$OBS1
       NAM1=OSPO
       V=V2.41-v02.0-fv01.0
       nfilesday=144
#       HINT=1
       maxsec=600
       nchan=3
       sid=1
       echo $OBS $OBS1

    elif [ $OBS == 'VIIRS' ] ; then
       OBS1=viirs
       OBS2=_NPP
       DATA0=''
       DATA1=$OBS1
       NAM1=OSPO
       V=V2.60-v02.0-fv01.0
       nfilesday=144
#       HINT=1
       maxsec=600
       nchan=3
       nobsmx=400000000
       sid=1
       echo $OBS $OBS1
    fi

    echo $OBS $nfilesday
    export DATA=$DATA0$OBS$OBS2

    dirou=$SSTDIR/$anldate/$DATA
    mkdir -p $dirou
    cd $dirou
    rm -f filenclist

    export HINT=1
    export fname_template1=${NAM1}-L2P_GHRSST-SSTsubskin-${DATA}-ACSPO_${V}.nc
    echo $fname_template
    
    ADATE=`/nwprod/util/exec/ndate -$time_window $anldate`
    EDATE=`/nwprod/util/exec/ndate +11 $anldate`

    ADATEHH=$ADATE
    while [[ $ADATEHH -le $EDATE ]] ; do
         echo "Running decoder for $anldate"
         fname_template=${ADATEHH}????-${fname_template1}
         DIRDATE=$(echo $ADATEHH | cut -c1-8)

         for file in $DATADIR/$DIRDATE/sst/$fname_template ; do
             echo $file >> filenclist
         done # file 
         ADATEHH=`/nwprod/util/exec/ndate $HINT $ADATEHH`   # increment per hour
         echo $ADATEHH

         nfiles=`wc -l filenclist | awk '{print $1}'`
         if [ $nfiles -lt $nfilesday ] ; then
            echo "$nfiles smaller than $nfilesday"
         fi
         nchfile=`wc -L filenclist | awk '{print $1}'`
cat << eof > osatnl 
    &satnl

    nz      = 0,
    nchan   = $nchan,
    nchfile = $nchfile,
    nmax    = $nfiles,
    maxsec  = $maxsec,
    runtime = $anldate,
    sat_name= $DATA,
    sid     = $sid,
    szamx   = 90,
    &end
eof

    done # while         
        
    cp -p $SSTDIR/../EMC_UMO/src/read_nc_combine_sst_IODA .
    ./read_nc_combine_sst_IODA
#   #ADD condition for empty files

done # OBS
