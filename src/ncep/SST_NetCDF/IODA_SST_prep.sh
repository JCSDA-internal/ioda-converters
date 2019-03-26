
#NOTE:############################################################################
##  read NetCDF SST from /dcomdev and 
##  create input files for IODA marine 

###################################################################################
set -x
echo "*** Started script $0"
if [ $# -lt 2 ] ; then
   echo USAGE="Usage: ncoda_SST_prep_nc.sh YYYYMMDDHH Time_Window_hrs " 
   exit 2
fi
module unload NetCDF
module load NetCDF/4.2/serial
###
for OBS in 19_G ; do
if [ $OBS == '19_G' ] ; then
  export OBS1='19'
export nfilesday=24
echo $OBS $OBS1
fi
export DATA0=AVHRR
export DIR=${DATA0}$OBS1
export DATA=$DATA0$OBS
export DATA1=avhrr$OBS1
export DATA2=${DATA}
###
export fname_template1=OSPO-L2P_GHRSST-SSTsubskin-${DATA2}-ACSPO_V2.41-v02.0-fv01.0.nc
export SSTDIR=/ocean/save/$USER/JEDI-SOCA
export DATADIR=/dcomdev/us007003

startdate=${1:?"arg1: startdate required"}   # echo warning message when arg1 is missing
time_window=${2:?"arg1: enddate required"} 
#enddate=${2:?"arg1: enddate required"}  #  echo's warning msg when arg2 is missing
echo "start date = '$startdate' Temporal Window of analysis  = +/- '$time_window' (hours)"
#echo "start date = '$startdate' end date = '$enddate'"     


ADATE=`ndate -12 $startdate`
enddate=`ndate +12 $startdate`

#ADATE2=$ADATE'00'
#ADATE2=`/nwprod/util/exec/ndate +24 $ADATE2 | cut -c1-8`
#ADATE2=$ADATE


while [[ $ADATE -le $enddate ]]
do
  echo Running decoder for $ADATE
  fname_template=$ADATE??????-${fname_template1}
  dirou=$SSTDIR/$ADATE                          #$SSTDIR/pre_ncodaqc_SST/$ADATE
  mkdir -p $dirou
  cd $dirou
#  cp $BIN/read_nc_combine_sst .


  rm -f filenclist #fort.18 fort.19 osatnl
  for file in $DATADIR/$ADATE2/sst/$fname_template ; do 
     direct=$(dirname $file)
     echo $direct
     filename=$(basename $file)
     echo $filename
     ymdhm=$(echo $filename | cut -c1-12)
     echo $filename
     echo $file >> filenclist
       find . -maxdepth 1 -type f -size 0 -delete

 #done  # files in $ADATE2
 export nfiles=`wc -l filenclist | awk '{print $1}'`
 if [ $nfiles -lt $nfilesday ] ; then 
    echo "$nfiles smaller than $nfilesday"
#    exit  
 fi
 export nchfile=`wc -L filenclist | awk '{print $1}'`
 echo $nchfile
#  nobsmx = 120000000,
#  nobsmxi =100000,
cat << eof > osatnl

  &satnl

  nz    =  0,
  nchan =  3,
  nchfile = $nchfile,
  nmax = $nfiles,
  maxsec = 3600,
  sid = 19,
  szamx = 90,
  time_anl = $startdate
  &end

eof
   cp -p ../src/read_nc_combine_sst_IODA .
  ./read_nc_combine_sst #$1 $2
     ADATE=$ADATE'00'
     ADATE=`/nwprod/util/exec/ndate +24 $ADATE | cut -c1-8`

done # while [[ $ADATE -le  $enddate ]]
done # for OBS
exit 0

