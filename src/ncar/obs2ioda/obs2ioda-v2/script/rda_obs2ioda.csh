#!/bin/csh -f
#Sample script to convert CISL RDA archived NCEP BUFR files to IODA-v1 format
#Author: Jamie Bresch (NCAR/MMM)
#Platform: Cheyenne

#set SUBMIT_CMD = qsub
set SUBMIT_CMD = none
set DATE_TOOL = ${HOME}/bin/da_advance_time.exe
set SCRIPT_TO_SUBMIT = /glade/u/home/hclin/proj/ioda/submit.pbs
set SOURCE_DIR_TOP = /gpfs/fs1/collections/rda/data  #ds337.0, ds735.0, ds351.0

set EXEC = /glade/work/hclin/jedi/obs2ioda/obs2ioda-v1/src/obs2ioda.x #intel
#module restore system
#cd /glade/work/hclin/jedi/obs2ioda/obs2ioda-v1/src
#make clean; make

set OBS_ERRTABLE = /glade/u/home/hclin/proj/ioda/obs_errtable
set SPC_COEFF_DIR = /glade/u/home/hclin/proj/ioda/SpcCoeff

#set OBTYPES = ( prepbufr gpsro 1bamua 1bmhs airsev )
#set OBTYPES = ( prepbufr gpsro 1bamua 1bmhs )
#set OBTYPES = ( prepbufr )
#set OBTYPES = ( satwnd )
set OBTYPES = ( 1bamua )
#set OBTYPES = ( airsev )
#set OBTYPES = ( mtiasi )
#set OBTYPES = ( cris )

#setenv OPTIONS "-split"  # writing out hourly files
setenv OPTIONS ""

set TARGET_DIR_TOP = /glade/scratch/hclin/data/ioda-v1
set START_DATE = 2018041500
set   END_DATE = 2018041518
set INTERVAL   = 6

set DATE = ${START_DATE}
while ( ${DATE} <= ${END_DATE} )
   set ccyy = `echo $DATE | cut -c1-4`
   set mmdd = `echo $DATE | cut -c5-8`
   set   hh = `echo $DATE | cut -c9-10`

   set TARGET_DIR = ${TARGET_DIR_TOP}/${DATE}
   if ( ! -d ${TARGET_DIR} ) mkdir -p ${TARGET_DIR}
   cd ${TARGET_DIR}
   echo "In ${TARGET_DIR}"

   foreach inst ( ${OBTYPES} )

      if ( ${inst} == satwnd ) then
         setenv THIS_FILE gdas.${inst}.t${hh}z.${ccyy}${mmdd}.bufr
         if ( ! -e ${THIS_FILE}) then
            echo "Source file: ${SOURCE_DIR_TOP}/ds351.0/bufr/${ccyy}/${THIS_FILE}"
            cp -p ${SOURCE_DIR_TOP}/ds351.0/bufr/${ccyy}/${THIS_FILE} .
         endif
         if ( -e ${OBS_ERRTABLE} ) then
            ln -sf ${OBS_ERRTABLE} obs_errtable
         endif
      else if ( ${inst} == prepbufr ) then
         setenv THIS_FILE prepbufr.gdas.${ccyy}${mmdd}.t${hh}z.nr.48h
         if ( ! -e ${THIS_FILE}) then
            echo "Source file: ${SOURCE_DIR_TOP}/ds337.0/prep48h/${ccyy}/${THIS_FILE}"
            cp -p ${SOURCE_DIR_TOP}/ds337.0/prep48h/${ccyy}/${THIS_FILE} .
         endif
         # use obs errors embedded in prepbufr file
         if ( -e obs_errtable ) then
            rm -f obs_errtable
         endif
         # use external obs error table
         #if ( -e ${OBS_ERRTABLE} ) then
         #   ln -sf ${OBS_ERRTABLE} obs_errtable
         #endif
      else
         # set the specific file to be extracted from the tar file
         setenv THIS_FILE gdas.${inst}.t${hh}z.${ccyy}${mmdd}.bufr
         if ( ${inst} == 'cris' && ${ccyy} >= '2021' ) then
            # cris file name became crisf4 since 2021
            setenv THIS_FILE gdas.${inst}f4.t${hh}z.${ccyy}${mmdd}.bufr
         endif
         set THIS_TAR_FILE = ${SOURCE_DIR_TOP}/ds735.0/${inst}/${ccyy}/${inst}.${ccyy}${mmdd}.tar.gz
         if ( ${inst} == 'cris' && ${ccyy} >= '2021' ) then
            # cris file name became crisf4 since 2021
            set THIS_TAR_FILE = ${SOURCE_DIR_TOP}/ds735.0/${inst}/${ccyy}/${inst}f4.${ccyy}${mmdd}.tar.gz
         endif
         if ( ! -e ${THIS_FILE}) then
# tar -x -f /gpfs/fs1/collections/rda/data/ds735.0/gpsro/2018/gpsro.20180415.tar.gz 20180415.gpsro/gdas.gpsro.t00z.20180415.bufr
# tar -x -f ${SOURCE_DIR_TOP}/ds735.0/${inst}/${ccyy}/${inst}.${ccyy}${mmdd}.tar.gz ${ccyy}${mmdd}.airssev/gdas.${inst}.t${hh}z.${ccyy}${mmdd}.bufr
# mv ${ccyy}${mmdd}.airssev/gdas.${inst}.t${hh}z.${ccyy}${mmdd}.bufr .
            # some tar files contain sub-directory
            set THIS_TAR_DIR = ${ccyy}${mmdd}.${inst}
            tar -x -f ${THIS_TAR_FILE} ${THIS_TAR_DIR}/${THIS_FILE}
            if ( $status == 0 ) then
               echo "Source file: tar -x -f ${THIS_TAR_FILE} ${THIS_TAR_DIR}/${THIS_FILE}"
               set got_file = true
               set SUB_DIR = true
            else #if ( $status != 0 ) then
               # try no sub-directory
               tar -x -f ${THIS_TAR_FILE} ${THIS_FILE}
               if ( $status == 0 ) then
                  echo "Source file: tar -x -f ${THIS_TAR_FILE} ${THIS_FILE}"
                  set SUB_DIR = false
                  set got_file = true
               else
                  set got_file = false
               endif
            endif
            if ( ${got_file} == false && ${inst} == airsev ) then
               #if airs, try again with another dir name
               #typo in the archived directory name
               set THIS_TAR_DIR = ${ccyy}${mmdd}.airssev
               tar -x -f ${THIS_TAR_FILE} ${THIS_TAR_DIR}/${THIS_FILE}
               if ( $status == 0 ) then
                  set SUB_DIR = true
                  echo "Source file: tar -x -f ${THIS_TAR_FILE} ${THIS_TAR_DIR}/${THIS_FILE}"
                  set got_file = true
               endif
            endif
            if ( ${got_file} == true ) then
               if (  ${SUB_DIR} == true ) then
                  mv ${THIS_TAR_DIR}/${THIS_FILE} .
                  rmdir ${THIS_TAR_DIR}
               endif
            endif
         endif # others
      endif # satwnd, prepbufr or others

      if ( -e ${THIS_FILE} ) then
         echo "Running ${EXEC} for ${inst} ..."
         # link SpcCoeff files for converting IR radiances to brightness temperature
         if ( ${inst} == 'cris' && ${ccyy} >= '2021' ) then
           ln -sf ${SPC_COEFF_DIR}/cris-fsr431_npp.SpcCoeff.bin  ./cris_npp.SpcCoeff.bin
           ln -sf ${SPC_COEFF_DIR}/cris-fsr431_n20.SpcCoeff.bin  ./cris_n20.SpcCoeff.bin
         else if ( ${inst} == 'cris' && ${ccyy} < '2021' ) then
           ln -sf ${SPC_COEFF_DIR}/cris399_npp.SpcCoeff.bin  ./cris_npp.SpcCoeff.bin
           ln -sf ${SPC_COEFF_DIR}/cris399_n20.SpcCoeff.bin  ./cris_n20.SpcCoeff.bin
         else if ( ${inst} == 'mtiasi' ) then
           ln -sf ${SPC_COEFF_DIR}/iasi616_metop-a.SpcCoeff.bin  ./iasi_metop-a.SpcCoeff.bin
           ln -sf ${SPC_COEFF_DIR}/iasi616_metop-b.SpcCoeff.bin  ./iasi_metop-b.SpcCoeff.bin
           ln -sf ${SPC_COEFF_DIR}/iasi616_metop-c.SpcCoeff.bin  ./iasi_metop-c.SpcCoeff.bin
         endif
         if ( ${SUBMIT_CMD} == none ) then
            ${EXEC} ${OPTIONS} ${THIS_FILE} >&! log_${inst}
         else
            if ( ! -e run_me.exe ) ln -sf ${EXEC} run_me.exe
            ${SUBMIT_CMD} -N run${mmdd}${hh} -o job_${inst} ${SCRIPT_TO_SUBMIT}
         endif
      endif

   end # inst loop

   if ( ${SUBMIT_CMD} != none ) then
      sleep 120 #in order not to swamp the queue
   endif

   set DATE = `${DATE_TOOL} ${DATE} ${INTERVAL}`

end # date loop
