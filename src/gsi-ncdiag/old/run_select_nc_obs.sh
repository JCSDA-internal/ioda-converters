#!/usr/bin/env bash
#
# Script to select out obs for the medium and small data sets
#

AllObsDir="GsiFullObsData"
MedObsDir="GsiMediumObsData"
SmObsDir="GsiSmallObsData"

SondeIdFile="RADIOSONDE_STATION_IDS"
AcftIdFile="AIRCRAFT_STATION_IDS"
SatwIdFile="SATW_STATION_IDS"

PATH=$HOME/tools/bin:$PATH

################################################################
# Radiosondes
################################################################
echo "Selecting subset of Radiosonde obs from netcdf files:"
NumRecs=24
for Var in tv tsen q uv
do
  # one obs (record)
  Cmd="select_nc_obs.py -c Sondes \
       --id_file ${AllObsDir}/${SondeIdFile} 1 \
       ${AllObsDir}/diag_conv_sond_${Var}.nc4 \
       ${SmObsDir}/diag_conv_sond_${Var}.nc4"

  echo "Running: $Cmd"
  $Cmd

  # hundreds of obs
  Cmd="select_nc_obs.py -c Sondes \
       --id_file ${AllObsDir}/${SondeIdFile} ${NumRecs} \
       ${AllObsDir}/diag_conv_sond_${Var}.nc4 \
       ${MedObsDir}/diag_conv_sond_${Var}.nc4"
  echo "Running: $Cmd"
  $Cmd
done
echo

################################################################
# Aircraft
################################################################
echo "Selecting subset of Aircraft obs from netcdf files:"
NumRecs=30
for Var in tsen q uv
do
  # small selection (one record)
  Cmd="select_nc_obs.py -c Aircraft \
       --id_file ${AllObsDir}/${AcftIdFile} 1 \
       ${AllObsDir}/diag_conv_acft_${Var}.nc4 \
       ${SmObsDir}/diag_conv_acft_${Var}.nc4"

  echo "Running: $Cmd"
  $Cmd

  # medium selection
  Cmd="select_nc_obs.py -c Aircraft \
       --id_file ${AllObsDir}/${AcftIdFile} ${NumRecs} \
       ${AllObsDir}/diag_conv_acft_${Var}.nc4 \
       ${MedObsDir}/diag_conv_acft_${Var}.nc4"

  echo "Running: $Cmd"
  $Cmd
done
echo

################################################################
# Satellite Winds
################################################################
echo "Selecting subset of Satellite Wind obs from netcdf files:"
NumRecs=30
for Var in uv
do
  # small selection (one record)
  Cmd="select_nc_obs.py -c Satwind \
       --id_file ${AllObsDir}/${SatwIdFile} 1 \
       ${AllObsDir}/diag_conv_satw_${Var}.nc4 \
       ${SmObsDir}/diag_conv_satw_${Var}.nc4"

  echo "Running: $Cmd"
  $Cmd

  # medium selection
  Cmd="select_nc_obs.py -c Satwind \
       --id_file ${AllObsDir}/${SatwIdFile} ${NumRecs} \
       ${AllObsDir}/diag_conv_satw_${Var}.nc4 \
       ${MedObsDir}/diag_conv_satw_${Var}.nc4"

  echo "Running: $Cmd"
  $Cmd
done
echo


################################################################
# AMSU-A
################################################################

echo "Selecting subset of AMSU-A obs from netcdf files:"
NumRecs=75
for Sat in aqua metop-a metop-b n15 n18 n19
do
  # small selection (one record)
  Cmd="select_nc_obs.py -c Amsua 1 \
      ${AllObsDir}/diag_amsua_${Sat}_ges.2018041500_ensmean.nc4 \
      ${SmObsDir}/diag_amsua_${Sat}_ges.2018041500_ensmean.nc4"
  echo "Running: $Cmd"
  $Cmd

  # medium selection
  Cmd="select_nc_obs.py -c Amsua \
       ${NumRecs} ${AllObsDir}/diag_amsua_${Sat}_ges.2018041500_ensmean.nc4 \
       ${MedObsDir}/diag_amsua_${Sat}_ges.2018041500_ensmean.nc4"
  echo "Running: $Cmd"
  $Cmd
done
