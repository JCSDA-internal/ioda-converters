#!/usr/bin/env bash
#
# Script to create test files, geovals and obs, from GSI netcdf output files.
#

# Have radiosonde and amsua for now.


cat_files=0
select_files=0
prep_files=1

if (($cat_files == 1))
then
  mkdir -p AllObs

  echo "Concatenating Radiosonde netcdf files:"
  for Var in ps t q uv
  do
    Cmd="cat_nc_files.py -o AllObs/sondes_obs_geoval_${Var}_01.nc4 Sondes_2018041500/*/conv_obs_geoval_${Var}_01.nc4"
    echo "Running: $Cmd"
    $Cmd
  done
  echo

  echo "Concatenating Aircraft netcdf files:"
  for Var in ps t q uv
  do
    Cmd="cat_nc_files.py -o AllObs/aircraft_obs_geoval_${Var}_01.nc4 Aircraft_2018041500/*/conv_obs_geoval_${Var}_01.nc4"
    echo "Running: $Cmd"
    $Cmd
  done
  echo

  echo "Concatenating AMSU-A netcdf files:"
  for Sat in aqua metop-a metop-b n15 n18 n19
  do
    Cmd="cat_nc_files.py -o AllObs/amsua_obs_geoval_${Sat}_01.nc4 Amsua_2018041500/*/amsua_obs_geoval_${Sat}_01.nc4"
    echo "Running: $Cmd"
    $Cmd
  done
  echo
fi


if (($select_files == 1))
then
  mkdir -p HundredsObs OneObs

  echo "Selecting subset of Radiosonde obs from netcdf files:"
  NumRecs=24
  for Var in t q uv
  do
    # one obs (record)
    Cmd="select_nc_obs.py Sondes -s AllObs/RadiosondeStationIds 1 AllObs/sondes_obs_geoval_${Var}_01.nc4 OneObs/sondes_obs_geoval_${Var}_01.nc4"

    echo "Running: $Cmd"
    $Cmd

    # hundreds of obs
    Cmd="select_nc_obs.py Sondes -s AllObs/RadiosondeStationIds ${NumRecs} AllObs/sondes_obs_geoval_${Var}_01.nc4 HundredsObs/sondes_obs_geoval_${Var}_01.nc4"
    echo "Running: $Cmd"
    $Cmd
  done
  echo

  echo "Selecting subset of Aircraft obs from netcdf files:"
  NumRecs=30
  for Var in t q uv
  do
    # one obs (record)
    Cmd="select_nc_obs.py Aircraft -a AllObs/AircraftStationIds 1 AllObs/aircraft_obs_geoval_${Var}_01.nc4 OneObs/aircraft_obs_geoval_${Var}_01.nc4"

    echo "Running: $Cmd"
    $Cmd

    # hundreds of obs
    Cmd="select_nc_obs.py Aircraft -a AllObs/AircraftStationIds ${NumRecs} AllObs/aircraft_obs_geoval_${Var}_01.nc4 HundredsObs/aircraft_obs_geoval_${Var}_01.nc4"
    echo "Running: $Cmd"
    $Cmd
  done
  echo

  echo "Selecting subset of AMSU-A obs from netcdf files:"
  NumRecs=75
  for Sat in aqua metop-a metop-b n15 n18 n19
  do
    # one obs (record)
    Cmd="select_nc_obs.py Amsua 1 AllObs/amsua_obs_geoval_${Sat}_01.nc4 OneObs/amsua_obs_geoval_${Sat}_01.nc4"
    echo "Running: $Cmd"
    $Cmd

    # hundreds of obs
    Cmd="select_nc_obs.py Amsua ${NumRecs} AllObs/amsua_obs_geoval_${Sat}_01.nc4 HundredsObs/amsua_obs_geoval_${Sat}_01.nc4"
    echo "Running: $Cmd"
    $Cmd
  done
  echo
fi

if (($prep_files == 1))
then
  mkdir -p GeoObs

  echo "Preparing Radiosonde geovals and obs netcdf files:"
  # one obs (record)
  Cmd="prep_nc_files.py Sondes -t OneObs/sondes_obs_geoval_t_01.nc4 -q OneObs/sondes_obs_geoval_q_01.nc4 -uv OneObs/sondes_obs_geoval_uv_01.nc4 GeoObs/sondes_geoval_2018041500_s.nc4 GeoObs/sondes_obs_2018041500_s.nc4"
  echo "Running: $Cmd"
  $Cmd

  # hundreds of obs
  Cmd="prep_nc_files.py Sondes -t HundredsObs/sondes_obs_geoval_t_01.nc4 -q HundredsObs/sondes_obs_geoval_q_01.nc4 -uv HundredsObs/sondes_obs_geoval_uv_01.nc4 GeoObs/sondes_geoval_2018041500_m.nc4 GeoObs/sondes_obs_2018041500_m.nc4"
  echo "Running: $Cmd"
  $Cmd
  echo

  echo "Preparing Aircraft geovals and obs netcdf files:"
  # one obs (record)
  Cmd="prep_nc_files.py Aircraft -t OneObs/aircraft_obs_geoval_t_01.nc4 -q OneObs/aircraft_obs_geoval_q_01.nc4 -uv OneObs/aircraft_obs_geoval_uv_01.nc4 GeoObs/aircraft_geoval_2018041500_s.nc4 GeoObs/aircraft_obs_2018041500_s.nc4"
  echo "Running: $Cmd"
  $Cmd

  # hundreds of obs
  Cmd="prep_nc_files.py Aircraft -t HundredsObs/aircraft_obs_geoval_t_01.nc4 -q HundredsObs/aircraft_obs_geoval_q_01.nc4 -uv HundredsObs/aircraft_obs_geoval_uv_01.nc4 GeoObs/aircraft_geoval_2018041500_m.nc4 GeoObs/aircraft_obs_2018041500_m.nc4"
  echo "Running: $Cmd"
  $Cmd
  echo

  echo "Preparing AMSU-A geovals and obs netcdf files:"
  for Sat in aqua metop-a metop-b n15 n18 n19
  do
    # one obs
    Cmd="prep_nc_files.py Amsua -r OneObs/amsua_obs_geoval_${Sat}_01.nc4 GeoObs/amsua_geoval_${Sat}_2018041500_s.nc4 GeoObs/amsua_obs_${Sat}_2018041500_s.nc4"
    echo "Running: $Cmd"
    $Cmd

    # hundreds of obs
    Cmd="prep_nc_files.py Amsua -r HundredsObs/amsua_obs_geoval_${Sat}_01.nc4 GeoObs/amsua_geoval_${Sat}_2018041500_m.nc4 GeoObs/amsua_obs_${Sat}_2018041500_m.nc4"
    echo "Running: $Cmd"
    $Cmd
  done
  echo
fi

