#!/usr/bin/env bash
#
# Script to select out obs for the medium and small data sets
#
build_sonde=1
build_aircraft=1
build_satwind=0
build_amsua=1

AllObsDir="GsiFullObsData"
MedObsDir="GsiMediumObsData"
SmObsDir="GsiSmallObsData"

GeoObsDir="GeoObsData"

PATH=$HOME/tools/bin:$PATH


if (($build_sonde == 1))
then
  ######################################################
  # Radiosondes
  ######################################################
  
  echo "Preparing Radiosonde geovals and obs netcdf files:"
  
  # small selection (one record)
  Cmd="prep_nc_files.py -c Sondes \
      -t ${SmObsDir}/diag_conv_sond_tsen.nc4 \
      -q ${SmObsDir}/diag_conv_sond_q.nc4 \
      -uv ${SmObsDir}/diag_conv_sond_uv.nc4 \
      ${GeoObsDir}/sondes_tsen_geovals_2018041500_s.nc4 \
      ${GeoObsDir}/sondes_tsen_obs_2018041500_s.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  Cmd="prep_nc_files.py -c Sondes \
      -t ${SmObsDir}/diag_conv_sond_tv.nc4 \
      -q ${SmObsDir}/diag_conv_sond_q.nc4 \
      -uv ${SmObsDir}/diag_conv_sond_uv.nc4 \
      ${GeoObsDir}/sondes_tv_geovals_2018041500_s.nc4 \
      ${GeoObsDir}/sondes_tv_obs_2018041500_s.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  ## medium selection
  Cmd="prep_nc_files.py -c Sondes \
      -t ${MedObsDir}/diag_conv_sond_tsen.nc4 \
      -q ${MedObsDir}/diag_conv_sond_q.nc4 \
      -uv ${MedObsDir}/diag_conv_sond_uv.nc4 \
      ${GeoObsDir}/sondes_tsen_geovals_2018041500_m.nc4 \
      ${GeoObsDir}/sondes_tsen_obs_2018041500_m.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  Cmd="prep_nc_files.py -c Sondes \
      -t ${MedObsDir}/diag_conv_sond_tv.nc4 \
      -q ${MedObsDir}/diag_conv_sond_q.nc4 \
      -uv ${MedObsDir}/diag_conv_sond_uv.nc4 \
      ${GeoObsDir}/sondes_tv_geovals_2018041500_m.nc4 \
      ${GeoObsDir}/sondes_tv_obs_2018041500_m.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  ## full selection
  Cmd="prep_nc_files.py -c Sondes \
      -t ${AllObsDir}/diag_conv_sond_tsen.nc4 \
      -q ${AllObsDir}/diag_conv_sond_q.nc4 \
      -uv ${AllObsDir}/diag_conv_sond_uv.nc4 \
      ${GeoObsDir}/sondes_tsen_geovals_2018041500_f.nc4 \
      ${GeoObsDir}/sondes_tsen_obs_2018041500_f.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  Cmd="prep_nc_files.py -c Sondes \
      -t ${AllObsDir}/diag_conv_sond_tv.nc4 \
      -q ${AllObsDir}/diag_conv_sond_q.nc4 \
      -uv ${AllObsDir}/diag_conv_sond_uv.nc4 \
      ${GeoObsDir}/sondes_tv_geovals_2018041500_f.nc4 \
      ${GeoObsDir}/sondes_tv_obs_2018041500_f.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  echo
fi

if (($build_aircraft == 1))
then
  ######################################################
  # Aircraft
  ######################################################
  
  echo "Preparing Aircraft geovals and obs netcdf files:"
  # small selection (one record)
  Cmd="prep_nc_files.py -c Aircraft \
      -t ${SmObsDir}/diag_conv_acft_tsen.nc4 \
      -q ${SmObsDir}/diag_conv_acft_q.nc4 \
      -uv ${SmObsDir}/diag_conv_acft_uv.nc4 \
      ${GeoObsDir}/aircraft_tsen_geovals_2018041500_s.nc4 \
      ${GeoObsDir}/aircraft_tsen_obs_2018041500_s.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  # medium selection
  Cmd="prep_nc_files.py -c Aircraft \
      -t ${MedObsDir}/diag_conv_acft_tsen.nc4 \
      -q ${MedObsDir}/diag_conv_acft_q.nc4 \
      -uv ${MedObsDir}/diag_conv_acft_uv.nc4 \
      ${GeoObsDir}/aircraft_tsen_geovals_2018041500_m.nc4 \
      ${GeoObsDir}/aircraft_tsen_obs_2018041500_m.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  # full selection
  Cmd="prep_nc_files.py -c Aircraft \
      -t ${AllObsDir}/diag_conv_acft_tsen.nc4 \
      -q ${AllObsDir}/diag_conv_acft_q.nc4 \
      -uv ${AllObsDir}/diag_conv_acft_uv.nc4 \
      ${GeoObsDir}/aircraft_tsen_geovals_2018041500_f.nc4 \
      ${GeoObsDir}/aircraft_tsen_obs_2018041500_f.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  echo
fi

if (($build_satwind == 1))
then
  ######################################################
  # Satellite wind
  ######################################################
  
  echo "Preparing Satellite Wind geovals and obs netcdf files:"
  # small selection (one record)
  Cmd="prep_nc_files.py -c Aircraft \
      -uv ${SmObsDir}/diag_conv_satw_uv.nc4 \
      ${GeoObsDir}/satwind_tsen_geovals_2018041500_s.nc4 \
      ${GeoObsDir}/satwind_tsen_obs_2018041500_s.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  # medium selection
  Cmd="prep_nc_files.py -c Aircraft \
      -uv ${MedObsDir}/diag_conv_satw_uv.nc4 \
      ${GeoObsDir}/satwind_tsen_geovals_2018041500_m.nc4 \
      ${GeoObsDir}/satwind_tsen_obs_2018041500_m.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  # full selection
  Cmd="prep_nc_files.py -c Aircraft \
      -uv ${AllObsDir}/diag_conv_satw_uv.nc4 \
      ${GeoObsDir}/satwind_tsen_geovals_2018041500_f.nc4 \
      ${GeoObsDir}/satwind_tsen_obs_2018041500_f.nc4"
  echo "Running: $Cmd"
  $Cmd
  
  echo
fi

if (($build_amsua == 1))
then
  echo "Preparing AMSU-A geovals and obs netcdf files:"
  for Sat in aqua metop-a metop-b n15 n18 n19
  do
    # small selection
    Cmd="prep_nc_files.py -c Amsua \
        -r ${SmObsDir}/diag_amsua_${Sat}_ges.2018041500_ensmean.nc4 \
        ${GeoObsDir}/amsua_geovals_${Sat}_2018041500_s.nc4 \
        ${GeoObsDir}/amsua_obs_${Sat}_2018041500_s.nc4"
    echo "Running: $Cmd"
    $Cmd
  
    # medium selection
    Cmd="prep_nc_files.py -c Amsua \
        -r ${MedObsDir}/diag_amsua_${Sat}_ges.2018041500_ensmean.nc4 \
        ${GeoObsDir}/amsua_geovals_${Sat}_2018041500_m.nc4 \
        ${GeoObsDir}/amsua_obs_${Sat}_2018041500_m.nc4"
    echo "Running: $Cmd"
    $Cmd
  
    # full selection
    Cmd="prep_nc_files.py -c Amsua \
        -r ${AllObsDir}/diag_amsua_${Sat}_ges.2018041500_ensmean.nc4 \
        ${GeoObsDir}/amsua_geovals_${Sat}_2018041500_f.nc4 \
        ${GeoObsDir}/amsua_obs_${Sat}_2018041500_f.nc4"
    echo "Running: $Cmd"
    $Cmd
  done
fi 
