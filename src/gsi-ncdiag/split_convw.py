#!/usr/bin/env python

"""Splitting GSI ncdiag conv_t and conv_q files into files for different platforms"""
from netCDF4 import Dataset
import numpy as np


def write_to_file(din, dout, nlocs, flag):
  """Copies variable from GSI ncdiag (din) file into IODA-style (dout)
     nlocs: number of unique locs to copy,
     flag: which obs to copy from the original file"""

  def write_var_1d(datatype, varin, varout):
    dout.createVariable(varout, datatype, ('nlocs'))
    var = din[varin][:]; var = var[flag]
    dout[varout][:] = var

  # create dimensions
  dout.createDimension('nlocs', nlocs)
  dout.createDimension('Station_ID_maxstrlen', len(din.dimensions['Station_ID_maxstrlen']))
  dout.createDimension('Observation_Class_maxstrlen', len(din.dimensions['Observation_Class_maxstrlen']))
  dout.createDimension('nlev', len(din.dimensions['atmosphere_ln_pressure_coordinate_arr_dim']))

  # copy attributes
  for attr in din.ncattrs():
    dout.setncattr(attr, din.getncattr(attr))

  dout.createVariable('station_id@MetaData', 'c', ('nlocs', 'Station_ID_maxstrlen'))
  dout['station_id@MetaData'][:,:] = din['Station_ID'][flag,:]
  dout.createVariable('observation_class@MetaData', 'c', ('nlocs', 'Observation_Class_maxstrlen'))
  dout['observation_class@MetaData'][:,:] = din['Observation_Class'][flag,:]
  write_var_1d('i',  'Observation_Type',  'obstype@MetaData')
  write_var_1d('f4', 'Latitude',          'latitude@MetaData')
  write_var_1d('f4', 'Longitude',         'longitude@MetaData')
  write_var_1d('f4', 'Station_Elevation', 'station_elevation@MetaData')
  write_var_1d('f4', 'Pressure',          'air_pressure@MetaData')
  write_var_1d('f4', 'Height',            'height@MetaData')
  write_var_1d('f4', 'Time',              'time@MetaData')
  # this time dimension is for geovals
  write_var_1d('f4', 'Time',              'time')
  for varname in ['eastward_wind', 'northward_wind']:
    write_var_1d('f4', 'Prep_QC_Mark',      varname + '@ObsQc')
    write_var_1d('f4', 'Prep_Use_Flag',     varname + '@PrepUse')
    write_var_1d('f4', 'Analysis_Use_Flag', varname + '@GsiUse')
    write_var_1d('f4', 'Nonlinear_QC_Rel_Wgt', varname + '@GsiQcWgt')

  write_var_1d('f4', 'u_Observation',     'eastward_wind@ObsValue')
  write_var_1d('f4', 'v_Observation',     'northward_wind@ObsValue')

  # invert obs error
  varin = 'Errinv_Input'
  var = din[varin][:]; var = var[flag]
  for varname in ['eastward_wind', 'northward_wind']:
    varout = varname + '@ObsError'
    dout.createVariable(varout, 'f4', ('nlocs'))
    dout[varout][:] = 1.0 / var
  # calc GSI H(x)
  varout = 'eastward_wind@GsiHofX'
  dout.createVariable(varout, 'f4', ('nlocs'))
  var = din['u_Observation'][:] - din['u_Obs_Minus_Forecast_unadjusted'][:];  var = var[flag]
  dout[varout][:] = var
  varout = 'northward_wind@GsiHofX'
  dout.createVariable(varout, 'f4', ('nlocs'))
  var = din['v_Observation'][:] - din['v_Obs_Minus_Forecast_unadjusted'][:];  var = var[flag]
  dout[varout][:] = var
  # invert GSI obs error
  varin = 'Errinv_Final'
  var = din[varin][:]; var = var[flag]
  for varname in ['eastward_wind', 'northward_wind']:
    varout = varname + '@GsiObsError'
    dout.createVariable(varout, 'f4', ('nlocs'))
    dout[varout][:] = 1.0 / var
  # calc GSI H(x) with bias correction
  varout = 'eastward_wind@GsiHofX_bc'
  dout.createVariable(varout, 'f4', ('nlocs'))
  var = din['u_Observation'][:] - din['u_Obs_Minus_Forecast_adjusted'][:];   var = var[flag]
  dout[varout][:] = var
  varout = 'northward_wind@GsiHofX_bc'
  dout.createVariable(varout, 'f4', ('nlocs'))
  var = din['v_Observation'][:] - din['v_Obs_Minus_Forecast_adjusted'][:];   var = var[flag]
  dout[varout][:] = var

  # GeoVaLs (to be in different file later)
  dout.createVariable('atmosphere_pressure', 'f4', ('nlocs', 'nlev'))
  logp = din['atmosphere_ln_pressure_coordinate'][:,:];   logp = logp[flag,:]
  dout['atmosphere_pressure'][:,:] = np.exp(logp) * 1000.0
  dout.createVariable('atmosphere_ln_pressure_coordinate', 'f4', ('nlocs', 'nlev'))
  dout['atmosphere_ln_pressure_coordinate'][:,:] = logp
  dout.createVariable('eastward_wind',  'f4', ('nlocs', 'nlev'))
  u = din['eastward_wind'][:,:];   u = u[flag,:]
  dout['eastward_wind'][:,:] = u
  dout.createVariable('northward_wind', 'f4', ('nlocs', 'nlev'))
  v = din['northward_wind'][:,:];  v = v[flag,:]
  dout['northward_wind'][:,:] = v

# Open tmeprature GSI ncdiag file
d = Dataset('diag_conv_uv_ges.2018041500_ensmean.nc4', 'r')

# Observation_Type contains obs code, see http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_2.htm for details
code = d['Observation_Type'][:]

ind_sond = np.logical_and(code >= 220, code <= 222)
ind_acft = np.logical_and(code >= 230, code <= 239)
ind_satw = np.logical_and(code >= 240, code <= 260)

# Copy aircraft wind data
dnew = Dataset('diag_conv_acft_uv.nc4', 'w')
write_to_file(d, dnew, len(code[ind_acft]), ind_acft)
dnew.close()

# Copy sondes wind data
dnew = Dataset('diag_conv_sond_uv.nc4', 'w')
write_to_file(d, dnew, len(code[ind_sond]), ind_sond)
dnew.close()

# Copy satwinds wind data
dnew = Dataset('diag_conv_satw_uv.nc4', 'w')
write_to_file(d, dnew, len(code[ind_satw]), ind_satw)
dnew.close()

d.close()

