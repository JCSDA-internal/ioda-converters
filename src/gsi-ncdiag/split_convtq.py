"""Splitting GSI ncdiag conv_t and conv_q files into files for different platforms"""
from netCDF4 import Dataset
import numpy as np

def write_to_file(din, dout, nlocs, flag, varname):
  """Copies variable from GSI ncdiag (din) file into IODA-style (dout)
     nlocs: number of unique locs to copy,
     flag: which obs to copy from the original file,
     varname: new variable name"""


  def write_var_1d(datatype, varin, varout):
    dout.createVariable(varout, datatype, ('nlocs'))
    var = din[varin][:]; var = var[flag]
    dout[varout][:] = var

  dout.createDimension('nlocs', nlocs)
  dout.createDimension('Station_ID_maxstrlen', len(din.dimensions['Station_ID_maxstrlen']))
  dout.createDimension('Observation_Class_maxstrlen', len(din.dimensions['Observation_Class_maxstrlen']))
  dout.createDimension('nlev', len(din.dimensions['atmosphere_ln_pressure_coordinate_arr_dim']))

  dout.createVariable('Station_ID@MetaData', 'c', ('nlocs', 'Station_ID_maxstrlen'))
  dout['Station_ID@MetaData'][:,:] = din['Station_ID'][flag,:]
  dout.createVariable('Observation_Class@MetaData', 'c', ('nlocs', 'Observation_Class_maxstrlen'))
  dout['Observation_Class@MetaData'][:,:] = din['Observation_Class'][flag,:]
  write_var_1d('i',  'Observation_Type',  'ObsType@MetaData')
  write_var_1d('f4', 'Latitude',          'Latitude@MetaData')
  write_var_1d('f4', 'Longitude',         'Longitude@MetaData')
  write_var_1d('f4', 'Station_Elevation', 'Station_Elevation@MetaData')
  write_var_1d('f4', 'Pressure',          'Pressure@MetaData')
  write_var_1d('f4', 'Height',            'Height@MetaData')
  write_var_1d('f4', 'Time',              'Time@MetaData')
  write_var_1d('f4', 'Prep_QC_Mark',      varname + '@ObsQC')
  write_var_1d('f4', 'Observation',       varname + '@ObsValue')
  write_var_1d('f4', 'Prep_Use_Flag',     varname + '@PrepUse')
  write_var_1d('f4', 'Analysis_Use_Flag', varname + '@GsiUse')
  write_var_1d('f4', 'Nonlinear_QC_Rel_Wgt', varname + '@GsiQCWgt')
  # invert obs error
  varin = 'Errinv_Input'; varout = varname + '@ObsError'
  dout.createVariable(varout, 'f4', ('nlocs'))
  var = din[varin][:]; var = var[flag]
  dout[varout][:] = 1.0 / var
  # calc GSI H(x)
  varout = varname + '@GsiHofX'
  dout.createVariable(varout, 'f4', ('nlocs'))
  var = din['Observation'][:] - din['Obs_Minus_Forecast_unadjusted'][:];  var = var[flag]
  dout[varout][:] = var
  # invert GSI obs error
  varin = 'Errinv_Final';  varout = varname + '@GsiObsError'
  dout.createVariable(varout, 'f4', ('nlocs'))
  var = din['Errinv_Final'][:];   var = var[flag]
  dout[varout][:] = 1.0 / var
  # calc GSI H(x) with bias correction
  varout = varname + '@GsiHofX_bc'
  dout.createVariable(varout, 'f4', ('nlocs'))
  var = din['Observation'][:] - din['Obs_Minus_Forecast_adjusted'][:];   var = var[flag]
  dout[varout][:] = var
  # GeoVaLs (to be in different file later)
  dout.createVariable('atmosphere_pressure', 'f4', ('nlocs', 'nlev'))
  logp = din['atmosphere_ln_pressure_coordinate'][:,:];   logp = logp[flag,:]
  dout['atmosphere_pressure'][:,:] = np.exp(logp) * 1000.0
  dout.createVariable('atmosphere_ln_pressure_coordinate', 'f4', ('nlocs', 'nlev'))
  dout['atmosphere_ln_pressure_coordinate'][:,:] = logp
  dout.createVariable(varname,  'f4', ('nlocs', 'nlev'))
  var =  din[varname][:,:];  var = var[flag,:]
  dout[varname][:,:] = var

# Open tmeprature GSI ncdiag file
d = Dataset('diag_conv_t_ges.2018041500_ensmean.nc4', 'r')

# Setup_QC_Mark contains an "iqt" flag: if iqt==0, observation is of virtual temperature
#                                       else,      observation is of sensible temperature
iqt = d['Setup_QC_Mark'][:]

ind_tv = (iqt == 0)
ind_ts = (iqt != 0)

# Observation_Type contains obs code, see http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_2.htm for details
code = d['Observation_Type'][:]

ind_sond = np.logical_and(code >= 120, code <= 122)
ind_acft = np.logical_and(code >= 130, code <= 139)

# Copy aircraft temperature data (only sensible temperature is observed)
dts = Dataset('diag_conv_acft_tsen.nc4', 'w')
ind = np.logical_and(ind_ts, ind_acft)
write_to_file(d, dts, len(iqt[ind]), ind, 'air_temperature')
dts.close()

# Copy sondes virtual temperature data
dtv = Dataset('diag_conv_sond_tv.nc4', 'w')
ind = np.logical_and(ind_tv, ind_sond)
write_to_file(d, dtv, len(iqt[ind]), ind, 'virtual_temperature')
dtv.close()

# Copy sondes sensible temperature data
dts = Dataset('diag_conv_sond_tsen.nc4', 'w')
ind = np.logical_and(ind_ts, ind_sond)
write_to_file(d, dts, len(iqt[ind]), ind, 'air_temperature')
dts.close()

d.close()

# Open humidity GSI ncdiag file
d = Dataset('diag_conv_q_ges.2018041500_ensmean.nc4', 'r')

# Observation_Type contains obs code
code = d['Observation_Type'][:]

ind_sond = np.logical_and(code >= 120, code <= 122)
ind_acft = np.logical_and(code >= 130, code <= 139)

# Copy aircraft humidity data
dq = Dataset('diag_conv_acft_q.nc4', 'w')
write_to_file(d, dq, len(code[ind_acft]), ind_acft, 'specific_humidity')
dq.close()

# Copy sondes humidity data
dq = Dataset('diag_conv_sond_q.nc4', 'w')
write_to_file(d, dq, len(code[ind_sond]), ind_sond, 'specific_humidity')
dq.close()

d.close()
