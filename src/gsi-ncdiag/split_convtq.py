"""Splitting GSI ncdiag conv_t and conv_q files into files for different platforms"""
from netCDF4 import Dataset
import numpy as np

def write_to_file(din, dout, nlocs, flag, varname):
  """Copies variable from GSI ncdiag (din) file into IODA-style (dout)
     nlocs: number of unique locs to copy,
     flag: which obs to copy from the original file,
     varname: new variable name"""
  dout.createDimension('nlocs', nlocs)
  dout.createDimension('Station_ID_maxstrlen', len(din.dimensions['Station_ID_maxstrlen']))
  dout.createDimension('Observation_Class_maxstrlen', len(din.dimensions['Observation_Class_maxstrlen']))
  var = dout.createVariable('Station_ID@MetaData', 'c', ('nlocs', 'Station_ID_maxstrlen'))
  var[:,:] = din['Station_ID'][flag,:]
  var = dout.createVariable('Observation_Class@MetaData', 'c', ('nlocs', 'Observation_Class_maxstrlen'))
  var[:,:] = din['Observation_Class'][flag,:]
  var = dout.createVariable('ObsType@MetaData', 'i', ('nlocs'));      var[:] = din['Observation_Type'][flag]
  var = dout.createVariable('Latitude@MetaData', 'f4', ('nlocs'));    var[:] = din['Latitude'][flag]
  var = dout.createVariable('Longitude@MetaData', 'f4', ('nlocs'));   var[:] = din['Longitude'][flag]
  var = dout.createVariable('Station_Elevation@MetaData', 'f4', ('nlocs'));  var[:] = din['Station_Elevation'][flag]
  var = dout.createVariable('Pressure@MetaData', 'f4', ('nlocs'));    var[:] = din['Pressure'][flag]
  var = dout.createVariable('Height@MetaData', 'f4', ('nlocs'));      var[:] = din['Height'][flag]
  var = dout.createVariable('Time@MetaData', 'f4', ('nlocs'));        var[:] = din['Time'][flag]
  var = dout.createVariable(varname + '@ObsQC', 'f4', ('nlocs'));     var[:] = din['Prep_QC_Mark'][flag]
  var = dout.createVariable(varname + '@ObsValue', 'f4', ('nlocs'));  var[:] = din['Observation'][flag]
  var = dout.createVariable(varname + '@ObsError', 'f4', ('nlocs'));  var[:] = 1.0 / din['Errinv_Input'][flag]
  var = dout.createVariable(varname + '@PrepUse', 'f4', ('nlocs'));   var[:] = din['Prep_Use_Flag'][flag]
  var = dout.createVariable(varname + '@GsiUse', 'f4', ('nlocs'));    var[:] = din['Analysis_Use_Flag'][flag]
  var = dout.createVariable(varname + '@GsiQCWgt', 'f4', ('nlocs'));  var[:] = din['Nonlinear_QC_Rel_Wgt'][flag]
  var = dout.createVariable(varname + '@GsiHofX', 'f4', ('nlocs'))
  var[:] = din['Observation'][flag] - din['Obs_Minus_Forecast_unadjusted'][flag]
  var = dout.createVariable(varname + '@GsiObsError', 'f4', ('nlocs')); var[:] = 1.0 / din['Errinv_Final'][flag]
  var = dout.createVariable(varname + '@GsiHofX_bc', 'f4', ('nlocs'))
  var[:] = din['Observation'][flag] - din['Obs_Minus_Forecast_adjusted'][flag]
  

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
