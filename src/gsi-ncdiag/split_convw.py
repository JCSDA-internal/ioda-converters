"""Splitting GSI ncdiag conv_uv file into files for different platforms"""
from netCDF4 import Dataset
import numpy as np

def write_to_file(din, dout, nlocs, flag):
  """Copies variable from GSI ncdiag (din) file into IODA-style (dout)
     nlocs: number of unique locs to copy,
     flag: which obs to copy from the original file"""
  dout.createDimension('nlocs', nlocs)
  dout.createDimension('Station_ID_maxstrlen', len(din.dimensions['Station_ID_maxstrlen']))
  dout.createDimension('Observation_Class_maxstrlen', len(din.dimensions['Observation_Class_maxstrlen']))
  var = dout.createVariable('Station_ID@MetaData', 'c', ('nlocs', 'Station_ID_maxstrlen'))
  var[:,:] = din['Station_ID'][flag,:]
  var = dout.createVariable('Observation_Class@MetaData', 'c', ('nlocs', 'Observation_Class_maxstrlen'))
  var[:,:] = din['Observation_Class'][flag,:]
  var = dout.createVariable('ObsType@MetaData', 'i', ('nlocs'));      var[:] = din['Observation_Type'][flag]
  var = dout.createVariable('ObsSubtype@MetaData', 'i', ('nlocs'));   var[:] = din['Observation_Subtype'][flag]
  var = dout.createVariable('Latitude@MetaData', 'f4', ('nlocs'));    var[:] = din['Latitude'][flag]
  var = dout.createVariable('Longitude@MetaData', 'f4', ('nlocs'));   var[:] = din['Longitude'][flag]
  var = dout.createVariable('Station_Elevation@MetaData', 'f4', ('nlocs'));  var[:] = din['Station_Elevation'][flag]
  var = dout.createVariable('Pressure@MetaData', 'f4', ('nlocs'));    var[:] = din['Pressure'][flag]
  var = dout.createVariable('Height@MetaData', 'f4', ('nlocs'));      var[:] = din['Height'][flag]
  var = dout.createVariable('Time@MetaData', 'f4', ('nlocs'));        var[:] = din['Time'][flag]
  varname = 'eastward_wind'
  var = dout.createVariable(varname + '@ObsQC', 'f4', ('nlocs'));     var[:] = din['Prep_QC_Mark'][flag]
  var = dout.createVariable(varname + '@ObsValue', 'f4', ('nlocs'));  var[:] = din['u_Observation'][flag]
  var = dout.createVariable(varname + '@ObsError', 'f4', ('nlocs'));  var[:] = 1.0 / din['Errinv_Input'][flag]
  var = dout.createVariable(varname + '@PrepUse', 'f4', ('nlocs'));   var[:] = din['Prep_Use_Flag'][flag]
  var = dout.createVariable(varname + '@GsiUse', 'f4', ('nlocs'));    var[:] = din['Analysis_Use_Flag'][flag]
  var = dout.createVariable(varname + '@GsiQCWgt', 'f4', ('nlocs'));  var[:] = din['Nonlinear_QC_Rel_Wgt'][flag]
  var = dout.createVariable(varname + '@GsiHofX', 'f4', ('nlocs'))
  var[:] = din['u_Observation'][flag] - din['u_Obs_Minus_Forecast_unadjusted'][flag]
  var = dout.createVariable(varname + '@GsiObsError', 'f4', ('nlocs')); var[:] = 1.0 / din['Errinv_Final'][flag]
  var = dout.createVariable(varname + '@GsiHofX_bc', 'f4', ('nlocs'))
  var[:] = din['u_Observation'][flag] - din['u_Obs_Minus_Forecast_adjusted'][flag]
  varname = 'northward_wind'
  var = dout.createVariable(varname + '@ObsQC', 'f4', ('nlocs'));     var[:] = din['Prep_QC_Mark'][flag]
  var = dout.createVariable(varname + '@ObsValue', 'f4', ('nlocs'));  var[:] = din['v_Observation'][flag]
  var = dout.createVariable(varname + '@ObsError', 'f4', ('nlocs'));  var[:] = 1.0 / din['Errinv_Input'][flag]
  var = dout.createVariable(varname + '@PrepUse', 'f4', ('nlocs'));   var[:] = din['Prep_Use_Flag'][flag]
  var = dout.createVariable(varname + '@GsiUse', 'f4', ('nlocs'));    var[:] = din['Analysis_Use_Flag'][flag]
  var = dout.createVariable(varname + '@GsiQCWgt', 'f4', ('nlocs'));  var[:] = din['Nonlinear_QC_Rel_Wgt'][flag]
  var = dout.createVariable(varname + '@GsiHofX', 'f4', ('nlocs'))
  var[:] = din['v_Observation'][flag] - din['v_Obs_Minus_Forecast_unadjusted'][flag]
  var = dout.createVariable(varname + '@GsiObsError', 'f4', ('nlocs')); var[:] = 1.0 / din['Errinv_Final'][flag]
  var = dout.createVariable(varname + '@GsiHofX_bc', 'f4', ('nlocs'))
  var[:] = din['v_Observation'][flag] - din['v_Obs_Minus_Forecast_adjusted'][flag]
  
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
