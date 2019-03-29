#!/usr/bin/env python

"""Renaming GSI conv files to IODA naming convention"""
from netCDF4 import Dataset
import numpy as np
import os.path
from rad_dicts import geovals_metadata_dict, loc_metadata_dict, variables_dict, \
                      geovals_dict, chan_metadata_dict

def write_to_obsfile(din, fileout):
  dout = Dataset(fileout, 'w')
  print "Writing observations to ", fileout
  # Copy attributes
  dout.setncattr("date_time", din.getncattr("date_time"))
  dout.setncattr("satellite", din.getncattr("Satellite"))
  dout.setncattr("sensor",    din.getncattr("Observation_type"))
  use = din['use_flag'][:]
  chan_number = din['sensor_chan'][:] #[use == 1]
  chan_indx = range(len(chan_number)) #np.where(use==1)[0]
  # get number of channels in obs from init file (includes channels that aren't used)
  nobs_in   = len(din.dimensions['nobs'])
  nchans_in = len(din.dimensions['nchans'])
  # calc number of locations
  nlocs  = nobs_in / nchans_in
  # get number of channels that are used
  nchans = len(chan_number)
  # calc number of obs
  nobs   = nchans * nlocs
  dout.createDimension("nlocs", nlocs)
  dout.createDimension("nrecs", nlocs)
  dout.createDimension("nvars", nchans)
  # Copy vars, rename and transform (for inverse errors and O-F)
  for var in din.variables.values():
    vname = var.name
    if (vname in loc_metadata_dict.keys()) or (vname in variables_dict.keys()) \
                                       or (vname in chan_metadata_dict.keys()):
      print "   Variable: ", vname
      if vname in loc_metadata_dict.keys():
        var_out = dout.createVariable(loc_metadata_dict[vname]+"@MetaData", \
                                      var.dtype, ('nlocs',))
        vdata = var[:]; vdata = vdata[::nchans_in]
        var_out[:] = vdata
      elif vname in chan_metadata_dict.keys():
        var_out = dout.createVariable(chan_metadata_dict[vname]+"@VarMetaData", \
                                      var.dtype, ('nvars',))
        vdata = var[:]; vdata = vdata[chan_indx]
        var_out[:] = vdata
        if vname == "error_variance":
          for i in range(nchans):
            var_out = dout.createVariable("brightness_temperature_"+str(chan_number[i])+"@ObsError", \
                                        var.dtype, ('nlocs'))
            var_out[:] = var[chan_indx[i]]
      elif vname in variables_dict.keys():
        vdata = var[:]
        if "unadjusted" in vname:
          vdata_obs = din["Observation"][:]
        elif "_adjusted" in vname:
          vdata_hx = din["Obs_Minus_Forecast_unadjusted"][:]
        for i in range(nchans):
          var_out = dout.createVariable("brightness_temperature_"+str(chan_number[i])+"@"+variables_dict[vname], \
                                        var.dtype, ('nlocs'))
          vdata_i = vdata[chan_indx[i]::nchans_in]
          if "Inverse" in vname:
             var_out[:] = 1.0 / vdata_i
          elif "unadjusted" in vname:
             vdata_obs_i = vdata_obs[chan_indx[i]::nchans_in]
             var_out[:] = vdata_obs_i - vdata_i
          elif "_adjusted" in vname:
             vdata_hx_i = vdata_hx[chan_indx[i]::nchans_in]
             var_out[:] = vdata_i - vdata_hx_i
          else:
             var_out[:] = vdata_i
  dout.close()

def write_to_geovalsfile(din, fileout):
  dout = Dataset(fileout, 'w')
  print "Writing geovals to ", fileout
  # Copy attributes
  dout.setncattr("date_time", din.getncattr("date_time"))
  dout.setncattr("satellite", din.getncattr("Satellite"))
  dout.setncattr("sensor",    din.getncattr("Observation_type"))
  # Copy dimensions
  nobs   = len(din.dimensions['nobs'])
  nchans = len(din.dimensions['nchans'])
  nlocs = nobs / nchans
  dout.createDimension("nlocs", nlocs)
  dout.createDimension("nlevs", len(din.dimensions["air_temperature_arr_dim"]))
  dout.createDimension("nlevsp1", len(din.dimensions["air_pressure_levels_arr_dim"]))
  # Copy vars, rename
  for var in din.variables.values():
    vname = var.name
    if (vname in geovals_metadata_dict.keys()) or (vname in geovals_dict.keys()):
      print "   Variable: ", vname
      if vname in geovals_metadata_dict.keys():
        var_out = dout.createVariable(geovals_metadata_dict[vname], \
                                      var.dtype, ('nlocs',))
        vdata = var[:];  vdata = vdata[::nchans]
        var_out[:] = vdata
      elif vname in geovals_dict.keys():
        if (len(var.dimensions) == 1):
          dims = ("nlocs",)
        elif "_levels" in vname:
          dims = ("nlocs", "nlevsp1")
        else:
          dims = ("nlocs", "nlevs")
        var_out = dout.createVariable(geovals_dict[vname], var.dtype, dims)
        vdata = var[...];   vdata = vdata[::nchans,...]
        var_out[...] = vdata
  dout.close()

rads = ['airs_aqua', \
        'amsua_aqua', 'amsua_metop-a', 'amsua_metop-b', 'amsua_n15', \
                      'amsua_n18', 'amsua_n19', \
        'atms_npp', \
        'hirs4_metop-a', 'hirs4_metop-b', \
        'iasi_metop-a', 'iasi_metop-b', \
        'mhs_metop-a', 'mhs_metop-b', 'mhs_n18', 'mhs_n19', \
        'seviri_m08', \
        'sndrd1_g15', 'sndrd2_g15', 'sndrd3_g15', 'sndrd4_g15']
#rads = ['amsua_n19']
date='2018041500'
for rad in rads:
  fname = "diag_"+rad+"_ges."+date+"_ensmean.nc4"
  if os.path.isfile(fname):
    print "Processing ", fname
    try:
      d = Dataset(fname, 'r')
      write_to_obsfile(d, rad+"_obs_"+date+".nc4")
      write_to_geovalsfile(d, rad+"_geoval_"+date+".nc4")
      d.close()
    except:
      print(fname+" failed")
      pass

