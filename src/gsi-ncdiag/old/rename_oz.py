#!/usr/bin/env python

"""Renaming GSI conv files to IODA naming convention"""
from netCDF4 import Dataset
import numpy as np
import os.path
from oz_dicts import geovals_metadata_dict, loc_metadata_dict, variables_dict, \
                      geovals_dict

def write_to_obsfile(din, fileout):
  dout = Dataset(fileout, 'w')
  print "Writing observations to ", fileout
  # Copy attributes
  dout.setncattr("date_time", din.getncattr("date_time"))
  nlocs = len(din.dimensions['nobs'])
  dout.createDimension("nlocs", nlocs)
  dout.createDimension("nrecs", nlocs)
  dout.createDimension("nvars", 1)
  # Copy vars, rename and transform (for inverse errors and O-F)
  for var in din.variables.values():
    vname = var.name
    if (vname in loc_metadata_dict.keys()) or (vname in variables_dict.keys()):
      print "   Variable: ", vname
      if vname in loc_metadata_dict.keys():
        var_out = dout.createVariable(loc_metadata_dict[vname]+"@MetaData", \
                                      var.dtype, ('nlocs',))
        vdata = var[:]
        var_out[:] = vdata
      elif vname in variables_dict.keys():
        vdata = var[:]
        if "unadjusted" in vname:
          vdata_obs = din["Observation"][:]
        elif "_adjusted" in vname:
          vdata_hx = din["Obs_Minus_Forecast_unadjusted"][:]
        var_out = dout.createVariable("mass_concentration_of_ozone_in_air@"+variables_dict[vname], \
                                      var.dtype, ('nlocs'))
        if "Inverse" in vname:
           var_out[:] = 1.0 / vdata
        elif "unadjusted" in vname:
           var_out[:] = vdata_obs - vdata
        elif "_adjusted" in vname:
           var_out[:] = vdata - vdata_hx
        else:
           var_out[:] = vdata
  dout.close()

def write_to_geovalsfile(din, fileout):
  dout = Dataset(fileout, 'w')
  print "Writing geovals to ", fileout
  # Copy attributes
  dout.setncattr("date_time", din.getncattr("date_time"))
  # Copy dimensions
  nlocs   = len(din.dimensions['nobs'])
  dout.createDimension("nlocs", nlocs)
  dout.createDimension("nlevs", len(din.dimensions["mass_concentration_of_ozone_in_air_arr_dim"]))
  dout.createDimension("nlevsp1", len(din.dimensions["air_pressure_levels_arr_dim"]))
  # Copy vars, rename
  for var in din.variables.values():
    vname = var.name
    if (vname in geovals_metadata_dict.keys()) or (vname in geovals_dict.keys()):
      print "   Variable: ", vname
      if vname in geovals_metadata_dict.keys():
        var_out = dout.createVariable(geovals_metadata_dict[vname], \
                                      var.dtype, ('nlocs',))
        vdata = var[:]
        var_out[:] = vdata
      elif vname in geovals_dict.keys():
        if (len(var.dimensions) == 1):
          dims = ("nlocs",)
        elif "_levels" in vname:
          dims = ("nlocs", "nlevsp1")
        else:
          dims = ("nlocs", "nlevs")
        var_out = dout.createVariable(geovals_dict[vname], var.dtype, dims)
        vdata = var[...]
        var_out[...] = vdata
  dout.close()

instrs = ['gome_metop-a', 'gome_metop-b', 'sbuv2_n19']
date='2018041500'
for instr in instrs:
  fname = "diag_"+instr+"_ges."+date+"_ensmean.nc4"
  if os.path.isfile(fname):
    print "Processing ", fname
    d = Dataset(fname, 'r')
    write_to_obsfile(d, instr+"_obs_"+date+".nc4")
    write_to_geovalsfile(d, instr+"_geoval_"+date+".nc4")
    d.close()

