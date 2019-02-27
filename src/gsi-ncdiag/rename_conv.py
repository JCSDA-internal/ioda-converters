#!/usr/bin/env python

"""Renaming GSI conv files to IODA naming convention"""
from netCDF4 import Dataset
import numpy as np
import os.path
from conv_dicts import geovals_metadata_dict,metadata_dict, variables_dict, geovals_vars, uv_variables_dict

def write_to_obsfile(din, fileout, filevarnames):
  dout = Dataset(fileout, 'w')
  print "Writing observations to ", fileout
  # Copy attributes
  dout.setncattr("date_time", din.getncattr("date_time"))
  # Copy dimensions
  nlocs = din.dimensions['nobs'].size
  nvars = len(filevarnames)
  dout.createDimension("nlocs", nlocs)
  dout.setncattr("nvars", nvars)
  dimname = "Station_ID_maxstrlen"
  dout.createDimension(dimname, din.dimensions[dimname].size)
  dimname = "Observation_Class_maxstrlen"
  dout.createDimension(dimname, din.dimensions[dimname].size)
  # Copy vars, rename and transform (for inverse errors and O-F)
  for var in din.variables.values():
    vname = var.name
    if (vname in metadata_dict.keys()) or (vname in variables_dict.keys()) \
                                       or (vname in uv_variables_dict.keys()):
      print "   Variable: ", vname
      vdata = var[...].data
      dims = ("nlocs",)+var.dimensions[1:] # replace nobs by nlocs
      if vname in metadata_dict.keys():
        var_out = dout.createVariable(metadata_dict[vname]+"@MetaData", \
                                      vdata.dtype, dims)
        var_out[...] = vdata[...]
      elif vname in variables_dict.keys():
        for filevarname in filevarnames:
          var_out = dout.createVariable(filevarname+"@"+variables_dict[vname], \
                                        vdata.dtype, dims)
          if "Errinv" in vname:
             var_out[...] = 1.0 / vdata[...]
          elif "Forecast" in vname:
             var_out[...] = din["Observation"][:] - vdata[...]
          else:
             var_out[...] = vdata[...]
      elif vname in uv_variables_dict.keys():
        var_out = dout.createVariable(uv_variables_dict[vname], vdata.dtype, dims)
        if "Forecast" in vname:
          if "u_Obs" in vname:
            var_out[...] = din["u_Observation"][:] - vdata[...]
          else:
            var_out[...] = din["v_Observation"][:] - vdata[...]
        else:
          var_out[...] = vdata[...]
  dout.close()

def write_to_geovalsfile(din, fileout):
  dout = Dataset(fileout, 'w')
  print "Writing geovals to ", fileout
  # Copy attributes
  dout.setncattr("date_time", din.getncattr("date_time"))
  # Copy dimensions
  nlocs = din.dimensions['nobs'].size
  dout.createDimension("nlocs", nlocs)
  dout.createDimension("nlevs", din.dimensions["atmosphere_ln_pressure_coordinate_arr_dim"].size)
  dimname = "Station_ID_maxstrlen"
  dout.createDimension(dimname, din.dimensions[dimname].size)
  dimname = "Observation_Class_maxstrlen"
  dout.createDimension(dimname, din.dimensions[dimname].size)
  # Copy vars, rename
  for var in din.variables.values():
    vname = var.name
    if (vname in geovals_metadata_dict.keys()) or (vname in geovals_vars):
      print "   Variable: ", vname
      vdata = var[...].data
      if vname in geovals_metadata_dict.keys():
        dims = ("nlocs",)+var.dimensions[1:]
        var_out = dout.createVariable(geovals_metadata_dict[vname], vdata.dtype, dims)
        var_out[...] = vdata[...]
      if vname in geovals_vars:
        if (len(var.dimensions) == 1):
          dims = ("nlocs",)
        else:
          dims = ("nlocs", "nlevs")
        var_out = dout.createVariable(vname, vdata.dtype, dims)
        var_out[...] = vdata[...]
        if vname == "atmosphere_ln_pressure_coordinate":
          var_out = dout.createVariable("air_pressure", vdata.dtype, dims)
          var_out[...] = 1000. * np.exp(vdata[...])
  dout.close()

vars_dict = {"tv"  : ["virtual_temperature"], \
             "tsen": ["air_temperature"], \
             "uv"  : ["eastward_wind", "northward_wind"], \
             "ps"  : ["surface_pressure"], \
             "q"   : ["specific_humidity"]}

date='2018041500'
pltfs = ['aircraft', 'sfc', 'sfcship', 'sondes', 'windprof', \
         'rass', 'satwind', 'scatwind', 'vadwind' ]
for pltf in pltfs:
  for var in vars_dict.keys():
    fname = "diag_"+pltf+"_"+var+"_"+date+".nc4"
    if os.path.isfile(fname):
      print "Processing ", fname
      d = Dataset(fname, 'r')
      write_to_obsfile(d, pltf+"_"+var+"_obs_"+date+".nc4", vars_dict[var])
      write_to_geovalsfile(d, pltf+"_"+var+"_geoval_"+date+".nc4")
      d.close()
