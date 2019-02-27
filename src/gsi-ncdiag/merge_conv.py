#!/usr/bin/env python

"""Splitting GSI ncdiag conv_t, conv_q, conv_uv files into files for different platforms"""
from netCDF4 import Dataset
import numpy as np
import numpy.ma as ma
from conv_dicts import variables_dict, geovals_vars

date="2018041500"

# create ID for obs at each locations (station id, lat, lon, time, pressure)
def create_obs_id(d):
  station_id_read = d['station_id@MetaData'][:]
  station_id = []
  for i in range(len(station_id_read)):
    station_id.append(station_id_read[i].tostring().decode('ascii').strip())
  return zip(*[station_id, d['latitude@MetaData'][:], d['longitude@MetaData'][:], \
               d['air_pressure@MetaData'][:], d['time@MetaData'][:]])

def create_obs_id_single(d, i):
  station_id_read = d['station_id@MetaData'][i]
  station_id = station_id_read.tostring().decode('ascii').strip()
  return (station_id, d['latitude@MetaData'][i], d['longitude@MetaData'][i], \
          d['air_pressure@MetaData'][i], d['time@MetaData'][i])

def create_dimensions(fname):
  d = Dataset(fname+"_obs_"+date+".nc4", 'a')
  nvars = d.getncattr('nvars')
  nlocs = d.dimensions['nlocs'].size
  d.createDimension("nvars", nvars)
  d.createDimension("nrecs", nvars)
  d.createDimension("nobs",  nlocs * nvars)
  d.close()

# merging file (fname) into d
def add_to_obsfile(fname, fname_add):
  # internal function to create the variables
  def add_variables():
    d.setncattr('nvars', d.getncattr('nvars') + d_add.getncattr('nvars'))
    for var in variable_names:
      print var
      var_file = d_add.variables[var]
      d.createVariable(var, var_file.datatype, var_file.dimensions)
  # open dataset which will be updated
  d = Dataset(fname+"_obs_"+date+".nc4", 'a')
  # open dataset to add (append mode to remove the values that
  # has been copied to file d
  d_add = Dataset(fname_add+"_obs_"+date+".nc4", 'a')
  # create IDs for both files
  obsid     = create_obs_id(d)
  print "writing to", fname, " with ", d.dimensions['nlocs'].size, " locs"
  # find all the variables to copy
  variable_names = []
  for var in d_add.variables.values():
    vv = var.name.split("@")
    if ((vv[1] in variables_dict.values()) or (vv[0] == "gsi_wind_red_factor")):
      variable_names.append(var.name)
  # noobs is a flag showing if any matching obs have been found
  # (don't create variables if not)
  noobs = True
  # nwrite is the counter for obs from file fname that were copied
  # into d
  nwrite = 0
  # loop over all locations in fname
  nlocs = d_add.dimensions['nlocs'].size
  for i in range(nlocs):
    obsid_add = create_obs_id_single(d_add, i)
    if (obsid_add in obsid):
      nwrite += 1
      ind = obsid.index(obsid_add)
      if noobs:
        add_variables(); noobs = False
      for var in variable_names:
        d[var][ind]   = d_add[var][i]
        d_add[var][i] = ma.masked
  print "wrote ", nwrite, " out of ", d_add.dimensions['nlocs'].size, " locs from ", fname_add
  if d_add[variable_names[0]][:].count() == 0:
    print fname_add, " now empty!"
  d_add.close()
  d.close()

# Merge winds, humidity into aircraft file
fname = "aircraft_tsen"
add_to_obsfile(fname, "aircraft_q")
add_to_obsfile(fname, "aircraft_uv")
create_dimensions(fname)

# Merge winds, humidity, surface pressure into sondes tsen file
fname = "sondes_tsen"
add_to_obsfile(fname, "sondes_q")
add_to_obsfile(fname, "sondes_uv")
add_to_obsfile(fname, "sondes_ps")
create_dimensions(fname)

# Merge winds, humidity, surface pressure into sondes tv file
fname = "sondes_tv"
add_to_obsfile(fname, "sondes_q")
add_to_obsfile(fname, "sondes_uv")
add_to_obsfile(fname, "sondes_ps")
create_dimensions(fname)

# Merge humidity, surface pressure into land station t file
fname = "sfc_tsen"
add_to_obsfile(fname, "sfc_q")
add_to_obsfile(fname, "sfc_ps")
create_dimensions(fname)

# Merge humidity, surface pressure into ship station t file
fname = "sfcship_tsen"
add_to_obsfile(fname, "sfcship_q")
add_to_obsfile(fname, "sfcship_ps")
create_dimensions(fname)

create_dimensions("satwind_uv")
create_dimensions("scatwind_uv")
create_dimensions("sfc_uv")
create_dimensions("sfcship_uv")
create_dimensions("sondes_uv")
create_dimensions("vadwind_uv")
create_dimensions("windprof_uv")
create_dimensions("rass_tsen")
