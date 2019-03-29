#!/usr/bin/env python

"""Splitting GSI ncdiag conv_t, conv_q, conv_uv files into files for different platforms"""
from netCDF4 import Dataset
import numpy as np

def write_to_file(din, fileout, nobs, flag):
  """Copies variable from GSI ncdiag (din) file into ncdiag (dout)
     nobs: number of unique obs to copy,
     flag: which obs to copy from the original file"""

  dout = Dataset(fileout, 'w')
  print "    Writing: ", fileout, " nobs=", nobs
  # Copy attributes
  for aname in din.ncattrs():
    avalue = din.getncattr(aname)
    dout.setncattr(aname, avalue)

  # Copy dimensions
  for dim in din.dimensions.values():
    if (dim.name == "nobs"):
      d_size = nobs
    else:
      d_size = len(dim)
    dout.createDimension(dim.name, d_size)

  # Copy vars, trim them down to the selected obs denoted in
  # arugument 'flag'.
  for var in din.variables.values():
    vname = var.name
    print "        Variable: ", vname
    vdata = var[...].data
    dims = tuple([len(din.dimensions[d]) for d in var.dimensions])
    vdata = np.frombuffer(vdata,dtype=var.dtype)
    vdata = np.reshape(vdata,dims)
    var_out = dout.createVariable(vname, var.dtype, var.dimensions)
    var_out[...] = vdata[flag,...]
  dout.close()

########################
# MAIN
########################

date='2018041500'

in_fname = "diag_conv_t_ges."+date+"_ensmean.nc4"
d = Dataset(in_fname, 'r')

# Setup_QC_Mark contains an "iqt" flag: if iqt==0, observation is of virtual temperature
#                                       else,      observation is of sensible temperature
iqt = d['Setup_QC_Mark'][:]

print "Splitting conventional obs file: ", in_fname, ", nobs=", len(iqt)

ind_tv = (iqt == 0)
ind_ts = (iqt != 0)

# Observation_Type contains obs code, see http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_2.htm for details
code = d['Observation_Type'][:]

ind_sond = np.logical_and(code >= 120, code <= 122)
ind_rass = (code == 126)
ind_acft = np.logical_and(code >= 130, code <= 139)
ind_ship = np.logical_or (code == 180, code == 183)
ind_surf = np.logical_or (code == 181, code == 187)

# Copy aircraft temperature data (only sensible temperature is observed)
write_to_file(d, "diag_aircraft_tsen_"+date+".nc4", len(iqt[ind_acft]), ind_acft)

# Copy sondes virtual temperature data
ind = np.logical_and(ind_tv, ind_sond)
write_to_file(d, "diag_sondes_tv_"+date+".nc4", len(iqt[ind]), ind)

# Copy sondes sensible temperature data
ind = np.logical_and(ind_ts, ind_sond)
write_to_file(d, "diag_sondes_tsen_"+date+".nc4", len(iqt[ind]), ind)

# Copy  RASS data
write_to_file(d, "diag_rass_tsen_"+date+".nc4", len(iqt[ind_rass]), ind_rass)

# Copy ship temperature data
write_to_file(d, "diag_sfcship_tsen_"+date+".nc4", len(iqt[ind_ship]), ind_ship)

# Copy surface station temperature data
write_to_file(d, "diag_sfc_tsen_"+date+".nc4", len(iqt[ind_surf]), ind_surf)

d.close()

# Open humidity GSI ncdiag file
in_fname = "diag_conv_q_ges."+date+"_ensmean.nc4"
d = Dataset(in_fname, 'r')

# Observation_Type contains obs code
code = d['Observation_Type'][:]

print "Splitting conventional obs file: ", in_fname, ", nobs=", len(code)

ind_sond = np.logical_and(code >= 120, code <= 122)
ind_acft = np.logical_and(code >= 130, code <= 139)
ind_ship = np.logical_or (code == 180, code == 183)
ind_surf = np.logical_or (code == 181, code == 187)

## Copy aircraft humidity data
write_to_file(d, "diag_aircraft_q_"+date+".nc4", len(code[ind_acft]), ind_acft)

# Copy sondes humidity data
write_to_file(d, "diag_sondes_q_"+date+".nc4", len(code[ind_sond]), ind_sond)

# Copy ship humidity data
write_to_file(d, "diag_sfcship_q_"+date+".nc4", len(code[ind_ship]), ind_ship)

# Copy surface station humidity data
write_to_file(d, "diag_sfc_q_"+date+".nc4", len(code[ind_surf]), ind_surf)

d.close()

# Open surface pressure GSI ncdiag file
in_fname = "diag_conv_ps_ges."+date+"_ensmean.nc4"
d = Dataset(in_fname, 'r')

# Observation_Type contains obs code
code = d['Observation_Type'][:]

print "Splitting conventional obs file: ", in_fname, ", nobs=", len(code)

ind_sond = np.logical_and(code >= 120, code <= 122)
ind_ship = np.logical_or (code == 180, code == 183)
ind_surf = np.logical_or (code == 181, code == 187)

# Copy sondes ps data
write_to_file(d, "diag_sondes_ps_"+date+".nc4", len(code[ind_sond]), ind_sond)

# Copy ship ps data
write_to_file(d, "diag_sfcship_ps_"+date+".nc4", len(code[ind_ship]), ind_ship)

# Copy surface station ps data
write_to_file(d, "diag_sfc_ps_"+date+".nc4", len(code[ind_surf]), ind_surf)

d.close()

# Open tmeprature GSI ncdiag file
in_fname = "diag_conv_uv_ges."+date+"_ensmean.nc4"
d = Dataset(in_fname, 'r')

# Observation_Type contains obs code, see http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_2.htm for details
code = d['Observation_Type'][:]

print "Splitting conventional obs file: ", in_fname, len(code), " nobs"

ind_sond = np.logical_and(code >= 220, code <= 222)
ind_acft = np.logical_and(code >= 230, code <= 239)
ind_satw = np.logical_and(code >= 240, code <= 260)
ind_vad  = (code == 224)
ind_prof = np.logical_and(code >= 227, code <= 229)
ind_ship = np.logical_or (code == 280, code == 282)
ind_ship = np.logical_or (ind_ship,    code == 284)
ind_surf = np.logical_or (code == 281, code == 287)
ind_scat = (code == 290)

# Copy aircraft wind data
write_to_file(d, "diag_aircraft_uv_"+date+".nc4", len(code[ind_acft]), ind_acft)

# Copy sondes wind data
write_to_file(d, "diag_sondes_uv_"+date+".nc4", len(code[ind_sond]), ind_sond)

# Copy satwinds wind data
write_to_file(d, "diag_satwind_uv_"+date+".nc4", len(code[ind_satw]), ind_satw)

# Copy vad wind data
write_to_file(d, "diag_vadwind_uv_"+date+".nc4", len(code[ind_vad]), ind_vad)

# Copy profiler wind data
write_to_file(d, "diag_windprof_uv_"+date+".nc4", len(code[ind_prof]), ind_prof)

# Copy surface ship wind data
write_to_file(d, "diag_sfcship_uv_"+date+".nc4", len(code[ind_ship]), ind_ship)

# Copy surface station wind data
write_to_file(d, "diag_sfc_uv_"+date+".nc4", len(code[ind_surf]), ind_surf)

# Copy scatterometer wind data
write_to_file(d, "diag_scatwind_uv_"+date+".nc4", len(code[ind_scat]), ind_scat)

d.close()

