#!/usr/bin/env python

"""Splitting GSI ncdiag conv_t, conv_q, conv_uv files into files for different platforms"""
from netCDF4 import Dataset
import numpy as np

def write_to_file(din, dout, nobs, flag):
  """Copies variable from GSI ncdiag (din) file into ncdiag (dout)
     nobs: number of unique obs to copy,
     flag: which obs to copy from the original file"""

  # Copy attributes
  for aname in din.ncattrs():
    avalue = din.getncattr(aname)
    dout.setncattr(aname, avalue)

  # Copy dimensions
  for dim in din.dimensions.values():
    if (dim.name == "nobs"):
      d_size = nobs
    else:
      d_size = dim.size
    dout.createDimension(dim.name, d_size)

  # Copy vars, trim them down to the selected obs denoted in
  # arugument 'flag'.
  for var in din.variables.values():
    vname = var.name
    print "        Variable: ", vname
    vdata = var[...].data
    var_out = dout.createVariable(vname, vdata.dtype, var.dimensions)
    var_out[...] = vdata[flag,...]


########################
# MAIN
########################

# Open tmeprature GSI ncdiag file
in_fname = "diag_conv_t_ges.2018041500_ensmean.nc4"
print "Splitting conventional obs file: ", in_fname
d = Dataset(in_fname, 'r')
print ""

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
out_fname = "diag_conv_acft_tsen.nc4"
print "    Writing: ", out_fname
dts = Dataset(out_fname, 'w')
ind = np.logical_and(ind_ts, ind_acft)
write_to_file(d, dts, len(iqt[ind]), ind)
dts.close()
print ""

# Copy sondes virtual temperature data
out_fname = "diag_conv_sond_tv.nc4"
print "    Writing: ", out_fname
dtv = Dataset(out_fname, 'w')
ind = np.logical_and(ind_tv, ind_sond)
write_to_file(d, dtv, len(iqt[ind]), ind)
dtv.close()
print ""

# Copy sondes sensible temperature data
out_fname = "diag_conv_sond_tsen.nc4"
print "    Writing: ", out_fname
dts = Dataset(out_fname, 'w')
ind = np.logical_and(ind_ts, ind_sond)
write_to_file(d, dts, len(iqt[ind]), ind)
dts.close()
print ""

d.close()

# Open humidity GSI ncdiag file
in_fname = "diag_conv_q_ges.2018041500_ensmean.nc4"
print "Splitting conventional obs file: ", in_fname
d = Dataset(in_fname, 'r')
print ""

# Observation_Type contains obs code
code = d['Observation_Type'][:]

ind_sond = np.logical_and(code >= 120, code <= 122)
ind_acft = np.logical_and(code >= 130, code <= 139)

## Copy aircraft humidity data
out_fname = "diag_conv_acft_q.nc4"
print "    Writing: ", out_fname
dq = Dataset(out_fname, 'w')
write_to_file(d, dq, len(code[ind_acft]), ind_acft)
dq.close()
print ""

# Copy sondes humidity data
out_fname = "diag_conv_sond_q.nc4"
print "    Writing: ", out_fname
dq = Dataset(out_fname, 'w')
write_to_file(d, dq, len(code[ind_sond]), ind_sond)
dq.close()
print ""

d.close()


# Open tmeprature GSI ncdiag file
in_fname = "diag_conv_uv_ges.2018041500_ensmean.nc4"
print "Splitting conventional obs file: ", in_fname
d = Dataset(in_fname, 'r')

# Observation_Type contains obs code, see http://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_2.htm for details
code = d['Observation_Type'][:]

ind_sond = np.logical_and(code >= 220, code <= 222)
ind_acft = np.logical_and(code >= 230, code <= 239)
ind_satw = np.logical_and(code >= 240, code <= 260)

# Copy aircraft wind data
out_fname = "diag_conv_acft_uv.nc4"
print "    Writing: ", out_fname
dnew = Dataset(out_fname, 'w')
write_to_file(d, dnew, len(code[ind_acft]), ind_acft)
dnew.close()

# Copy sondes wind data
out_fname = "diag_conv_sond_uv.nc4"
print "    Writing: ", out_fname
dnew = Dataset(out_fname, 'w')
write_to_file(d, dnew, len(code[ind_sond]), ind_sond)
dnew.close()

# Copy satwinds wind data
out_fname = "diag_conv_satw_uv.nc4"
print "    Writing: ", out_fname
dnew = Dataset(out_fname, 'w')
write_to_file(d, dnew, len(code[ind_satw]), ind_satw)
dnew.close()

d.close()

