import conv

fname = '/scratch3/NCEPDEV/stmp1/Cory.R.Martin/JEDI/output/2018041500/GSI_diags/diag_conv_t_ges.2018041500_ensmean.nc4'
conv_t = conv.Conv(fname)
conv_t.read()
print conv_t.nobs
print conv_t.obstype
#conv_t.toIODAobs('./',clobber=False)
conv_t.toIODAobs('./',clobber=True)
print conv_t.obsvars
