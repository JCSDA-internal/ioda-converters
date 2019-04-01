import GSI2JEDI

samplefile =GSI2JEDI.GSIDiagFile(
"/scratch3/NCEPDEV/stmp1/Cory.R.Martin/JEDI/output/2018041500/GSI_diags/diag_conv_t_ges.2018041500_ensmean.nc4")
if samplefile.needs_split:
  samplefile.split("./")
