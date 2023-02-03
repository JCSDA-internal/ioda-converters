import argparse
import netCDF4
import numpy as np
import os

# Writes out netCDF file with bias coefficient for VIIRS AOD observations. Currently this
# is just a single global value for the "constant" parameter value. This value is defined in the
# python code rather than read in from a file, and comes from Shobha Kondragunta at NOAA/NESDIS.
# Future implementations will include region, seasonal and AOD magnitude dependencies.

# Usage:
# python viirs_biaswriter.py -o viirs_bias.nc

parser = argparse.ArgumentParser(
    description=('Write VIIRS aerosol optical depth bias coefficients to NetCDF')
)
parser.add_argument(
    '-o', '--output',
    help="name of NetCDF output file",
    type=str, required=True)

args = parser.parse_args()
predictor_out = netCDF4.stringtochar(np.array(['constant'], 'S8'))
channels_out = [4]
ncfile = netCDF4.Dataset(args.output, mode='w', format='NETCDF4')
coef_dim = ncfile.createDimension('bias_coefficients', 1)
coef_err_dim = ncfile.createDimension('bias_coeff_errors', 1)
npredictors = ncfile.createDimension('npredictors', 1)
nchannels = ncfile.createDimension('nchannels', 1)
coef = ncfile.createVariable('bias_coefficients', np.float, ('bias_coefficients', ))
coef_error = ncfile.createVariable('bias_coeff_errors', np.float, ('bias_coeff_errors', ))
predictors = ncfile.createVariable('predictors', 'S8', ('npredictors', ))
channels = ncfile.createVariable('channels', np.int, ('nchannels', ))
coef.units = 'Aerosol optical depth'
coef.long_name = 'bias_coefficients'
coef_error.units = 'Aerosol optical depth'
coef_error.long_name = 'bias_coeff_errors'
n_coef = len(coef_dim)
coef[:] = -0.0163
coef_error[:] = .0863
predictor2 = netCDF4.chartostring(predictor_out)
predictors[:] = predictor2
channels[:] = channels_out
ncfile.close()
