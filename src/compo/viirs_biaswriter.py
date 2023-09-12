import argparse
import netCDF4
import numpy as np

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
channels_out = [4]
ncfile = netCDF4.Dataset(args.output, mode='w', format='NETCDF4')
nrecs = ncfile.createDimension('nrecs', 1)
nchannels = ncfile.createDimension('nvars', 1)
coef = ncfile.createVariable('biasCoefficients/constant', np.float, ('nrecs', 'nvars'))
coef_error = ncfile.createVariable('biasCoeffErrors/constant', np.float, ('nrecs', 'nvars'))
channels = ncfile.createVariable('channels', np.int32, ('nvars', ))
recs = ncfile.createVariable('nrecs', np.int32, ('nrecs', ))
varvars = ncfile.createVariable('nvars', np.int32, ('nvars', ))
recs[:] = 0
varvars[:] = 0
coef[:] = -0.0163
coef_error[:] = .0863
channels[:] = channels_out
ncfile.close()
