#!/usr/bin/env python3
# this is a quick script to convert
# netCDF satbias files from the old to new format
import argparse
import netCDF4 as nc
import numpy as np

"""
The old format is like so:
dimensions:
	nchannels = 15 ;
	npredictors = 2 ;
variables:
	float bias_coefficients(npredictors, nchannels) ;
		bias_coefficients:_FillValue = -3.368795e+38f ;
	int channels(nchannels) ;
		channels:_FillValue = -2147483647 ;
	int nchannels(nchannels) ;
		nchannels:suggested_chunk_dim = 15LL ;
	int npredictors(npredictors) ;
		npredictors:suggested_chunk_dim = 2LL ;
	string predictors(npredictors) ;
		string predictors:_FillValue = "" ;

// global attributes:
		string :_ioda_layout = "ObsGroup" ;
		:_ioda_layout_version = 0 ;


The updated format will be the following:

dimensions:
	nrecs = 1 ;
	nvars = 15 ;
variables:
	int channels(nvars) ;
		channels:_FillValue = -2147483647 ;
	int nrecs(nrecs) ;
		nrecs:suggested_chunk_dim = 1LL ;
	int number_obs_assimilated(nrecs, nvars) ;
		number_obs_assimilated:_FillValue = -2147483647 ;
	int nvars(nvars) ;
		nvars:suggested_chunk_dim = 15LL ;

// global attributes:
		string :_ioda_layout = "ObsGroup" ;
		:_ioda_layout_version = 0 ;

group: biasCoefficients {
  variables:
  	float cloud_liquid_water(nrecs, nvars) ;
  		cloud_liquid_water:_FillValue = -3.368795e+38f ;
  	float constant(nrecs, nvars) ;
  		constant:_FillValue = -3.368795e+38f ;
  	float cosine_of_latitude_times_orbit_node(nrecs, nvars) ;
  		cosine_of_latitude_times_orbit_node:_FillValue = -3.368795e+38f ;
  	float emissivity(nrecs, nvars) ;
  		emissivity:_FillValue = -3.368795e+38f ;
  	float lapse_rate(nrecs, nvars) ;
  		lapse_rate:_FillValue = -3.368795e+38f ;
  	float lapse_rate_order_2(nrecs, nvars) ;
  		lapse_rate_order_2:_FillValue = -3.368795e+38f ;
  	float scan_angle(nrecs, nvars) ;
  		scan_angle:_FillValue = -3.368795e+38f ;
  	float scan_angle_order_2(nrecs, nvars) ;
  		scan_angle_order_2:_FillValue = -3.368795e+38f ;
  	float scan_angle_order_3(nrecs, nvars) ;
  		scan_angle_order_3:_FillValue = -3.368795e+38f ;
  	float scan_angle_order_4(nrecs, nvars) ;
  		scan_angle_order_4:_FillValue = -3.368795e+38f ;
  	float sine_of_latitude(nrecs, nvars) ;
  		sine_of_latitude:_FillValue = -3.368795e+38f ;
  	float zenith_angle(nrecs, nvars) ;
  		zenith_angle:_FillValue = -3.368795e+38f ;
  } // group biasCoefficients
}

"""
def satbias_upgrader(infile, outfile):
    # convert satbias files from old to new format

    # open the input file
    oldnc = nc.Dataset(infile, 'r')

    # get the list of predictors
    predictors = oldnc.variables['predictors'][:]

    # open the output file for writing
    newnc = nc.Dataset(outfile, 'w')

    # global attributes
    newnc._ioda_layout = "ObsGroup"
    newnc._ioda_layout_version = np.int32(0)

    # create dimensions
    nrecs = newnc.createDimension("nrecs", 1)
    nvars = newnc.createDimension("nvars", len(oldnc.dimensions['nchannels']))

    # create top level variables
    channels_in = oldnc.variables['channels'][:]
    channels_out = newnc.createVariable("channels","i4",("nvars",))
    channels_out[:] = channels_in
    nrecs_out = newnc.createVariable("nrecs", "i4", ("nrecs"))
    nvars_out = newnc.createVariable("nvars", "i4", ("nvars"))
    nrecs_out[:] = 0
    nvars_out[:] = 0
    nobs_assim_in = oldnc.variables['number_obs_assimilated'][:]
    nobs_assim_out = newnc.createVariable("number_obs_assimilated", "i4", ("nrecs", "nvars"))
    nobs_assim_out[0,:] = nobs_assim_in

    # loop through predictors and create predictor variables
    if 'bias_coefficients' in oldnc.variables.keys():
        bias_coeff = oldnc.variables['bias_coefficients'][:]
        for i, pred in enumerate(predictors):
            var1_out = newnc.createVariable(f"biasCoefficients/{pred}", "f4", ("nrecs", "nvars"),
                                            fill_value=-3.36879526e+38)
            var1_out[0,:] = bias_coeff[i,:]
    if 'bias_coeff_errors' in oldnc.variables.keys():
        bias_coeff_err = oldnc.variables['bias_coeff_errors'][:]
        for i, pred in enumerate(predictors):
            var2_out = newnc.createVariable(f"biasCoeffErrors/{pred}", "f4", ("nrecs", "nvars"),
                                            fill_value=-3.36879526e+38)
            var2_out[0,:] = bias_coeff_err[i,:]


def main():
    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Converts netCDF satbias and satbias_cov files '
            'from the old format used by UFO to the new format.'
        )
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of input satbias netCDF file",
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of output converted satbias netCDF file",
        type=str, required=True)
    args = parser.parse_args()

    # run the converter function
    satbias_upgrader(args.input, args.output)


if __name__ == "__main__":
    main()
