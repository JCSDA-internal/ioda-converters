#!/usr/bin/env python3
# this is a quick script to convert
# netCDF satbias files from the old to new format
import argparse
import netCDF4 as nc
import numpy as np


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
    if 'channels' in oldnc.variables.keys():
        channels_in = oldnc.variables['channels'][:]
        channels_out = newnc.createVariable("channels", "i4", ("nvars",))
        channels_out[:] = channels_in
    if 'variables' in oldnc.variables.keys():
        vars_in = oldnc.variables['variables'][:]
        vars_out = newnc.createVariable("variables", str, ("nvars",))
        vars_out[:] = vars_in
    nrecs_out = newnc.createVariable("nrecs", "i4", ("nrecs"))
    nvars_out = newnc.createVariable("nvars", "i4", ("nvars"))
    nrecs_out[:] = 0
    nvars_out[:] = 0
    if 'number_obs_assimilated' in oldnc.variables.keys():
        nobs_assim_in = oldnc.variables['number_obs_assimilated'][:]
        nobs_assim_out = newnc.createVariable("number_obs_assimilated", "i4", ("nrecs", "nvars"))
        nobs_assim_out[0, :] = nobs_assim_in

    # loop through predictors and create predictor variables
    if 'bias_coefficients' in oldnc.variables.keys():
        bias_coeff = oldnc.variables['bias_coefficients'][:]
        for i, pred in enumerate(predictors):
            var1_out = newnc.createVariable(f"biasCoefficients/{pred}", "f4", ("nrecs", "nvars"),
                                            fill_value=-3.36879526e+38)
            var1_out[0, :] = bias_coeff[i, :]
    if 'bias_coeff_errors' in oldnc.variables.keys():
        bias_coeff_err = oldnc.variables['bias_coeff_errors'][:]
        for i, pred in enumerate(predictors):
            var2_out = newnc.createVariable(f"biasCoeffErrors/{pred}", "f4", ("nrecs", "nvars"),
                                            fill_value=-3.36879526e+38)
            var2_out[0, :] = bias_coeff_err[i, :]


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
