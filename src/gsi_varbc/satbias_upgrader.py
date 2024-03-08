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
    nrecs = newnc.createDimension("Record", 1)

    channels = False
    variables = False
    # create top level variables
    if 'channels' in oldnc.variables.keys():
        nvars = newnc.createDimension("Channel", len(oldnc.dimensions['nchannels']))
        channels_in = oldnc.variables['channels'][:]
        channels_out = newnc.createVariable("sensorChannelNumber", "i4", ("Channel",))
        channels_out[:] = channels_in
        dimname = 'Channel'
    if 'variables' in oldnc.variables.keys():
        nvars = newnc.createDimension("Variable", len(oldnc.dimensions['nvariables']))
        vars_in = oldnc.variables['variables'][:]
        vars_out = newnc.createVariable("Variable", str, ("Variable",))
        vars_out[:] = vars_in
        dimname = 'Variable'
    #Hui: TODO once using string Record is fixed in OOPS and UFO, change Record to str.
    #Hui: nrecs_out = newnc.createVariable("Record", str, ("Record"))
    #Hui: nrecs_out[0] = ' '
    nrecs_out = newnc.createVariable("Record", "i4", ("Record"))
    nrecs_out[0] = 0
    if 'number_obs_assimilated' in oldnc.variables.keys():
        nobs_assim_in = oldnc.variables['number_obs_assimilated'][:]
        nobs_assim_out = newnc.createVariable("numberObservationsUsed", "i4", ("Record", dimname))
        nobs_assim_out[0, :] = nobs_assim_in

    # loop through predictors and create predictor variables
    if 'bias_coefficients' in oldnc.variables.keys():
        bias_coeff = oldnc.variables['bias_coefficients'][:]
        for i, pred in enumerate(predictors):
            temp = pred.split('_')
            if "order" in temp:
                idorder=temp.index('order')
                predOut = temp[0] + ''.join(ele.title() for ele in temp[1:idorder])+'_'+temp[idorder]+'_'+temp[idorder+1]
            else:
                predOut = temp[0] + ''.join(ele.title() for ele in temp[1:])
            if 'scanAngle' in predOut:
                predOut = predOut.replace("scanAngle", "sensorScanAngle")
            if 'zenithAngle' in predOut:
                predOut = predOut.replace("zenithAngle", "sensorZenithAngle")
            if 'cloudLiquidWater' in predOut:
                predOut = predOut.replace("cloudLiquidWater", "cloudWaterContent")
            if 'orbialAngle' in predOut:
                predOut = predOut.replace("orbialAngle", "satelliteOrbitalAngle")
            var1_out = newnc.createVariable(f"BiasCoefficients/{predOut}", "f4", ("Record", dimname),
                                            fill_value=-3.36879526e+38)
            var1_out[0, :] = bias_coeff[i, :]
    if 'bias_coeff_errors' in oldnc.variables.keys():
        bias_coeff_err = oldnc.variables['bias_coeff_errors'][:]
        for i, pred in enumerate(predictors):
            temp = pred.split('_')
            if "order" in temp:
                idorder=temp.index('order')
                predOut = temp[0] + ''.join(ele.title() for ele in temp[1:idorder])+'_'+temp[idorder]+'_'+temp[idorder+1]
            else:
                predOut = temp[0] + ''.join(ele.title() for ele in temp[1:])
            if 'scanAngle' in predOut:
                predOut = predOut.replace("scanAngle", "sensorScanAngle")
            if 'zenithAngle' in predOut:
                predOut = predOut.replace("zenithAngle", "sensorZenithAngle")
            if 'cloudLiquidWater' in predOut:
                predOut = predOut.replace("cloudLiquidWater", "cloudWaterContent")
            if 'orbialAngle' in predOut:
                predOut = predOut.replace("orbialAngle", "satelliteOrbitalAngle")
            var2_out = newnc.createVariable(f"BiasCoefficientErrors/{predOut}", "f4", ("Record", dimname),
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
