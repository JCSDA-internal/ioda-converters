#!/usr/bin/env python3
# this is a quick script to convert
# netCDF satbias files from the old to new format
import argparse
import netCDF4 as nc

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

"""
def satbias_upgrader(infile, outfile):
    # convert satbias files from old to new format

    # open the input file
    oldnc = nc.Dataset(infile, 'r')

    # get the list of predictors
    preds = oldnc.variables['predictors'][:]




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
