#!/usr/bin/env python3
from datetime import datetime
from multiprocessing import Pool
import numpy as np
import os
import pandas as pd
import sys
# sys.path.append("/home/vagrant/jedi/bundle-ioda/build/tools/lib/pyiodaconv")  # dummy before install
sys.path.append("@SCRIPT_LIB_PATH@")
import ioda_conv_ncio as iconv

# (C) Copyright 2019 UCAR
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# Author:
# 2020-11-17: James McCreight

# Conceptual Figure:
# https://jointcenterforsatellitedataassimilation-jedi-docs.readthedocs-hosted.com/en/latest/_images/IODA_InMemorySchematic.png

# Purpose: Convert OWP snow observations from CSV format to IODA format.

# Example testing usage:
# ipython --pdb -c "%run owp_snow_obs_csv_2_ioda.py \
#    -i ../../test/testinput/owp_snow_obs_prelim.csv  \
#    -o ../../test/testoutput/owp_snow_obs_csv.nc \
#    -d 2019021502"

# Input structure:
# jedi@vagrant[395]:~/jedi/bundle-ioda/ioda-converters/src/owp_snow_obs> cat ../../test/testinput/owp_snow_obs_prelim.csv
# ObsValue,ObsError,PreQC,time,latitude,longitude,datetime,variable_name
# 9.96921e+36,9.96921e+36,9.96921e+36,3.0,44.05,-96.6,2019-02-15T05:00:00Z,snow_depth
# 9.96921e+36,9.96921e+36,9.96921e+36,3.0,43.05,-96.15,2019-02-15T05:00:00Z,snow_depth

# Output structure:

# root@8bba783e233d:/jedi/repos/ioda-converters/test/testoutput# ncdump -h owp_snow_obs.nc
# netcdf owp_snow_obs {
# dimensions:
# 	nvars = 1 ;
# 	nlocs = 156 ;
# 	nrecs = 1 ;
# 	nstring = 50 ;
# 	ndatetime = 20 ;
# variables:
# 	float snow_depth@PreQC(nlocs) ;
# 	float snow_depth@ObsError(nlocs) ;
# 	float snow_depth@ObsValue(nlocs) ;
# 	float time@MetaData(nlocs) ;
# 	float latitude@MetaData(nlocs) ;
# 	float longitude@MetaData(nlocs) ;
# 	char datetime@MetaData(nlocs, ndatetime) ;
# 	char variable_names@VarMetaData(nvars, nstring) ;

# // global attributes:
# 		:nrecs = 1 ;
# 		:nvars = 1 ;
# 		:nlocs = 156 ;
# 		:thinning = 0.5 ;
# 		:date_time = 2019021502 ;
# 		:converter = "owp_snow_obs_pkl_2_ioda.py" ;
# }

# Some todos on potentially desirable information to add in the future:
# TODO JLM: would be nice if the metadata on time@MetaData would say what the datum is and
#           what they units are.
# TODO JLM: Which of these are standardized?
#           Every variable may not have elevation.... nor station_id etc...
#           Should these be metadata or not?
#       float elevation@MetaData(nlocs) ;
#       float rec_elevation@MetaData(nlocs) ;
#       char station_id@MetaData(nlocs) ;
#       char station_name@MetaData(nlocs) ;
# // global attributes:
# 		:platform = "OWP Snow Obs" ;
# 		:sensor = "Multiple" ;
# 		:processing_level = "??" ;

arg_parse_description = (
    """Reads snow OWP observations in python pkl files and converts
    tio IODA output files. """)

output_var_names = {'snow_depth': 'snow_depth', 'snow_water_equivalent': 'swe'}


def read_input(input_args: dict):
    """
    Reads/converts a single input file, performing optional thinning.

    Arguments:
        The args are a dict as this is to be called by multiprocessing
        input_args: A tuple of input (filename, global_config)
            input_file: The name of file to read
            global_config: structure for global arguments related to read
            TODO JLM: what is the structure of global config? Perty opaque.

    Returns:

        A tuple of (obs_data, loc_data, attr_data) needed by the IODA writer.
    """

    # -----------------------------------------------------------------------------
    # The src/input data
    input_file = input_args['input']
    global_config = input_args['global_config']

    # Get the input data and massage it.
    print("Reading ", input_file)
    obs_df = pd.read_csv(input_file, header=0)
    attr_data = {}
    variable_dict = {}
    variable_names = list(output_var_names.keys())

    for vv in variable_names:
        var_df = obs_df[obs_df['variable_name'] == vv]
        if len(var_df) == 0:
            continue
        # -----------------------------------------------------------------------------
        # Location (=space *time) Meta Data: Orange box in conceptual figure.

        # TODO JLM: apparently the date does not limit the window (date range) in
        #           any way. Seems like setting the window size would provide an
        #           opportunity to catch
        #           gross errors.

        # TODO JLM: what is the relationship between the time in the input file and
        #           the time argument which is passed to this "main"? Seems like
        #           something mysterious is happening in the writer.

        # The fundamental/unique location = space * time coordinates
        time = np.array(pd.to_datetime(var_df['datetime']))
        lons = np.array(var_df['longitude'])
        lats = np.array(var_df['latitude'])
        time_str = np.array([tt.strftime("%Y-%m-%dT%H:%M:%SZ") for tt in time])

        # get some of the global attributes that we are interested in
        # data_in['quality_level'] = ncd.variables['quality_level'][:].ravel()
        # mask = data_in['quality_level'] >= 0
        # data_in['quality_level'] = data_in['quality_level'][mask]
        # TODO JLM: Masking operations?
        # TODO JLM: mask on quality? mask on missing?

        # Additional metadata?
        # The possibilities: ['station_elevation', 'station_id', 'station_name',
        # 'station_rec_elevation'])
        # Optional (reproducibly) random thinning: Create a thin_mask (seed
        # depends on ref_date_time).
        np.random.seed(int((
            global_config['ref_date_time'] - datetime(1970, 1, 1)
        ).total_seconds()))
        thin_mask = np.random.uniform(size=len(lons)) > global_config['thin']

        # final output structure
        loc_data = {
            'latitude': lats[thin_mask],
            'longitude': lons[thin_mask],
            'datetime': time_str[thin_mask],
        }

        # -----------------------------------------------------------------------------
        # Obs data and ObsError: Blue and yellow boxes in conceptual figure.

        # Structure it for easy iteration in populating the output structure.
        # TODO JLM: the err and qc multipliers are COMPLETELY MADE UP.
        variable_dict[vv] = {
            'values': var_df['ObsValue'].ravel()[thin_mask],
            'err': var_df['ObsError'].ravel()[thin_mask],
            'qc': var_df['PreQC'].ravel()[thin_mask]}

    # calculate output values
    # Note: the qc flags in GDS2.0 run from 0 to 5, with higher numbers
    # being better. IODA typically expects 0 to be good, and higher numbers
    # are bad, so the qc flags flipped here.
    # Shorten
    oval_name = global_config['oval_name']
    oerr_name = global_config['oerr_name']
    opqc_name = global_config['opqc_name']
    obs_data = {}
    for key in output_var_names.keys():
        if global_config['output_' + key]:
            var_name = output_var_names[key]  # shorten
            obs_data[(var_name, oval_name)] = variable_dict[key]['values']
            obs_data[(var_name, oerr_name)] = variable_dict[key]['err']
            obs_data[(var_name, opqc_name)] = variable_dict[key]['qc']

    return (obs_data, loc_data, attr_data)


def owp_snow_obs_csv_2_ioda(args):

    writer = iconv.NcWriter(args.output, [], [])

    # TODO JLM: Global config: is what?
    # {
    #   'date': for what purpose - seems to only be used for
    #           setting the thinning seed
    #   'thin': A fractional thinning amt? 0.0,
    #   # The following just provide field names?
    #   'opqc_name': What does this mean? 'PreQC'
    #   'oerr_name': What does this mean? 'ObsError',
    #   'oval_name': What does this mean? 'ObsValue'
    # }
    global_config = {}
    global_config['ref_date_time'] = args.ref_date_time
    global_config['thin'] = args.thin
    global_config['oval_name'] = writer.OvalName()
    global_config['oerr_name'] = writer.OerrName()
    global_config['opqc_name'] = writer.OqcName()
    global_config['output_snow_depth'] = args.only_depth
    global_config['output_snow_water_equivalent'] = args.only_swe

    # Create a list of arg dicts
    pool_inputs = [
        {'input': i, 'global_config': global_config} for i in args.input]

    # Serial version for debugging and option to process files in parallel.
    if args.processes == 1:
        obs = [read_input(ii) for ii in pool_inputs]
    else:
        with Pool(processes=args.processes) as pool:
            obs = pool.map(read_input, pool_inputs)

    # TODO JLM: concatenate the data from the files
    obs_data, loc_data, attr_data = obs[0]
    loc_data['datetime'] = writer.FillNcVector(
        loc_data['datetime'], "datetime")
    for i in range(1, len(obs)):
        for k in obs_data:
            axis = len(obs[i][0][k].shape)-1
            obs_data[k] = np.concatenate(
                (obs_data[k], obs[i][0][k]), axis=axis)
        for k in loc_data:
            d = obs[i][1][k]
            if k == 'datetime':
                d = writer.FillNcVector(d, 'datetime')
            loc_data[k] = np.concatenate((loc_data[k], d), axis=0)

    # prepare global attributes we want to output in the file,
    # in addition to the ones already loaded in from the input file
    # TODO JLM: is this format reformatted by the writer.BuildNetcdf? does
    # not match output

    # TODO JLM: What is this date_time_string used for?
    #   Apparently it is the self._ref_date_time in the NcWriter class.
    # TODO JLM: could 'date' be called ref_date_time? there are at least
    #   4 names that this value takes:
    #   args.date
    #   global_config['date']
    #   arttr_data['date_time_string']
    #   self._ref_date_time
    #   That's super confusing. I like "args.ref_date_time" ->
    #       global_config['ref_date_time'] ->
    #       attr_data['ref_date_time'] -> self._ref_date_time
    #   ref indicates something useful.
    attr_data['date_time_string'] = global_config[
        'ref_date_time'].strftime("%Y-%m-%dT%H:%M:%SZ")
    attr_data['thinning'] = global_config['thin']
    attr_data['converter'] = os.path.basename(__file__)

    # determine which variables we are going to output
    selected_names = []
    if global_config['output_snow_depth']:
        selected_names.append(output_var_names['snow_depth'])
    if global_config['output_snow_water_equivalent']:
        selected_names.append(output_var_names['snow_water_equivalent'])

    var_data = {writer._var_list_name: writer.FillNcVector(
        selected_names, "string")}

    # pass parameters to the IODA writer
    # (needed because we are bypassing ExtractObsData within BuildNetcdf)
    writer._nrecs = 1
    writer._nvars = len(selected_names)
    writer._nlocs = obs_data[(selected_names[0], 'ObsValue')].shape[0]

    # use the writer class to create the final output file
    writer.BuildNetcdf(obs_data, loc_data, var_data, attr_data)


# Make parser separate, testable.
def parse_arguments():
    import argparse
    parser = argparse.ArgumentParser(
        description=arg_parse_description
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of OWP snow observation input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--ref_date_time',
        metavar="YYYYMMDDHH",
        help="date and time of the center of the assimilation window",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-t', '--thin',
        help="percentage of random thinning, from 0.0 to 1.0. Zero indicates"
             " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)
    optional.add_argument(
        '--processes',
        # TODO JLM: multiprocessing.Pool provides process based parallelism.
        # TODO JLM: multiprocessing.pool.ThreadPool provides unsupported
        #           thread-based pool parallelism.
        help='multiprocessing.Pool can load input files in parallel.'
             ' (default: %(default)s)',
        type=int, default=1)
    optional.add_argument(
        '--only_depth',
        help='If set, only the snow depth is output.'
             ' Otherwise, depth and swe are both output.',
        action='store_true')
    optional.add_argument(
        '--only_swe',
        help='If set, only the swe is output.'
             ' Otherwise, depth and swe are both output.',
        action='store_true')

    args = parser.parse_args()
    args.ref_date_time = datetime.strptime(args.ref_date_time, '%Y%m%d%H')
    if not args.only_depth and not args.only_swe:
        args.only_depth = True
        args.only_swe = False  # When we have the data....

    return args


if __name__ == '__main__':
    args = parse_arguments()
    return_code = owp_snow_obs_csv_2_ioda(args)
    sys.exit(return_code)
