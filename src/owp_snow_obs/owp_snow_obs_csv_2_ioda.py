#!/usr/bin/env python3
from datetime import datetime
from multiprocessing import Pool
import numpy as np
import os
import pandas as pd
import pathlib
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
# ipython --pdb -c %run owp_snow_obs_csv_2_ioda.py \
#     -i ../../test/# testinput/owp_snow_obs.csv  \
#     -o_depth owp_snow_obs_depth_csv.nc \
#     -o_swe owp_snow_obs_swe_csv.nc \
#     -d 2018100100 -t .3

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
    """Reads snow OWP observations in CSV files and converts
    to IODA output files. """)

output_var_names = {'snow_depth_mm': 'snow_depth', 'snow_water_equivalent_mm': 'swe'}
output_type_names = {'snow_depth_mm': 'output_depth', 'snow_water_equivalent_mm': 'output_swe'}
output_var_units = {'snow_depth': 'mm', 'swe': 'mm'}


def read_input(input_file, global_config):
    """
    Reads/converts a csv input file, performing optional thinning.
    Arguments:
        input_file: the file to read_csv
        global_config: the ioda global config
    Returns:
        A tuple of (obs_data, loc_data, attr_data) needed by the IODA writer.
    """

    # Get the input data and massage it.
    print("Reading ", input_file)
    obs_df = pd.read_csv(input_file, header=0)
    attr_data = {}
    variable_dict = {}
    variable_names = list(output_var_names.keys())
    return_dict = {}

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
        randoms = np.random.uniform(size=len(lons))
        thin_quantile = np.quantile(randoms, global_config['thin'])
        thin_mask = randoms >= thin_quantile
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
        var_name = output_var_names[vv]  # shorten
        obs_data[(var_name, oval_name)] = variable_dict[vv]['values']
        obs_data[(var_name, oerr_name)] = variable_dict[vv]['err']
        obs_data[(var_name, opqc_name)] = variable_dict[vv]['qc']

        return_dict[vv] = {
            'obs_data': obs_data,
            'loc_data': loc_data,
            'attr_data': attr_data}

    return return_dict


def owp_snow_obs_csv_2_ioda(args):

    args_dict = vars(args)
    if args_dict['output_depth'] is not None:
        output_dum = args_dict['output_depth']
    elif args_dict['output_swe'] is not None:
        output_dum = args_dict['output_swe']
    else:
        raise ValueError("Neither SWE nor depth outputfile requested")

    writer_dum = iconv.NcWriter(output_dum, [], [])
    pathlib.Path(output_dum).unlink()

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
    global_config['oval_name'] = writer_dum.OvalName()
    global_config['oerr_name'] = writer_dum.OerrName()
    global_config['opqc_name'] = writer_dum.OqcName()

    # Create a list of arg dicts
    obs = read_input(input_file=args.input[0], global_config=global_config)

    for var_name, output_type in output_type_names.items():
        if args_dict[output_type] is None:
            continue
        writer = iconv.NcWriter(args_dict[output_type], [], [])

        obs_data = obs[var_name]['obs_data']
        loc_data = obs[var_name]['loc_data']
        attr_data = obs[var_name]['attr_data']
        loc_data['datetime'] = writer.FillNcVector(
            loc_data['datetime'], "datetime")

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

        var_list_name = output_var_names[var_name]
        # var_list_name = var_name
        var_data = {
            writer._var_list_name: writer.FillNcVector(var_list_name, "string")}

        # pass parameters to the IODA writer
        # (needed because we are bypassing ExtractObsData within BuildNetcdf)
        writer._nrecs = 1
        writer._nvars = 1
        writer._nlocs = obs_data[(output_var_names[var_name], 'ObsValue')].shape[0]

        # use the writer class to create the final output file
        writer.BuildNetcdf(obs_data, loc_data, var_data, attr_data, VarUnits=output_var_units)


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
        '-o_depth', '--output_depth',
        help="path of IODA output snow depth file",
        type=str, required=False)
    required.add_argument(
        '-o_swe', '--output_swe',
        help="path of IODA output SWE file",
        type=str, required=False)
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

    args = parser.parse_args()
    args.ref_date_time = datetime.strptime(args.ref_date_time, '%Y%m%d%H')

    return args


if __name__ == '__main__':
    args = parse_arguments()
    return_code = owp_snow_obs_csv_2_ioda(args)
    sys.exit(return_code)
