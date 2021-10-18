#!/usr/bin/env python3

from datetime import datetime
# from multiprocessing import Pool
import numpy as np
import os
import pandas as pd
import pathlib
import sys
from pathlib import Path


IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

# (C) Copyright 2019 UCAR
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# Purpose: Convert OWP snow observations from CSV format to IODA netcdf format.
# Author:
# 2020-11-17: James McCreight

# Conceptual Figure:
# TODO: this is probably out of date... a
# https://jointcenterforsatellitedataassimilation-jedi-docs.readthedocs-hosted.com/en/latest/_images/IODA_InMemorySchematic.png

# Example testing usage:
# ipython --pdb -c %run owp_snow_obs_csv_2_ioda.py \
#     -i ../../test/# testinput/owp_snow_obs.csv  \
#     -o_depth owp_snow_obs_depth_csv.nc \
#     -o_swe owp_snow_obs_swe_csv.nc \
#     -d 2018100100 -t .3

arg_parse_description = (
    """Reads snow OWP observations in CSV files and converts
    to IODA output files. """)

# obs file name -> ioda file name
output_var_dict = {'snow_depth_mm': 'snow_depth', 'snow_water_equivalent_mm': 'swe'}
# ioda file_name -> ioda file units
output_var_unit_dict = {'snow_depth': 'mm', 'swe': 'mm'}

location_key_list = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string"),]

dim_dict = {}

var_dims = {
    'snow_depth': ['nlocs'],
    'swe': ['nlocs'], }

attr_data = {
    'converter': os.path.basename(__file__),
    'converter_version': 0.3,
    'nvars': np.int32(len(var_dims)), }


class OwpSnowObs(object):
    def __init__(self, file_in, file_out):  #, thin):
        self.file_in = file_in
        self.file_out = file_out
        # self.thin = thin

        self.var_dict = defaultdict(lambda: defaultdict(dict))
        self.meta_dict = defaultdict(lambda: defaultdict(dict))
        self.data = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_metadata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        
        self.attr_data = attr_data
        self.units = {}
        self._read()

    def _read(self):
        print(f"Reading: {self.file_in}")
        self.attr_data['obs_file'] = str(self.file_in)
        obs_df = pd.read_csv(self.file_in, header=0, index_col=False)
        data_cols = set(['ObsValue', 'ObsError', 'PreQC'])
        index_cols = list(
            set(obs_df.columns.values.tolist()).difference(data_cols))
        obs_df = obs_df.reset_index().set_index(index_cols).drop(columns='index')
        obs_df = obs_df.unstack('variable_name').reset_index()
        obs_df.columns = [' '.join(col).strip() for col in obs_df.columns.values]

        # Do we want to think both variable the same amount?
        # if self.thin > 0:
        #     # Set a seed for each day - reproducibly random
        #     time_diff = (
        #         pd.Timestamp(self.attr_data['ref_date_time']) -
        #         pd.Timestamp('2014-01-23 00:00:00', tz='UTC'))
        #     np.random.seed(int(time_diff.total_seconds()))
        #     randoms = np.random.uniform(size=len(obs_df))
        #     thin_quantile = np.quantile(randoms, self.thin)
        #     thin_inds = np.where(randoms <= thin_quantile)[0]  # 1-D
        #     obs_df = obs_df.drop(thin_inds)

        # TODO: drop rows where PreQC != 0 ?

        self.attr_data['ref_date_time'] = (
            pd.to_datetime(obs_df.datetime[0])
            .round('D')
            .strftime('%Y-%m-%dT%H:%M:%SZ'))

        self.data[('datetime', 'MetaData')] = obs_df.datetime
        self.data[('latitude', 'MetaData')] = obs_df.latitude
        self.data[('longitude', 'MetaData')] = obs_df.longitude

        for obsvar, iodavar in output_var_dict.items():
            # define the ioda variable
            self.var_dict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.var_dict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.var_dict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            # define ioda meta/ancillary
            self.units[iodavar] = output_var_unit_dict[iodavar]
            self.var_metadata[iodavar]['coordinates'] = 'longitude latitude'
            # the data
            self.data[self.var_dict[iodavar]['valKey']] = obs_df[f'ObsValue {obsvar}']
            self.data[self.var_dict[iodavar]['errKey']] = obs_df[f'ObsError {obsvar}']
            self.data[self.var_dict[iodavar]['qcKey']] = obs_df[f'PreQC {obsvar}']

        nlocs = len(self.data[('datetime', 'MetaData')])
        dim_dict['nlocs'] = nlocs
        attr_data['nlocs'] = np.int32(nlocs)

    def write(self):
        writer = iconv.IodaWriter(self.file_out, location_key_list, dim_dict)
        result = writer.BuildIoda(
            self.data, var_dims, self.var_metadata, self.attr_data, self.units)
        return result


def parse_arguments():
    import argparse
    parser = argparse.ArgumentParser(
        description=arg_parse_description)

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of OWP snow observation input file(s)",
        type=str,
        # nargs='+',
        required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=False)
    # optional = parser.add_argument_group(title='optional arguments')
    # optional.add_argument(
    #     '-t', '--thin',
    #     help="percentage of random thinning, from 0.0 to 1.0. Zero indicates"
    #          " no thinning is performed. (default: %(default)s)",
    #     type=float, default=0.0)
    # optional.add_argument(
    #     '--processes',
    #     # TODO JLM: multiprocessing.Pool provides process based parallelism.
    #     # TODO JLM: multiprocessing.pool.ThreadPool provides unsupported
    #     #           thread-based pool parallelism.
    #     help='multiprocessing.Pool can load input files in parallel.'
    #          ' (default: %(default)s)',
    #     type=int, default=1)

    args = parser.parse_args()
    return args


if __name__ == '__main__':
    args = parse_arguments()
    owp_snow_obs = OwpSnowObs(args.input, args.output)  # , args.thin)
    result = owp_snow_obs.write()
    sys.exit(result)
