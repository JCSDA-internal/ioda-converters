#!/usr/bin/env python3

# (C) Copyright 2019 UCAR
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# Purpose: Convert OWP snow observations from CSV format to IODA netcdf format.
# Author:
# 2020-11-17, 2021-10-18, 2022-01-09: James McCreight

from datetime import datetime
import numpy as np
import os
import pandas as pd
import pathlib
from pathlib import Path
import sys
import warnings


IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

arg_parse_description = (
    """Reads snow OWP observations in CSV files and converts
    to IODA output files. """)

# obs file name -> ioda file name
output_var_dict = {'snow_depth_m': 'snow_depth', 'snow_water_equivalent_mm': 'swe'}
# ioda file_name -> ioda file units
output_var_unit_dict = {'snow_depth': 'm', 'swe': 'mm'}
one = 1.00000000000000
output_conversion_factor = {'snow_depth': one, 'swe': one}

col_types = {
    'StnObjID': np.int32,
    'StnID': str,
    'ObsValue': np.float32,
    'ObsError': np.float32,
    'PreQC': np.int32,
    'dem_elev_m': np.float32,
    'rec_elev_m': np.float32,
    'latitude': np.float64,
    'longitude': np.float64,
    'datetime': str,
    'variable_name': str}

location_key_list = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("height", "integer"),
    ("datetime", "string"), ]

dim_dict = {}

var_dims = {
    'snow_depth': ['nlocs'],
    'swe': ['nlocs'], }

attr_data = {
    'converter': os.path.basename(__file__),
    'converter_version': 0.3,
    'nvars': np.int32(len(var_dims)), }

fill_value = 9.96921e+36


def dummy_err(obs_values, var_name):
    obs_errs = 0.5 * obs_values
    return obs_errs


def mask_nans(arr):
    arr2 = arr.astype('float32')
    arr2 = np.nan_to_num(arr2, nan=fill_value)
    arr2 = np.ma.masked_where(arr2 == fill_value, arr2)
    return arr2


class OwpSnowObs(object):
    def __init__(
            self,
            file_in, file_out,
            thin_swe, thin_depth, thin_random_seed,
            err_fn):

        self.file_in = file_in
        self.file_out = file_out
        self.thin_swe = thin_swe
        self.thin_depth = thin_depth
        self.thin_random_seed = thin_random_seed
        self.err_fn = err_fn

        self.var_dict = defaultdict(lambda: defaultdict(dict))
        self.meta_dict = defaultdict(lambda: defaultdict(dict))
        self.data = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.var_metadata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))

        self.attr_data = attr_data
        self.units = {}
        self._read()

    def _read(self):
        # print(f"Reading: {self.file_in}")
        self.attr_data['obs_file'] = str(self.file_in.split('/')[-1])
        # use pandas to get the data lined up
        obs_df = pd.read_csv(self.file_in, header=0, index_col=False, dtype=col_types)

        # Deal with duplicates.
        n_obs = len(obs_df)
        obs_df = obs_df.drop_duplicates().reset_index().drop(columns='index')
        n_obs2 = len(obs_df)
        if n_obs2 != n_obs:
            warnings.warn(
                f"{n_obs - n_obs2} duplicate rows removed from {self.file_in}")

        # TODO: drop rows where PreQC != 0 ?
        data_cols = set(['ObsValue', 'ObsError', 'PreQC'])
        index_cols = list(
            set(obs_df.columns.values.tolist()).difference(data_cols))
        index_cols2 = sorted(set(index_cols).difference(set(['variable_name'])))
        obs_df = obs_df.reset_index().set_index(index_cols + ['index'])
        obs_df = obs_df.unstack('variable_name').reset_index()
        obs_df.columns = [' '.join(col).strip() for col in obs_df.columns.values]
        # Unstack is not reproducible, must sort after
        obs_df = (
            obs_df
            .set_index(index_cols2)
            .sort_index()
            .reset_index())

        self.attr_data['ref_date_time'] = (
            pd.to_datetime(obs_df.datetime[0])
            .round('D')
            .strftime('%Y-%m-%dT%H:%M:%SZ'))

        if self.thin_swe == 1.0 and self.thin_depth == 1.0:
            print('No output requested, but variables thined by 100%')
            return(None)
        self.attr_data['thin_swe'] = self.thin_swe
        self.attr_data['thin_depth'] = self.thin_depth
        thin_dict = {
            'snow_water_equivalent_mm': self.thin_swe,
            'snow_depth_m': self.thin_depth}
        # Sort it by value in case one of the values is 1.
        thin_dict = dict(sorted(thin_dict.items(), key=lambda x: x[1]))
        # to make it reproducible if a seed is not passed
        if self.thin_random_seed is None:
            # Set a seed for each day - reproducibly random
            time_datum = pd.Timestamp('1979-01-01 00:00:00', tz='UTC')
            time_diff = pd.Timestamp(self.attr_data['ref_date_time']) - time_datum
            self.thin_random_seed = int(time_diff.total_seconds())
        np.random.seed(self.thin_random_seed)
        for var, thin in thin_dict.items():
            if thin > 0.0:
                wh_var = np.where(~np.isnan(obs_df[f'ObsValue {var}']))[0]  # 1-D
                randoms = np.random.uniform(size=len(wh_var))
                thin_quantile = np.quantile(randoms, thin)
                thin_inds = np.where(randoms <= thin_quantile)[0]
                for cc in data_cols:
                    obs_df.loc[wh_var[thin_inds], f'{cc} {var}'] = np.nan
                if all(np.isnan(obs_df[f'ObsValue {var}'])):
                    # If there's no data, drop the variable(s) from the output list
                    del output_var_dict[f'{var}']
                    # Also can then remove nans in the remaining other var (since we only have 2 vars)
                    var_other = list(output_var_dict.keys())[0]
                    wh_not_var_other = np.where(np.isnan(obs_df[f'ObsValue {var_other}']))[0]  # 1-D
                    obs_df = obs_df.drop(wh_not_var_other).reset_index()

        self.data[('datetime', 'MetaData')] = obs_df.datetime.values
        self.data[('latitude', 'MetaData')] = obs_df.latitude.values.astype('float32')
        self.data[('longitude', 'MetaData')] = obs_df.longitude.values.astype('float32')

        self.data[('height', 'MetaData')] = obs_df.rec_elev_m.values.astype('float32')
        self.data[('station_id', 'MetaData')] = obs_df.StnID.values
        self.var_metadata[('height', 'MetaData')]['units'] = 'm'
        self.var_metadata[('station_id', 'MetaData')]['units'] = 'unitless'

        for obs_var, ioda_var in output_var_dict.items():
            # define the ioda variable
            self.var_dict[ioda_var]['valKey'] = ioda_var, iconv.OvalName()
            self.var_dict[ioda_var]['errKey'] = ioda_var, iconv.OerrName()
            self.var_dict[ioda_var]['qcKey'] = ioda_var, iconv.OqcName()
            # define ioda meta/ancillary
            for name in [iconv.OvalName(), iconv.OerrName(), iconv.OqcName()]:
                self.var_metadata[ioda_var, name]['coordinates'] = 'longitude latitude'
                self.var_metadata[ioda_var, name]['units'] = output_var_unit_dict[ioda_var]  # not really for Oqc... but
            # just kidding for OqcName... a lazy tag along above, fix now (less code to overwrite)
            self.var_metadata[ioda_var, iconv.OqcName()]['units'] = 'unitless'
            # the data
            conv_fact = output_conversion_factor[ioda_var]
            self.data[self.var_dict[ioda_var]['valKey']] = (
                mask_nans(obs_df[f'ObsValue {obs_var}'].values * conv_fact))
            if self.err_fn is None:
                self.data[self.var_dict[ioda_var]['errKey']] = (
                    mask_nans(obs_df[f'ObsError {obs_var}'].values * conv_fact))
            else:
                self.data[self.var_dict[ioda_var]['errKey']] = (
                    mask_nans(globals()[self.err_fn](obs_df[f'ObsValue {obs_var}'].values * conv_fact, obs_var)))
            self.data[self.var_dict[ioda_var]['qcKey']] = (
                mask_nans(obs_df[f'PreQC {obs_var}'].values * conv_fact))

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
        # nargs='+',  # could consider multiple days/files in the future.
        required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=False)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--thin_swe',
        help="percentage of random thinning for SWE, from 0.0 to 1.0. Zero indicates"
             " no thinning is performed. (type: float, default: %(default)s)",
        type=float, default=0.0)
    optional.add_argument(
        '--thin_depth',
        help="percentage of random thinning for snow depth, from 0.0 to 1.0. Zero indicates"
             " no thinning is performed. (type: float, default: %(default)s)",
        type=float, default=0.0)
    optional.add_argument(
        '--thin_random_seed',
        help="A random seed for reproducible random thinning. Default is total # seconds from "
             "1970-01-01 to the day of the data provided. (type: int, default: %(default)s)",
        type=int, default=None)
    optional.add_argument(
        '--err_fn',
        help="Name of error function to apply. The options are hardcoded in the module, currently:"
        "['dummy_error']. Default (none) uses ObsError column in the input file. (type: str, default: %(default)s)",
        type=str, default=None)

    args = parser.parse_args()
    return args


if __name__ == '__main__':
    args = parse_arguments()
    owp_snow_obs = OwpSnowObs(
        args.input, args.output,
        args.thin_swe, args.thin_depth, args.thin_random_seed,
        args.err_fn)
    result = owp_snow_obs.write()
    sys.exit(result)
