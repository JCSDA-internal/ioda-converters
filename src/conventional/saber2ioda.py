#!/usr/bin/env python3
#
# (C) Copyright 2025 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

"""
Python code to ingest Sounding of the Atmosphere using Broadband Emission Radiometry (SABER) data
"""
import argparse
import logging
import os
from datetime import datetime, timedelta, timezone
from collections import defaultdict
from concurrent.futures import ThreadPoolExecutor

import netCDF4
import numpy

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import (
    int_missing_value,
    long_missing_value,
    float_missing_value,
    epoch,
    record_time
)

# Globals
META_DATA_NAME = iconv.MetaDataName()
OBS_VAL_NAME = iconv.OvalName()
OBS_ERR_NAME = iconv.OerrName()
OBS_QC_NAME = iconv.OqcName()

# The global attributes of the IODA file
GLOBAL_ATTRS = {
    "converter": os.path.basename(__file__),
    "source": "Sounding of the Atmosphere using Broadband Emission Radiometry (SABER)"
}

# Location info
LOCATION_KEYS = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long")
]

# Assign dimensions to the obs values
VAR_DIMS = {
    'airTemperature': ['Location'],
}

# Variable attributes
VAR_ATTRS = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
VAR_ATTRS['airTemperature', OBS_VAL_NAME]['coordinates'] = 'longitude latitude'
VAR_ATTRS['airTemperature', OBS_VAL_NAME]['units'] = 'K'
VAR_ATTRS['airTemperature', OBS_ERR_NAME]['coordinates'] = 'longitude latitude'
VAR_ATTRS['airTemperature', OBS_ERR_NAME]['units'] = 'K'
VAR_ATTRS['airTemperature', OBS_QC_NAME]['coordinates'] = 'longitude latitude'
VAR_ATTRS['pressure', META_DATA_NAME]['units'] = 'Pa'
VAR_ATTRS['height', META_DATA_NAME]['units'] = 'm'
VAR_ATTRS['tpAscendDescend', META_DATA_NAME]['units'] = 'unitless'
VAR_ATTRS['tpDayNight', META_DATA_NAME]['units'] = 'unitless'
VAR_ATTRS['solarZenithAngle', META_DATA_NAME]['units'] = 'degrees'
VAR_ATTRS['dateTime', META_DATA_NAME]['units'] = 'seconds since 1970-01-01T00:00:00Z'

# Missing values definitions
MISSING_VALS = {
    'integer': int_missing_value,
    'long': long_missing_value,
    'float': float_missing_value,
    'short': "-32767",
}
DTYPES = {
    'integer': numpy.int32,
    'long': numpy.int64,
    'float': numpy.float32,
    'short': numpy.int16,
}


def map_missing_values(obs_data):
    mapped = {}
    for key, val in obs_data.items():
        if isinstance(val, numpy.ma.MaskedArray):
            dtype = val.dtype
            type_name = None

            for name, dt in DTYPES.items():
                if numpy.issubdtype(dtype, dt):
                    type_name = name
                    break

            if type_name is None or type_name not in MISSING_VALS:
                raise ValueError(f"No missing value defined for dtype {dtype}")

            mapped[key] = val.filled(MISSING_VALS[type_name])
        else:
            mapped[key] = val
    return mapped


def parse_yyyymmddhh(date_str):
    try:
        return datetime.strptime(date_str, "%Y%m%d%H").replace(tzinfo=timezone.utc)
    except ValueError as e:
        raise argparse.ArgumentTypeError(
            f"Invalid date format: '{date_str}'. Expected YYYYMMDDHH."
        ) from e


def time_window_screening(full_obs_data, window_start, window_end):
    window_start_epoch = int(window_start.timestamp())
    window_end_epoch = int(window_end.timestamp())
    time_values = full_obs_data[('dateTime', META_DATA_NAME)]
    valid_time_mask = (time_values >= window_start_epoch) & (time_values <= window_end_epoch)

    filtered_obs_data = {}
    for key, val in full_obs_data.items():
        if isinstance(val, numpy.ma.MaskedArray):
            filtered_obs_data[key] = val[valid_time_mask]
        else:
            filtered_obs_data[key] = numpy.array(val)[valid_time_mask]
    return filtered_obs_data


def process_file(file_name):
    if not os.path.isfile(file_name):
        raise FileNotFoundError(f"Input file not found (-i option): {file_name}")
    logging.info("Reading input file: %s", file_name)
    with netCDF4.Dataset(file_name, 'r') as file:
        return get_data_from_file(file)


def merge_obs_data(obs_data_list):
    merged = defaultdict(list)
    for data in obs_data_list:
        for key, val in data.items():
            merged[key].append(val)
    return {k: numpy.ma.concatenate(v) for k, v in merged.items()}


def read_variable(obs_file_handle, varname, dtype, flatten=True):
    var = obs_file_handle[varname]
    data = var[:]
    masked = numpy.ma.masked_array(
        data=data.data.astype(dtype),
        mask=numpy.ma.getmaskarray(data)
    )
    if flatten:
        masked = masked.flatten()
    return masked


def get_epoch_time(date_var, time_var):
    n_events, n_levels = time_var.shape
    epoch_time = numpy.ma.masked_all((n_events, n_levels), dtype=numpy.int64)

    for i in range(n_events):
        if date_var.mask[i]:
            continue

        date_val = date_var[i]
        year = date_val // 1000
        day_of_year = date_val % 1000

        try:
            base_date = datetime(year, 1, 1, tzinfo=timezone.utc) + timedelta(days=int(day_of_year) - 1)
        except ValueError:
            continue

        base_epoch_sec = int((base_date - epoch).total_seconds())
        times = time_var[i, :]

        if numpy.ma.is_masked(times):
            valid = ~times.mask
            epoch_time[i, valid] = base_epoch_sec + (times[valid] // 1000)
        else:
            epoch_time[i, :] = base_epoch_sec + (times // 1000)

    return epoch_time.flatten()


def get_data_from_file(obs_file_handle):
    obs_data = {
        ('latitude', META_DATA_NAME): read_variable(obs_file_handle, 'tplatitude', dtype=numpy.float32),
        ('longitude', META_DATA_NAME): read_variable(obs_file_handle, 'tplongitude', dtype=numpy.float32),
        ('height', META_DATA_NAME): read_variable(obs_file_handle, 'tpaltitude', dtype=numpy.float32),
        ('pressure', META_DATA_NAME): read_variable(obs_file_handle, 'pressure', dtype=numpy.float32),
        ('tpAscendDescend', META_DATA_NAME): read_variable(obs_file_handle, 'tpAD', dtype=numpy.int16),
        ('tpDayNight', META_DATA_NAME): read_variable(obs_file_handle, 'tpDN', dtype=numpy.int16),
        ('solarZenithAngle', META_DATA_NAME): read_variable(obs_file_handle, 'tpSolarZen', dtype=numpy.float32),
        ('airTemperature', OBS_VAL_NAME): read_variable(obs_file_handle, 'ktemp', dtype=numpy.float32),
    }

    # Change height units from km to m
    obs_data[('height', META_DATA_NAME)] *= 1000.0

    # Change pressure units from hPa to Pa
    obs_data[('pressure', META_DATA_NAME)] *= 100.0

    # Handle longitudes to be within [-180, 180)
    obs_data[('longitude', META_DATA_NAME)] = numpy.ma.where(
        obs_data[('longitude', META_DATA_NAME)] > 180,
        obs_data[('longitude', META_DATA_NAME)] - 360,
        obs_data[('longitude', META_DATA_NAME)]
    )

    # Handle time conversion
    date_raw = read_variable(obs_file_handle, 'date', dtype=numpy.int64, flatten=False)
    time_raw = read_variable(obs_file_handle, 'time', dtype=numpy.int64, flatten=False)
    obs_data[('dateTime', META_DATA_NAME)] = get_epoch_time(date_raw, time_raw)

    # Add error and QC values
    nlocs = len(obs_data[('latitude', META_DATA_NAME)])
    obs_data[('airTemperature', OBS_ERR_NAME)] = numpy.full(nlocs, 1.0, dtype=numpy.float32)
    obs_data[('airTemperature', OBS_QC_NAME)] = numpy.zeros(nlocs, dtype=numpy.int32)

    logging.debug("Extracted %d observations from file.", nlocs)

    return obs_data


def main(args):
    if args.verbose:
        logging.basicConfig(level=logging.INFO)
    elif args.debug:
        logging.basicConfig(level=logging.DEBUG)
    else:
        logging.basicConfig(level=logging.WARNING)

    logging.debug("Starting processing")
    tic = record_time()

    # Parallel file processing
    with ThreadPoolExecutor() as executor:
        obs_data_list = list(executor.map(process_file, args.file_names))

    # Merge all obs_data dictionaries
    obs_data = merge_obs_data(obs_data_list)

    # Map missing values
    obs_data = map_missing_values(obs_data)

    # Time window screening
    if args.date:
        window_start = args.date - timedelta(hours=args.window_offset)
        window_end = window_start + timedelta(hours=args.window_length)
        GLOBAL_ATTRS['datetimeReference'] = [args.date.strftime('%Y-%m-%dT%H:%M:%SZ')]
        GLOBAL_ATTRS['datetimeRange'] = [
            window_start.strftime('%Y-%m-%dT%H:%M:%SZ'),
            window_end.strftime('%Y-%m-%dT%H:%M:%SZ')
        ]
        logging.debug(
            "Screening out observations outside of the window: [window_start=%s, window_end=%s].",
            window_start, window_end
        )
        obs_data = time_window_screening(obs_data, window_start, window_end)

    # Write IODA
    nlocs = len(obs_data[('latitude', META_DATA_NAME)])
    if nlocs > 0:
        dim_dict = {'Location': nlocs}
        writer = iconv.IodaWriter(args.output_file, LOCATION_KEYS, dim_dict)
        writer.BuildIoda(obs_data, VAR_DIMS, VAR_ATTRS, GLOBAL_ATTRS)
        logging.debug("Wrote %d observations into the IODA file.", nlocs)

    else:
        raise ValueError('No valid observations remained.')

    logging.debug("Processing complete. Time elapsed: %f", record_time() - tic)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Read SABER netCDF files and convert into IODA output file"
    )

    required = parser.add_argument_group(title='Required Arguments')
    required.add_argument('-i', '--input-files', nargs='+', dest='file_names',
                          required=True, help='Input netCDF files')
    required.add_argument('-o', '--output-file', dest='output_file',
                          required=True, help='Output IODA file')

    optional = parser.add_argument_group(title='Optional Arguments')
    optional.add_argument('--date', metavar="YYYYMMDDHH", type=parse_yyyymmddhh, default=None,
                          help='Date for center of the window')
    optional.add_argument('--window_length', type=float, default=6,
                          help='Window length (in hours as a float)')
    optional.add_argument('--window_offset', type=float, default=3,
                          help='Window offset (in hours as a float)')
    optional.add_argument('--debug', action='store_true',
                          help='Enable debug messages (DEBUG level)')
    optional.add_argument('--verbose', action='store_true',
                          help='Enable verbose messages (INFO level)')

    main(parser.parse_args())
