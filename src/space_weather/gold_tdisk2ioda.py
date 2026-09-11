#!/usr/bin/env python3

#
# (C) Copyright 2019-2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

# imports
import argparse
import os
from datetime import datetime
from pathlib import Path

import netCDF4 as nc
import numpy as np
from pyiodaconv.def_jedi_utils import epoch, iso8601_string
import pyiodaconv.ioda_conv_engines as iconv

# IODA names
OBS_VARIABLE = "airTemperature"

OBS_VALUE = iconv.OvalName()
OBS_ERROR = iconv.OerrName()
PRE_QC = iconv.OqcName()
META_DATA = iconv.MetaDataName()

# file reading and processing functions


def read_files(filenames):
    """
    Read multiple netCDF files and combine the data into a single dictionary.

    Args:
        filenames (list): List of paths to the netCDF files.
    """
    combined_data = {}

    for filename in filenames:
        data = read_file(filename)

        # combine the data from each file
        for name, value in data.items():
            if name not in combined_data:
                combined_data[name] = value
            else:
                combined_data[name] = np.concatenate((combined_data[name], value), axis=0)

    return combined_data


def read_file(filename: str):
    """
    Read a netCDF file and extract/compute the variables needed for IODA conversion.

    Args:
        filename (str): Path to the netCDF file.

    """
    input_names = [
        "tdisk_unc_ran",            # Retrieved Temperature Random Uncertainty
        "latitude",                 # Latitude
        "dqi",                      # File-level Data Quality Indicator. 0 indicates good data.
        "solar_zenith_angle",       # Solar Zenith Angle
        "tdisk",                    # Retrieved Temperature
        "time_utc",                 # Time of the observation in UTC
        "longitude",                # Longitude
        "emission_angle",           # Emission angle
        "tdisk_dqi",                # Retrieved Temperature Data Quality Indicator. 0 indicates good data.
    ]

    # read the netCDF file and extract the variables
    with nc.Dataset(filename, "r") as dataset:
        # Explicitly disable auto-masking to avoid issues with masked arrays
        dataset.set_auto_mask(False)

        # initialize a dictionary to hold the data
        data = {}

        # loop through the input names and extract the corresponding variables
        for name in input_names:
            if name in dataset.variables:
                data[name] = dataset.variables[name][:]
            else:
                raise KeyError(f"Variable '{name}' not found in the netCDF file.")

        # reshape the data to match the expected IODA format
        data = reshape_data(data)

        # compute combined data quality indicator
        data = combined_dqi(data)

        # flatten the data to 1D arrays for IODA format
        for name in data:
            data[name] = data[name].flatten()

        # remove invalid tdisk indices
        valid_tdisk = np.isfinite(data["tdisk"])
        for name, values in data.items():
            data[name] = values[valid_tdisk]

        # convert time in utc to dateTime required by ioda
        data = convert_time(data)

        return data


def reshape_data(data: dict):
    """
    Reshape the data to match the expected IODA format.

    Args:
        data (dict): Dictionary containing the extracted variables.
    """
    target_shape = data["tdisk"].shape

    # latitude and longitude are 2D arrays, so broadcast them to match the shape of tdisk
    data["latitude"] = np.broadcast_to(data["latitude"], target_shape)
    data["longitude"] = np.broadcast_to(data["longitude"], target_shape)

    # dqi is a 1D array, so broadcast them to match the shape of tdisk
    data["dqi"] = np.broadcast_to(data["dqi"][:, None, None], target_shape)

    # time_utc is a 4D array with one dimension of characters, so convert to string
    data["time_utc"] = nc.chartostring(data["time_utc"])

    return data


def combined_dqi(data: dict):
    """
    Compute combined data quality indicator. Removes data["dqi"] and data["tdisk_dqi"] and adds data["combined_dqi"] to the dictionary.

    Args:
        data (dict): Dictionary containing the extracted and reshaped variables.
    """

    # combine the data quality indicators into a single field
    data["combined_dqi"] = (
        (data["dqi"] != 0) | (data["tdisk_dqi"] != 0)
    ).astype(np.int32)

    del data["dqi"]
    del data["tdisk_dqi"]

    return data


def convert_time(data):
    """
    Convert time in utc to dateTime required by ioda. Removes data["time_utc"] and adds data["dateTime"] to the dictionary.

    Args:
        data (dict): Dictionary containing data["time_utc"] to be converted to data["dateTime"]
    """
    data["dateTime"] = np.array([
        round(
            (datetime.fromisoformat(value.replace("Z", "+00:00")) - epoch)
            .total_seconds()
        )
        for value in data["time_utc"]
    ], dtype=np.int64)

    del data["time_utc"]

    return data


def validate_paths(input_filenames, output_filename):
    """
    Validate the input and output paths.

    Args:
        input_filenames (list): List of input file paths.
        output_filename (str): Output file path.
    """

    # Input checks
    for filename in input_filenames:
        input_path = Path(filename)

        if not input_path.is_file():
            raise FileNotFoundError(
                f"Input file does not exist: {input_path}"
            )

        if not os.access(input_path, os.R_OK):
            raise PermissionError(
                f"Input file is not readable: {input_path}"
            )

    # Output checks
    output_path = Path(output_filename)
    output_directory = output_path.parent

    if output_path.is_dir():
        raise IsADirectoryError(
            f"Output path is a directory; provide a filename: {output_path}"
        )

    if not output_directory.is_dir():
        raise FileNotFoundError(
            f"Output directory does not exist: {output_directory}"
        )

    if not os.access(output_directory, os.W_OK):
        raise PermissionError(
            f"Output directory is not writable: {output_directory}"
        )


# output writing function
def write_ioda(data, output_filename):
    """
    Write IODA file from dictionary of observations and metadata.

    Args:
        data (dict): dictionary of observations and metadata
        output_filename (str): path to the output IODA file
    """
    # Define the IODA variable names and their corresponding data
    ioda_data = {
        (OBS_VARIABLE, OBS_VALUE): data["tdisk"].astype(np.float32),
        (OBS_VARIABLE, OBS_ERROR): data["tdisk_unc_ran"].astype(np.float32),
        (OBS_VARIABLE, PRE_QC): data["combined_dqi"].astype(np.int32),

        ("latitude", META_DATA): data["latitude"].astype(np.float32),
        ("longitude", META_DATA): data["longitude"].astype(np.float32),
        ("dateTime", META_DATA): data["dateTime"].astype(np.int64),
        ("solarZenithAngle", META_DATA):
            data["solar_zenith_angle"].astype(np.float32),
        ("sensorZenithAngle", META_DATA):
            data["emission_angle"].astype(np.float32),
    }

    # Define output structure
    nlocs = data["tdisk"].size

    dim_dict = {
        "Location": nlocs,
    }

    location_key_list = [
        ("latitude", "float"),
        ("longitude", "float"),
        ("dateTime", "long"),
        ("solarZenithAngle", "float"),
        ("sensorZenithAngle", "float"),
    ]

    var_dims = {
        OBS_VARIABLE: ["Location"],
    }

    var_attrs = {
        (OBS_VARIABLE, OBS_VALUE): {"units": "K"},
        (OBS_VARIABLE, OBS_ERROR): {"units": "K"},
        (OBS_VARIABLE, PRE_QC): {},

        ("latitude", META_DATA): {"units": "degree"},
        ("longitude", META_DATA): {"units": "degree"},
        ("dateTime", META_DATA): {"units": iso8601_string},
        ("solarZenithAngle", META_DATA): {"units": "degree"},
        ("sensorZenithAngle", META_DATA): {"units": "degree"},
    }

    global_attrs = {
        "converter": os.path.basename(__file__),
    }

    writer = iconv.IodaWriter(
        output_filename,
        location_key_list,
        dim_dict,
    )

    writer.BuildIoda(
        ioda_data,
        var_dims,
        var_attrs,
        global_attrs,
    )


# program entry point
def main():
    # Define arguments
    parser = argparse.ArgumentParser(
        description="Read GOLD TDISK data and convert it to IODA format."
    )

    parser.add_argument("-i", "--input", nargs="+", required=True)
    parser.add_argument("-o", "--output", required=True)

    # Read user input arguments
    args = parser.parse_args()

    # Check paths are valid
    try:
        validate_paths(args.input, args.output)
    except OSError as error:
        parser.error(str(error))

    # Read GOLD TDISK file
    data = read_files(args.input)

    # Write IODA file
    write_ioda(data, args.output)


if __name__ == "__main__":
    main()
