#!/usr/bin/env python3

# (C) Copyright 2019-2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

"""Code to read and process NSIDC sea-ice concentration data.

    This converter handles data from the NSIDC NASA Team and Bootstrap algorithms.
    It extracts valid observations and applies necessary masking, assigns a constant obs. error (0.1).

    Note: These algorithms have different data types and scaling factors. The NASA Team data are in
    "ubyte" and have a scaling factor of 0.004, while the BT's in "short" format and its scale
    factor is "0.001". They also use different filling values for land and pole hole. But NetCDF4
    package "should" handle this automatically.

    More details:
    https://nsidc.org/data/user-resources/help-center/descriptions-and-differences-between-nasa-team-and-bootstrap-algorithms

    The grid file(s) can be found here:
    https://daacdata.apps.nsidc.org/pub/DATASETS/nsidc0771_polarstereo_anc_grid_info/

    Tested grid file for Arctic: NSIDC0771_LatLon_PS_N25km_v1.1.nc
    Tested grid file for Antarctic: NSIDC0771_LatLon_PS_S25km_v1.1.nc
"""

import os
import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import netCDF4 as nc
import numpy as np

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

# Define IODA missing values
float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

# IODA metadata location keys
LOCATION_KEYS = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z")
]

# Quick access to IODA group conventions
metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()


def read_nsidc_obs(obs_filename: str, grid_filename: str, var_name: str = 'F17_ICECON') -> dict:
    """Reads NSIDC observation and grid NetCDF files and extracts valid observations.

    Filters out invalid NSIDC mask values (land and pole hole flags), converts array
    dimensions, and standardizes data units into unitless fraction [0, 1].

    Args:
        obs_filename (str): Path to the input NSIDC observation NetCDF file.
        grid_filename (str): Path to the corresponding grid NetCDF file containing lat/lon.
        var_name (str, optional): Name of the ice concentration variable inside
            the NetCDF file. Defaults to 'F17_ICECON'.

    Returns:
        dict: A dictionary containing extracted observation data and metadata arrays:
            - 'nobs' (int): Total count of valid observation locations.
            - 'latitude' (np.ndarray): 1D array of latitudes (float32).
            - 'longitude' (np.ndarray): 1D array of longitudes (float32).
            - 'dateTime' (np.ndarray): 1D array of epoch timestamps in seconds (int64).
            - 'seaIceFraction' (np.ndarray): 1D array of ice fraction values [0, 1] (float32).
            - 'seaIceFraction_error' (np.ndarray): 1D array of observation errors (float32).
            - 'seaIceFraction_qc' (np.ndarray): 1D array of quality control flags (int32).

    Raises:
        KeyError: If `var_name` or coordinate variables are missing from the NetCDF files.
        FileNotFoundError: If `obs_filename` or `grid_filename` cannot be located.
    """
    with nc.Dataset(obs_filename, 'r') as ncd, nc.Dataset(grid_filename, 'r') as ncgrid:
        # Extract arrays (handling potential shape variations)
        cice_2d = np.squeeze(ncd.variables[var_name][:])
        lon_2d = np.squeeze(ncgrid.variables['longitude'][:])
        lat_2d = np.squeeze(ncgrid.variables['latitude'][:])

        # Use the existing NetCDF mask directly so land/pole-hole locations
        # are excluded from the output instead of being retained as masked values.
        cice_data = np.ma.getdata(cice_2d)
        invalid_mask = np.ma.getmaskarray(cice_2d)
        invalid_mask |= ~np.isfinite(cice_data)

        # Keep only valid data points to reduce memory/disk footprint
        valid = ~invalid_mask

        cice_1d = cice_data[valid].astype(np.float32)
        lon_1d = np.ma.getdata(lon_2d)[valid].astype(np.float32)
        lat_1d = np.ma.getdata(lat_2d)[valid].astype(np.float32)

        # Convert fraction/percentage to scale [0, 1] if required
        if np.max(cice_1d) > 1.0:
            cice_1d /= 1000.0

        nobs = cice_1d.size

        # Convert days/seconds since epoch to standard integer seconds
        time_var = ncd.variables['time']
        if 'seconds' in getattr(time_var, 'units', ''):
            seconds_since_epoch = int(time_var[0])
        else:
            seconds_since_epoch = int(time_var[0] * 86400)

        dateTime = np.full(nobs, seconds_since_epoch, dtype=np.int64)
        obs_error = np.full(nobs, 0.1, dtype=np.float32)
        qc_flags = np.zeros(nobs, dtype=np.int32)

    return {
        'nobs': nobs,
        'latitude': lat_1d,
        'longitude': lon_1d,
        'dateTime': dateTime,
        'seaIceFraction': cice_1d,
        'seaIceFraction_error': obs_error,
        'seaIceFraction_qc': qc_flags
    }


def write_ioda_file(output_filename: str, obs_data: dict) -> None:
    """Writes extracted observation dictionary into JEDI/IODA NetCDF format.

    Constructs IODA-compliant metadata, variable groups (ObsValue, ObsError, PreQC),
    and attributes using `pyiodaconv`.

    Args:
        output_filename (str): Target path where the output IODA NetCDF file will be written.
        obs_data (dict): Dictionary containing prepared observation arrays as returned by
            `read_nsidc_obs`.
    """
    nobs = obs_data['nobs']

    data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

    # Populate Metadata
    data[('latitude', metaDataName)] = obs_data['latitude']
    data[('longitude', metaDataName)] = obs_data['longitude']
    data[('dateTime', metaDataName)] = obs_data['dateTime']

    varAttrs[('latitude', metaDataName)]['units'] = 'degrees_north'
    varAttrs[('longitude', metaDataName)]['units'] = 'degrees_east'
    varAttrs[('dateTime', metaDataName)]['units'] = 'seconds since 1970-01-01T00:00:00Z'

    varAttrs[('latitude', metaDataName)]['_FillValue'] = float_missing_value
    varAttrs[('longitude', metaDataName)]['_FillValue'] = float_missing_value
    varAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value

    # Populate Variables (seaIceFraction)
    data[('seaIceFraction', obsValName)] = obs_data['seaIceFraction']
    data[('seaIceFraction', obsErrName)] = obs_data['seaIceFraction_error']
    data[('seaIceFraction', qcName)] = obs_data['seaIceFraction_qc']

    varAttrs[('seaIceFraction', obsValName)]['units'] = '1'
    varAttrs[('seaIceFraction', obsErrName)]['units'] = '1'
    varAttrs[('seaIceFraction', qcName)]['units'] = 'unitless'

    varAttrs[('seaIceFraction', obsValName)]['_FillValue'] = float_missing_value
    varAttrs[('seaIceFraction', obsErrName)]['_FillValue'] = float_missing_value
    varAttrs[('seaIceFraction', qcName)]['_FillValue'] = int_missing_value

    # Dimensions Mapping
    DimDict = {'Location': nobs}
    varDims = {
        'latitude': ['Location'],
        'longitude': ['Location'],
        'dateTime': ['Location'],
        'seaIceFraction': ['Location']
    }

    # Execute Build
    writer = iconv.IodaWriter(output_filename, LOCATION_KEYS, DimDict)
    writer.BuildIoda(data, varDims, varAttrs, {})


def main() -> None:
    """Parses command-line arguments and executes the converter workflow."""
    parser = ArgumentParser(
        description='Convert NSIDC sea-ice concentration data to JEDI/IODA format.',
        formatter_class=ArgumentDefaultsHelpFormatter
    )
    parser.add_argument('-i', '--input', type=str, required=True, help='Input NSIDC obs file')
    parser.add_argument('-g', '--gridfile', type=str, required=True, help='NSIDC grid file')
    parser.add_argument('-o', '--output', type=str, default='nsidc_ioda_output.nc', help='Output IODA file')
    parser.add_argument('-v', '--variable', type=str, default='F17_ICECON', help='Observation variable name inside NetCDF')

    args = parser.parse_args()

    obs_data = read_nsidc_obs(args.input, args.gridfile, var_name=args.variable)
    write_ioda_file(args.output, obs_data)


if __name__ == '__main__':
    main()
