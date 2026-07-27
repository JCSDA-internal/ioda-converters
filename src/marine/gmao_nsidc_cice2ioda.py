#!/usr/bin/env python3

# (C) Copyright 2019-2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

import os
import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import netCDF4 as nc
import numpy as np
from datetime import datetime

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

# IODA variable and metadata definitions
locationKey = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z")
]

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

dtypes = {'integer': np.int32, 'long': np.int64, 'float': np.float32}

class NSIDCobs:
    """Class to read and process NSIDC sea-ice concentration data.

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

    Attributes:
        filename (str): Path to the input NSIDC NetCDF observation file.
        gridname (str): Path to the NSIDC grid NetCDF file.
        data (dict): Dictionary containing the processed IODA-ready data arrays.
    """

    def __init__(self, filename, gridname):
        """Initializes NSIDCobs with input files and triggers reading.

        Args:
            filename (str): Path to the input NSIDC observation file.
            gridname (str): Path to the NSIDC grid file.
        """
        self.filename = filename
        self.gridname = gridname
        self._read()

    def _read(self):
        """Reads and masks the NSIDC data from the provided files.

        Applies masking for land and pole holes and flattens the 2D grid
        into 1D observation arrays.
        """
        ncd = nc.Dataset(self.filename)
        ncgrid = nc.Dataset(self.gridname)

        # Read only the required slice directly from disk
        cice_2d = ncd.variables['F17_ICECON'][0, :, :]
        lon_2d = ncgrid.variables['longitude'][:, :]
        lat_2d = ncgrid.variables['latitude'][:, :]

        # Create a combined mask (True where we want to KEEP the data)
        # Assuming we want to exclude land (1200) and pole hole (1100)
        # And usually valid concentration is 0-1000 (if scaled) or 0-1.0
        # Check for missing values as well if they exist
        mask = (cice_2d != 1200) & (cice_2d != 1100)

        # Flatten using the mask to only keep valid observations
        cice_1d = cice_2d[mask].flatten().astype(np.float32)
        lon_1d = lon_2d[mask].flatten().astype(np.float32)
        lat_1d = lat_2d[mask].flatten().astype(np.float32)

        nobs = cice_1d.size

        # Convert days since epoch to seconds
        seconds_since_epoch = int(ncd.variables['time'][0] * 86400)
        dateTime = np.full(nobs, seconds_since_epoch, dtype=np.int64)
        obs_error = np.full(nobs, 0.1, dtype=np.float32)

        self.data = {
            'nobs': nobs,
            'latitude': lat_1d,
            'longitude': lon_1d,
            'dateTime': dateTime,
            'seaIceFraction': cice_1d,
            'seaIceFraction_error': obs_error,
            'seaIceFraction_qc': np.zeros(nobs, dtype=np.int32)
        }
        ncd.close()
        ncgrid.close()

class IODA:
    """Class to write processed observation data into IODA format.

    This class takes the data processed by NSIDCobs and uses the ioda_conv_engines
    to create a JEDI-compatible IODA NetCDF file.

    Attributes:
        filename (str): Path where the output IODA file will be written.
        obs (NSIDCobs): An instance of NSIDCobs containing the processed data.
    """

    def __init__(self, filename, obs):
        """Initializes IODA and triggers the writing process.

        Args:
            filename (str): Output IODA file path.
            obs (NSIDCobs): Processed observation data.
        """
        self.filename = filename
        self.obs = obs
        self.write_ioda()

    def write_ioda(self):
        """Builds and writes the IODA file using the ioda_conv_engines.
        """
        nobs = self.obs.data['nobs']
        data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        data[('latitude', metaDataName)] = self.obs.data['latitude']
        data[('longitude', metaDataName)] = self.obs.data['longitude']
        data[('dateTime', metaDataName)] = self.obs.data['dateTime']
        data[('seaIceFraction', obsValName)] = self.obs.data['seaIceFraction']
        data[('seaIceFraction', obsErrName)] = self.obs.data['seaIceFraction_error']
        data[('seaIceFraction', qcName)] = self.obs.data['seaIceFraction_qc']

        varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        varAttrs[('latitude', metaDataName)]['units'] = 'degrees_north'
        varAttrs[('longitude', metaDataName)]['units'] = 'degrees_east'
        varAttrs[('dateTime', metaDataName)]['units'] = 'seconds since 1970-01-01T00:00:00Z'
        varAttrs[('seaIceFraction', obsValName)]['units'] = '1'
        varAttrs[('seaIceFraction', obsErrName)]['units'] = '1'
        varAttrs[('seaIceFraction', qcName)]['units'] = 'unitless'
        varAttrs[('latitude', metaDataName)]['_FillValue'] = float_missing_value
        varAttrs[('longitude', metaDataName)]['_FillValue'] = float_missing_value
        varAttrs[('dateTime', metaDataName)]['_FillValue'] = long_missing_value
        varAttrs[('seaIceFraction', obsValName)]['_FillValue'] = float_missing_value
        varAttrs[('seaIceFraction', obsErrName)]['_FillValue'] = float_missing_value
        varAttrs[('seaIceFraction', qcName)]['_FillValue'] = int_missing_value

        DimDict = {'Location': nobs}
        varDims = {'seaIceFraction': ['Location']}

        writer = iconv.IodaWriter(self.filename, locationKey, DimDict)
        writer.BuildIoda(data, varDims, varAttrs, {})

def main():
    """Main execution block for NSIDC to IODA conversion.
    """
    parser = ArgumentParser(
        description='Convert NSIDC sea-ice concentration data (NASA Team and Bootstrap) to IODA format.',
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument('-i', '--input', type=str, required=True, help='Input NSIDC obs file')
    parser.add_argument('-g', '--gridfile', type=str, required=True, help='NSIDC grid file')
    parser.add_argument('-o', '--output', type=str, default='nsidc_ioda_output.nc', help='Output IODA file')
    args = parser.parse_args()

    obs = NSIDCobs(args.input, args.gridfile)
    IODA(args.output, obs)

if __name__ == '__main__':
    main()
