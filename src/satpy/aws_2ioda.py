#!/usr/bin/env python3

#
# (C) Copyright 2020-2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# author: Benjamin Ruston
# Use a satpy reader to ingest ESA Arctic Weather Satellite (AWS) data
# and output to the JEDI IODA format
#

from datetime import datetime, timedelta
import numpy as np
from satpy.scene import Scene
# from satpy.readers import aws1_mwr_l1b_nc

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import (
    compute_scan_angle,
    concat_obs_dict,
    epoch,
    ioda_float_type,
    ioda_int_type,
    set_metadata_attributes,
    set_obspace_attributes,
)

# globals
AWS_PFM_WMO_sat_ID = 80

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()

GlobalAttrs = {
    "platformCommonName": "AWS",
    "platformLongDescription": "ESA Arctic Weather Satellite L1B Brightness Temperature Data",
    "sensorCentralWavelength": "[50.3, 89, 165.5, 175.31-191.31, 317.15-333.15, 52.61-57.61]",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]


def get_aws_scene(args):

    """
    decode an ESA AWS L1B file using satpy

    Args:
        filename list - list of files to decode

    Returns:
       obs_scene - the resampled obs_scene to a common lat/lon projection
       ancillary_data - scene lat, lon, dateTime, sensorZenigth and scanPosition
    """

    # filename(s) to be read example below
    # filenames = ['MSG4-SEVI-MSG15-0100-NA-20220622191243.890000000Z-NA.nat']

    # what datasets are available
    filenames = args.input
#   header = aws1_mwr_l1b_nc.read_header(filenames[0])
#   available_datasets = aws1_mwr_l1b_nc.get_available_channels(header)
#   aload = [k for k, v in available_datasets.items() if v]

    # load Scene
    scn = Scene(reader="aws1_mwr_l1b_nc", filenames=filenames)
#   scn.load(aload)

    # ensure the the loaded datasets in the Scene are calibrated (version dependent)
    # scn.calibrate()

    # ancillary information
    # latitude, longitude and dateTime, satelliteZenith, sensorScan
#   lat, lon, satellite_zenith_angle = get_zenith_angle(scn)
    # get a time for each pixel on new target area
#   locationDateTime = get_pixel_time(scn)
    # scan position is the x-coordinate
#   sensorScanPosition = get_scanPosition(scn)

    # set ancillary_data
    ancillary_data = { }
#   ancillary_data = {
#       'lat': lat,
#       'lon': lon,
#       'dateTime': locationDateTime,
#       'satellite_zenith_angle': satellite_zenith_angle,
#       'sensor_scan_position': sensorScanPosition,
#   }

    return scn, ancillary_data


def main():

    from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
    import os
    desc = 'Convert AWS L1B into IODA convention use a netCDF4 backend'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="full path name of satellite observation input file(s)",
        type=str, nargs='+', required=True, default=None)
    required.add_argument(
        '-o', '--output',
        help='name of the output netCDF IODA-compliant file',
        type=str, required=True, default='output.nc')
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-d', '--date',
        metavar="YYYYMMDDTHHMMSSZ",
        help="base dateTime for observation window",
        type=str, required=False, default=None)

    args = parser.parse_args()

    GlobalAttrs['converter'] = os.path.basename(__file__)
    obs_scene, ancillary_data = get_aws_scene(args)

#   VarDims, VarAttrs, DimDict = get_obs_properties(obs_scene)

#   obs = variables_to_obs(obs_scene, ancillary_data, VarDims)
#   del obs_scene
#   del ancillary_data
#   platform = get_platform_short_name(obs[('satelliteIdentifier', metaDataName)][0])
#   GlobalAttrs["platformCommonName"] = " ".join([GlobalAttrs["platformCommonName"], platform])

    # setup the IODA writer
#   writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    # write everything out
#   writer.BuildIoda(obs, VarDims, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
