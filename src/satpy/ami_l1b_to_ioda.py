#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Converts GEO-KOMPSAT-2A AMI Level 1B full-disk infrared radiances to IODA brightness
# temperatures with satpy's ami_l1b reader. Input is the set of per-channel netCDF files
# of one full-disk scan, for example from NOAA open data:
#   s3://noaa-gk2a-pds/AMI/L1B/FD/YYYYMM/DD/HH/gk2a_ami_le1b_ir105_fd020ge_YYYYMMDDHHMM.nc
#
# AMI channels (CRTM numbering, ami_gk2 coefficients):
#    7 SW038  3.8 um     8 WV063  6.3 um     9 WV069  6.9 um    10 WV073  7.3 um
#   11 IR087  8.7 um    12 IR096  9.6 um    13 IR105 10.4 um    14 IR112 11.2 um
#   15 IR123 12.4 um    16 IR133 13.3 um
#
# Usage: ami_l1b_to_ioda.py -i gk2a_ami_le1b_*_fd020ge_202609070000.nc -o ami_gk2a.nc4 [-s 16]
#

import os

from geo_imager_utils import convert, get_parser

channels = ['SW038', 'WV063', 'WV069', 'WV073', 'IR087', 'IR096', 'IR105', 'IR112', 'IR123', 'IR133']
channel_numbers = list(range(7, 17))

# WMO Common Code Table C-5
wmo_sat_ids = {'GEO-KOMPSAT-2A': 811}

GlobalAttrs = {
    'platformLongDescription': 'KMA GEO-KOMPSAT-2A AMI L1B full-disk infrared brightness temperatures',
}


def main():
    parser = get_parser('Convert GEO-KOMPSAT-2A AMI L1B full-disk IR channels to IODA')
    args = parser.parse_args()
    GlobalAttrs['converter'] = os.path.basename(__file__)
    # AMI scans the full disk from north to south; the reader gives no per-line time.
    convert(args, 'ami_l1b', channels, channel_numbers, wmo_sat_ids, GlobalAttrs, ref='IR105', north_to_south=True)


if __name__ == '__main__':
    main()
