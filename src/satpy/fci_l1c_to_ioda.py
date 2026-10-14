#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Converts MTG-I FCI Level 1C full-disk infrared radiances to IODA brightness temperatures
# with satpy's fci_l1c_nc reader. Input is the set of BODY chunk files of one repeat cycle
# of the FDHSI product, for example EUMETSAT Data Store collection EO:EUM:DAT:0662:
#   W_XX-EUMETSAT-Darmstadt,IMG+SAT,MTI1+FCI-1C-RRAD-FDHSI-FD--CHK-BODY---NC4E_C_EUMT_..._0001.nc
#   ... through chunk 0040 (the TRAIL file is not needed).
#
# FCI channels (CRTM numbering, fci_mtg-i1 coefficients):
#    9 ir_38  3.8 um    10 wv_63  6.3 um    11 wv_73  7.3 um    12 ir_87  8.7 um
#   13 ir_97  9.7 um    14 ir_105 10.5 um   15 ir_123 12.3 um   16 ir_133 13.3 um
#
# Usage: fci_l1c_to_ioda.py -i <repeat cycle dir>/*BODY*.nc -o fci_mtg-i1.nc4 [-s 16]
#

import os

from geo_imager_utils import convert, get_parser

channels = ['ir_38', 'wv_63', 'wv_73', 'ir_87', 'ir_97', 'ir_105', 'ir_123', 'ir_133']
channel_numbers = list(range(9, 17))

# WMO Common Code Table C-5: Meteosat-11 is 70, Meteosat-12 (MTG-I1) is 71, ...
wmo_sat_ids = {f'Meteosat-{n}': 59 + n for n in range(12, 18)}

GlobalAttrs = {
    'platformLongDescription': 'EUMETSAT MTG-I FCI L1C full-disk infrared brightness temperatures',
}


def main():
    parser = get_parser('Convert MTG-I FCI L1C full-disk IR channels to IODA')
    args = parser.parse_args()
    args.input = [f for f in args.input if 'TRAIL' not in os.path.basename(f)]
    GlobalAttrs['converter'] = os.path.basename(__file__)
    # The reader provides the observation time of every pixel, in seconds since 2000-01-01 UTC.
    convert(args, 'fci_l1c_nc', channels, channel_numbers, wmo_sat_ids, GlobalAttrs, ref='ir_105',
            times_from='ir_105_time', times_epoch='2000-01-01T00:00:00', processing_level='L1C')


if __name__ == '__main__':
    main()
