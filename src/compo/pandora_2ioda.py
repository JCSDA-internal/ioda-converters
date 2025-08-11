#!/usr/bin/env python3

#
# (C) Copyright 2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import sys
import argparse
import netCDF4 as nc
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
import os
import re
from pathlib import Path

import xarray as xr
import math
from numpy import log as ln

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import epoch, iso8601_string

# constants
MBAR2PA = 1E2

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)


class pandora(object):

    def __init__(self, filenames, date_range, site_classification):
        self.filenames = filenames
        self.date_range = date_range
        self.site_classification = site_classification
        self.make_dictionaries()      # Set up variable names for IODA
        self.DimDict = {}
        self.read()    # Read data from file

    def read(self):

        # Loop through input filenames
        first = True
        for filename in self.filenames:
            print(filename)
            # Read station lat lon
            with open(filename, 'r', encoding='ISO-8859-1') as file:
                content = file.read()

            latitude = re.search(r"Location latitude \[deg\]:\s*([-\d.]+)", content)
            longitude = re.search(r"Location longitude \[deg\]:\s*([-\d.]+)", content)

            lat = float(latitude.group(1))
            lon = float(longitude.group(1))

            # Separate the tabular section of pandora txt file or the 3rd section
            sections = content.split('-' * 87)

            if len(sections) >= 3:
                table_content = sections[2].strip()
                # Rows do not have the same number of columms
                # because of optional columns at the end of some rows
                # Find max num of cols and pad rows with missing columns with NaN
                lines = table_content.split('\n')
                rows = []
                # except for datetime convert to float
                for line in lines:
                    row = []
                    for value in line.split():
                        try:
                            row.append(float(value))
                        except ValueError:
                            row.append(value)
                    rows.append(row)

                max_columns = max(len(row) for row in rows)
                padded_rows = [row + [np.nan] * (max_columns - len(row)) for row in rows]
                data = np.array(padded_rows, dtype=object)
            else:
                print("The input file is not in the standard format")

            if data.size == 0 or data.shape[1] == 0:
                print(f"{filename} is empty")
                continue

            times = data[:, 0]
            no2 = data[:, 38]  # no2 total column amount [mole/m2]
            no2_unc = data[:, 42]  # Total uncertainty of nitrogen dioxide total vertical column amount [moles per square meter]
            surf_p = data[:, 11]*MBAR2PA  # climatological station pressure [mbar]
            nlocs = len(times)

            lats = np.full(nlocs, lat)
            lons = np.full(nlocs, lon)

            lats = lats.astype(np.float32)
            lons = lons.astype(np.float32)
            aq_class = np.zeros(nlocs, dtype=np.int32)

            if self.site_classification:
                print('site classification file is available')
                pandora_sites = pd.read_csv(self.site_classification)
                pandora_lats = pandora_sites['lat']
                pandora_lons = pandora_sites['lon']

                lat_tol = 0.01
                lon_tol = 0.01
                # Filter rows within the tolerance range
                close_rows = pandora_sites[
                    (pandora_lats >= lat - lat_tol) & (pandora_lats <= lat + lat_tol)
                    & (pandora_lons >= lon - lon_tol) & (pandora_lons <= lon + lon_tol)]

                # Find the urb_class of the closest row(s)
                if not close_rows.empty:
                    closest_row = close_rows.iloc[0]  # Select the first closest row
                    urb_class = closest_row['urb_class']
                    elements = closest_row['pct_urb'].strip("[]").split()
                    float_elements = list(map(float, elements))
                    pct_urb_L = float_elements[0]
                    pct_urb_M = float_elements[1]
                    pct_urb_H = float_elements[2]
                    print(closest_row['File'])
                    print(f"The closest urb_class is: {urb_class}")
                else:
                    print("No nearby location found within the tolerance. assign UNKNOWN")
                    urb_class = 0

            aq_class = np.full(nlocs, urb_class)
            if urb_class ==0:
                pct_urb_L = np.full(nlocs, 0, dtype=np.float32)
                pct_urb_M = np.full(nlocs, 0, dtype=np.float32)
                pct_urb_H = np.full(nlocs, 0, dtype=np.float32)
            else:
                pct_urb_L = np.full(nlocs, pct_urb_L, dtype=np.float32)
                pct_urb_M = np.full(nlocs, pct_urb_M, dtype=np.float32)
                pct_urb_H = np.full(nlocs, pct_urb_H, dtype=np.float32)

            #pct_urb_L = pct_urb_L.astype(np.float32)
            #pct_urb_M = pct_urb_M.astype(np.float32)
            #pct_urb_H = pct_urb_H.astype(np.float32)

            # set flag
            flag = np.full((nlocs), True)
            flag_time = np.full((nlocs), 0)
            obs_error = no2_unc.astype(np.float32)
            qa = np.full((nlocs), 0)

            # date range to fit DA window
            time = np.array([datetime.strptime(date, '%Y%m%dT%H%M%S.%fZ') for date in times])
            iodatime = np.array([date.strftime('%Y-%m-%dT%H:%M:%SZ') for date in time], dtype='object')

            wbegin = np.datetime64(datetime.strptime(self.date_range[0], "%Y%m%d%H%M"))
            wend = np.datetime64(datetime.strptime(self.date_range[1], "%Y%m%d%H%M"))
            flag_time = np.where((time >= wbegin) & (time <= wend), 1, 0)

            flag_neg = np.where(no2 > 0, 1, 0)
            flag = np.logical_and(flag_time, flag_neg)

            if (np.sum(flag) == 0):
                print(f"No data within time range in {filename}")
                continue

            flag = flag.astype(bool)

            # Write MetaData and data
            var_name = "nitrogendioxideTotal"
            data = {}
            data[var_name] = no2.astype(np.float32)
            if first:
                self.outData[('dateTime', 'MetaData')] = iodatime[flag]
                self.outData[('latitude', 'MetaData')] = lats[flag]
                self.outData[('longitude', 'MetaData')] = lons[flag]
                self.outData[('airQualityClassification', 'MetaData')] = aq_class[flag]
                self.outData[('pctUrbL', 'MetaData')] = pct_urb_L[flag]
                self.outData[('pctUrbM', 'MetaData')] = pct_urb_M[flag]
                self.outData[('pctUrbH', 'MetaData')] = pct_urb_H[flag]

                self.outData[self.varDict[var_name]['valKey']] = \
                    data[var_name][flag]
                self.outData[self.varDict[var_name]['errKey']] = \
                    obs_error[flag]
                self.outData[self.varDict[var_name]['qcKey']] = \
                    qa[flag]
            else:
                self.outData[('dateTime', 'MetaData')] = np.concatenate(
                    (self.outData[('dateTime', 'MetaData')], iodatime[flag]))
                self.outData[('latitude', 'MetaData')] = np.concatenate(
                    (self.outData[('latitude', 'MetaData')], lats[flag]))
                self.outData[('longitude', 'MetaData')] = np.concatenate(
                    (self.outData[('longitude', 'MetaData')], lons[flag]))
                self.outData[('airQualityClassification', 'MetaData')] = np.concatenate(
                    (self.outData[('airQualityClassification', 'MetaData')], aq_class[flag]))
                self.outData[('pctUrbL', 'MetaData')] = np.concatenate(
                    (self.outData[('pctUrbL', 'MetaData')], pct_urb_L[flag]))
                self.outData[('pctUrbM', 'MetaData')] = np.concatenate(
                    (self.outData[('pctUrbM', 'MetaData')], pct_urb_M[flag]))
                self.outData[('pctUrbH', 'MetaData')] = np.concatenate(
                    (self.outData[('pctUrbH', 'MetaData')], pct_urb_H[flag]))

                self.outData[self.varDict[var_name]['valKey']] = np.concatenate(
                    (self.outData[self.varDict[var_name]['valKey']], data[var_name][flag]))
                self.outData[self.varDict[var_name]['errKey']] = np.concatenate(
                    (self.outData[self.varDict[var_name]['errKey']], obs_error[flag]))
                self.outData[self.varDict[var_name]['qcKey']] = np.concatenate(
                    (self.outData[self.varDict[var_name]['qcKey']], qa[flag]))

            first = False

        # No data within range in any sites
        if len(self.outData[('dateTime', 'MetaData')]) == 0:
            print("No data within the time range in any site")
            sys.exit(0)

        self.DimDict['Location'] = len(self.outData[('dateTime', 'MetaData')])
        self.AttrData['Location'] = np.int32(self.DimDict['Location'])

    def make_dictionaries(self):
        """
        Make all the necessary dictionaries for this class object.
        """

        self.outData = defaultdict(lambda: DefaultOrderedDict(OrderedDict))

        self.make_obsVars()
        self.make_AttrData()
        self.make_varDict()
        self.make_varAttrs()

    def make_obsVars(self):
        """
        Make a dictionary of obsvars
        """
        obsvars = {"nitrogendioxideTotal"}
        self.obsvars = obsvars

    def make_AttrData(self):
        """
        Make a dictionary of AttrData based on obsvars
        """
        AttrData = {
            'converter': os.path.basename(__file__),
            'nvars': np.int32(len(self.obsvars))
        }
        self.AttrData = AttrData

    def make_varDict(self):
        """
        """
        self.varDict = defaultdict(lambda: defaultdict(dict))
        for item in self.obsvars:
            self.varDict[item]['valKey'] = item, iconv.OvalName()
            self.varDict[item]['errKey'] = item, iconv.OerrName()
            self.varDict[item]['qcKey'] = item, iconv.OqcName()

    def make_varAttrs(self):
        """
        Assign attribute to each self.obsvar
        """
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        for item in self.obsvars:
            self.varAttrs[item, iconv.OvalName()]['units'] = 'mol m-2'
            self.varAttrs[item, iconv.OerrName()]['units'] = 'mol m-2'
            self.varAttrs[item, iconv.OqcName()]['units'] = 'unitless'
            self.varAttrs[item, iconv.OvalName()]['_FillValue'] = float_missing_value
            self.varAttrs[item, iconv.OerrName()]['_FillValue'] = float_missing_value
        self.varAttrs[('airQualityClassification', 'MetaData')]['_FillValue'] = int_missing_value


def get_parser():
    """
    Get the parser object for this script.
    Returns:
        parser (ArgumentParser): ArgumentParser which includes all the parser information.
    """

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads Pandora L2 total columns of NO2 from txt files (L2_rnvs3)'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be concatenated.'),
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.print_usage = parser.print_help

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of Pandora measurement txt file",
        type=str,
        nargs='+',
        required=True)

    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str,
        required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--date_range',
        help="extract a date range to fit the data assimilation window"
        "format --date_range YYYYMMDDHHmm YYYYMMDDHHmm",
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('197001010000', '217001010000'))

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--site_classification',
        help="site classification file with lat, lon, urb_class info"
        "‘UNKNOWN’:0, ‘RURAL’:1,"
        "‘SUBURBAN’:2, ‘URBAN AND CENTER CITY’:3",
        type=str, default='')

    return parser


def main():

    locationKeyList = [
        ("latitude", "float", "degrees_north"),
        ("longitude", "float", "degrees_east"),
        ("dateTime", "long", iso8601_string),
    ]

    varDims = {
        'x': ['Location'],
    }

    # -- read command line arguments
    parser = get_parser()
    args = parser.parse_args()

    # Read in the pandora station data
    pandoraData = pandora(args.input, args.date_range, args.site_classification)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, pandoraData.DimDict)

    # write everything out
    writer.BuildIoda(pandoraData.outData, varDims, pandoraData.varAttrs, pandoraData.AttrData)


if __name__ == '__main__':
    main()
