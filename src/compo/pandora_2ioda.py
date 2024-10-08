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
MOLECCM_2MOLM_2 = 10000/6.0221408e+23  # molec/cm2 to mol/m2
MBAR2PA = 1E2

float_missing_value = iconv.get_default_fill_val(np.float32)

class pandora(object):

    def __init__(self, filenames, time_range):
        self.filenames = filenames
        self.time_range = time_range
        self.make_dictionaries()      # Set up variable names for IODA
        self.DimDict = {}
        self.read()    # Read data from file


    def read(self):

        # Loop through input filenames
        first = True
        for filename in self.filenames:

            # Read station lat lon
            with open(filename, 'r', encoding='ISO-8859-1') as file:
                content = file.read()
            
            latitude = re.search(r"Location latitude \[deg\]:\s*([-\d.]+)", content)
            longitude = re.search(r"Location longitude \[deg\]:\s*([-\d.]+)", content)
            lat = float(latitude.group(1))
            lon = float(longitude.group(1))
            
            # Separate the tabular section of pandora txt file or the 3rd section
            sections = content.split('---------------------------------------------------------------------------------------')

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

            times = data[:,0]
            no2 = data[:,38] # no2 total column amount [mole/m2]
            no2_unc = data[:,42] #  Total uncertainty of nitrogen dioxide total vertical column amount [moles per square meter]
            surf_p = data[:,11]*MBAR2PA  # climatological station pressure [mbar]
            nlocs = len(times)

            lats = np.full(nlocs, lat)
            lons = np.full(nlocs, lon)

            lats = lats.astype(np.float32)
            lons = lons.astype(np.float32)

            # 2 vertice: surface pressure and top (0)
            num_vert = 2
            pressure_vertice = np.zeros([nlocs,num_vert], dtype=np.float32)
            pressure_vertice[:, 1] = surf_p


            # set flag
            flag = np.full((nlocs), True)
            #obs_error = np.full((nlocs), 0.0).astype(np.float32)
            obs_error = no2_unc.astype(np.float32)
            qa = np.full((nlocs), 0)

            # date range to fit DA window
            time = np.array([datetime.strptime(date, '%Y%m%dT%H%M%S.%fZ') for date in times])
            iodatime = np.array([date.strftime('%Y-%m-%dT%H:%M:%SZ') for date in time],dtype='object')

            wbegin = np.datetime64(datetime.strptime(self.time_range[0], "%Y%m%d%H"))
            wend = np.datetime64(datetime.strptime(self.time_range[1], "%Y%m%d%H"))
            flag_time = np.where((time >= wbegin) & (time <= wend), 1, 0)
            flag_neg = np.where(no2>0, 1, 0)
            flag = np.logical_and(flag_time, flag_neg)

            flag = flag.astype(bool)

            # Write MetaData and data
            var_name = "nitrogendioxideTotal"
            data = {}
            data[var_name] = no2.astype(np.float32)
            if first:
                self.outData[('dateTime', 'MetaData')] = iodatime[flag]
                self.outData[('latitude', 'MetaData')] = lats[flag]
                self.outData[('longitude', 'MetaData')] = lons[flag]
                self.outData[('pressureVertice', 'RetrievalAncillaryData')] = pressure_vertice[flag]

                varDict = self.varDict.get(var_name)
                self.outData[varDict['valKey']] = \
                    data[var_name][flag]
                self.outData[self.varDict[var_name]['errKey']] = \
                    obs_error[flag]
                self.outData[self.varDict[var_name]['qcKey']] = \
                    qa[flag]
            else:
                self.outData[('dateTime', 'MetaData')] = np.concatenate(
                    (self.outData[('dateTime', 'MetaData')], times[flag]))
                self.outData[('latitude', 'MetaData')] = np.concatenate(
                    (self.outData[('latitude', 'MetaData')], lats[flag]))
                self.outData[('longitude', 'MetaData')] = np.concatenate(
                    (self.outData[('longitude', 'MetaData')], lons[flag]))
                self.outData[('pressureVertice', 'RetrievalAncillaryData')] = np.concatenate(
                    (self.outData[('pressureVertice', 'RetrievalAncillaryData')], pressure_vertice[flag]))

                self.outData[(var_name, 'valKey')] = np.concatenate(
                    (self.outData[(var_name, 'valKey')], times[flag]))
                self.outData[(var_name, 'errKey')] = np.concatenate(
                    (self.outData[(var_name, 'errKey')], times[flag]))
                self.outData[(var_name, 'qcKey')] = np.concatenate(
                    (self.outData[(var_name, 'qcKey')], times[flag]))

            first = False

        self.DimDict['Location'] = len(self.outData[('dateTime', 'MetaData')])
        self.AttrData['Location'] = np.int32(self.DimDict['Location'])

        self.DimDict['Vertice'] = num_vert  # surface and aircraft
        self.AttrData['Vertice'] = np.int32(self.DimDict['Vertice'])

        varname = 'pressureVertice'
        vkey = (varname, 'RetrievalAncillaryData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'Pa'

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
        '-r', '--time_range',
        help="extract a date range to fit the data assimilation window"
        "format -r YYYYMMDDHH YYYYMMDDHH",
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('1970010100', '2170010100'))

    return parser


def main():

    locationKeyList = [
        ("latitude", "float", "degrees_north"),
        ("longitude", "float", "degrees_east"),
        ("dateTime", "long", iso8601_string),
    ]

    varDims = {
        'x': ['Location'],
        'pressureVertice': ['Location', 'Vertice']
    }

    # -- read command line arguments
    parser = get_parser()
    args = parser.parse_args()

    # Read in the pandora station data
    pandoraData = pandora(args.input, args.time_range)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, pandoraData.DimDict)

    # write everything out
    writer.BuildIoda(pandoraData.outData, varDims, pandoraData.varAttrs, pandoraData.AttrData)


if __name__ == '__main__':
    main()
