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
from datetime import datetime, timezone, timedelta
import os
from pathlib import Path

import xarray as xr
import math
from numpy import log as ln

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import epoch, iso8601_string


# constants
HPA2PA = 1E2
ppbv2molmol = 1e-9
pptv2molmol = 1e-12

float_missing_value = iconv.get_default_fill_val(np.float32)

# {'iodaName' : ['obsName', 'iodaUnit', 'obsToIodaUnivConv']
obsvars = {'nitrogendioxideInsitu': ['NO2_ACES', 'mol mol-1', ppbv2molmol],
           'carbonmonoxideInsitu': ['CO_ppb', 'mol mol-1', ppbv2molmol],
           'ozoneInsitu': ['O3_CL', 'mol mol-1', ppbv2molmol],
           'formaldehydeInsitu': ['CH2O_ISAF', 'mol mol-1', pptv2molmol],
           'airTemperature': ['T', 'K', 1],
           'windEastward': ['U', 'm s-1', 1],
           'windNorthward': ['V', 'm s-1', 1]}


class icartt(object):

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
            try:
                dsFlight = xr.open_dataset(filename)
            except IOError:
                raise IOError('%s file not found!' % self.filename)
            except Exception:
                raise Exception('Unknown error opening %s' % self.filename)

            # Read global attributes
            self.AttrData['platform'] = dsFlight.attrs['PLATFORM']
            self.AttrData['description'] = dsFlight.attrs['RA']

            # Read lat lon
            lats = dsFlight['Latitude'].values
            lons = dsFlight['Longitude'].values

            lats = lats.astype(np.float32)
            lons = lons.astype(np.float32)

            # Read time and convert to ioda time format
            # sec since SDATE
            stime = dsFlight['iWAS_Start_UTC'].values
            sdate_str = dsFlight.attrs['SDATE']
            sdate = datetime.strptime(sdate_str, '%Y, %m, %d')
            sdate = sdate.replace(tzinfo=timezone.utc)
            seconds_difference = (sdate - epoch).total_seconds()

            times = stime + seconds_difference
            stime_datetime = np.datetime64(epoch.strftime("%Y-%m-%dT%H:%M:%S")) + np.timedelta64(int(seconds_difference), 's') + stime.astype('timedelta64[s]')

            pressure = dsFlight['P'] * HPA2PA  # hPa to Pa
            pressure = pressure.astype(np.float32)
            nlocs = pressure.shape[0]

            data = {}
            for var in self.obsvars:
                var_data = (dsFlight[self.obsvars[var][0]] * self.obsvars[var][2])
                var_data = np.where(np.isnan(var_data), float_missing_value, var_data)
                data[var] = var_data.astype(np.float32)

            # set flag
            flag = np.full((nlocs), True)
            obs_error = np.full((nlocs), 0.0).astype(np.float32)
            qa = np.full((nlocs), 0)

            # date range to fit DA window
            wbegin = np.datetime64(datetime.strptime(self.time_range[0], "%Y%m%d%H"))
            wend = np.datetime64(datetime.strptime(self.time_range[1], "%Y%m%d%H"))
            flag = np.where((stime_datetime >= wbegin) & (stime_datetime <= wend), 1, 0)
            flag = flag.astype(bool)

            # ---- Write Metadata and data
            if first:

                # add metadata variables
                self.outData[('dateTime', 'MetaData')] = times[flag]
                self.outData[('latitude', 'MetaData')] = lats[flag]
                self.outData[('longitude', 'MetaData')] = lons[flag]
                self.outData[('pressure', 'MetaData')] = pressure[flag]

                for var in self.obsvars:
                    self.outData[self.varDict[var]['valKey']] = \
                        data[var][flag]
                    self.outData[self.varDict[var]['errKey']] = \
                        obs_error[flag]
                    self.outData[self.varDict[var]['qcKey']] = \
                        qa[flag]

            else:
                self.outData[('dateTime', 'MetaData')] = np.concatenate(
                    (self.outData[('dateTime', 'MetaData')], times[flag]))
                self.outData[('latitude', 'MetaData')] = np.concatenate(
                    (self.outData[('latitude', 'MetaData')], lats[flag]))
                self.outData[('longitude', 'MetaData')] = np.concatenate(
                    (self.outData[('longitude', 'MetaData')], lons[flag]))

                for var in self.obsvars:
                    self.outData[(var, 'valKey')] = np.concatenate(
                        (self.outData[self.varDict[var]['valKey']], data[var][flag]))
                    self.outData[(var_name, 'errKey')] = np.concatenate(
                        (self.outData[self.varDict[var]['errKey']], obs_error[flag]))
                    self.outData[(var_name, 'qcKey')] = np.concatenate(
                        (self.outData[self.varDict[var]['qcKey']], qa[flag]))

            first = False

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
            self.varAttrs[item, iconv.OvalName()]['units'] = self.obsvars[item][1]
            self.varAttrs[item, iconv.OerrName()]['units'] = self.obsvars[item][1]
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
            'Reads icartt_nc files'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be concatenated.'),
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.print_usage = parser.print_help

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of icartt_nc observation netCDF input file(s)",
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
        ("pressure", "float", "Pa"),
        ("dateTime", "long", iso8601_string),
    ]

    varDims = {}
    for key in obsvars.keys():
        variable = obsvars[key][0]
        varDims[variable] = ['Location']

    # -- read command line arguments
    parser = get_parser()
    args = parser.parse_args()

    # Read in the flight data
    flightData = icartt(args.input, args.time_range)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, flightData.DimDict)

    # write everything out
    writer.BuildIoda(flightData.outData, varDims, flightData.varAttrs, flightData.AttrData)


if __name__ == '__main__':
    main()
