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
HPA2PA = 1E2

float_missing_value = iconv.get_default_fill_val(np.float32)


class gcas(object):

    def __init__(self, filenames, column, time_range):

        self.filenames = filenames
        self.column = column
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
            self.AttrData['platform'] = dsFlight.attrs['MeasurementPlatform']
            self.AttrData['description'] = dsFlight.attrs['RevisionNumber']

            # Read lat lon and flatten from 2d (time,xtrack) and make it 1d
            flats = dsFlight['lat'].values.flatten()
            flats = np.where(np.isnan(flats), float_missing_value, flats)
            lats = flats.astype(np.float32)

            flons = dsFlight['lon'].values.flatten()
            flons = np.where(np.isnan(flons), float_missing_value, flons)
            lons = flons.astype(np.float32)

            alts_1d = dsFlight['aircraft_altitude'].values.flatten()
            xtrack = np.arange(dsFlight.dims['xtrack'])  # pixels in swath
            nlocs = lats.shape[0]
            alts = np.repeat(alts_1d, len(xtrack))  # repeat alt for each swath

            lats = lats.astype(np.float32)
            lons = lons.astype(np.float32)
            alts = alts.astype(np.float32)

            # Read 3d model alt (level) and reshape it to 2d
            geoscf = xr.open_dataset(filename, group='_Model_Information')
            # Model alt label in incorrect, change km to m
            fmodel_alt = geoscf['model_altitude']*1000
            level_dim, time_dim, xtrack_dim = fmodel_alt.shape
            model_alt = fmodel_alt.values.reshape(level_dim, time_dim * xtrack_dim)

            # Read 3d model pressure and reshape it to 2d
            fmodel_press = geoscf['model_pressure']*HPA2PA
            model_press = fmodel_press.values.reshape(level_dim, time_dim * xtrack_dim)
            model_press = np.where(np.isnan(model_press), float_missing_value, model_press)
            model_press = model_press.astype(np.float32)

            # Find aircraft P using model pressure and altitude info
            aircraft_p = np.zeros(time_dim * xtrack_dim, dtype=np.float32)
            for t in range(time_dim * xtrack_dim):
                differences = np.abs(np.array(model_alt[:, t]) - np.array(alts[t]))
                idx = np.argmin(differences)
                aircraft_p[t] = model_press[idx, t]

            # Only 2 vertice: surface pressure and flight pressure
            # For total column flight_pressure is 0
            num_vert = 2
            pressure_vertice = np.zeros([time_dim * xtrack_dim, num_vert], dtype=np.float32)
            pressure_vertice[:, 1] = model_press[level_dim-1, :]  # surface pressure from model

            # Read time and convert to ioda time format
            obs_time = dsFlight['time'].values
            time = np.repeat(obs_time, len(xtrack))

            times = (time - np.datetime64(epoch.strftime("%Y-%m-%dT%H:%M:%S"))) / np.timedelta64(1, 's')

            if (self.column.strip() == 'total'):
                var_data_below = dsFlight['no2_vertical_column_below_aircraft'].values.flatten()
                var_data_above = dsFlight['no2_vertical_column_above_aircraft'].values.flatten()
                var_data = (var_data_below+var_data_above) * MOLECCM_2MOLM_2  # change to mol/m2
                var_name = 'nitrogendioxideTotal'
                pressure_vertice[:, 0] = np.zeros(time_dim * xtrack_dim, dtype=np.float32)
            elif (self.column.strip() == 'tropo'):
                var_data = dsFlight['no2_vertical_column_below_aircraft'].values.flatten()*MOLECCM_2MOLM_2
                var_name = 'nitrogendioxideColumn'
                pressure_vertice[:, 0] = aircraft_p

            data = {}
            # flatten 2d (time,xtrack)
            var_data = np.where(np.isnan(var_data), float_missing_value, var_data)
            data[var_name] = var_data.astype(np.float32)

            # set flag
            flag = np.full((nlocs), True)
            obs_error = np.full((nlocs), 0.0).astype(np.float32)
            qa = np.full((nlocs), 0)

            # date range to fit DA window
            wbegin = np.datetime64(datetime.strptime(self.time_range[0], "%Y%m%d%H"))
            wend = np.datetime64(datetime.strptime(self.time_range[1], "%Y%m%d%H"))
            flag = np.where((time >= wbegin) & (time <= wend), 1, 0)
            flag = flag.astype(bool)

            # ---- Write Metadata and data
            if first:

                # add metadata variables
                self.outData[('dateTime', 'MetaData')] = times[flag]
                self.outData[('latitude', 'MetaData')] = lats[flag]
                self.outData[('longitude', 'MetaData')] = lons[flag]
                self.outData[('aircraft_altitude', 'MetaData')] = alts[flag]
                self.outData[('aircraft_pressure', 'MetaData')] = aircraft_p[flag]
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
                self.outData[('aircraft_altitude', 'MetaData')] = np.concatenate(
                    (self.outData[('aircraft_altitude', 'MetaData')], alts[flag]))
                self.outData[('aircraft_pressure', 'MetaData')] = np.concatenate(
                    (self.outData[('aircraft_pressure', 'MetaData')], aircraft_p[flag]))
                self.outData[('pressureVertice', 'RetrievalAncillaryData')] = np.concatenate(
                    (self.outData[('pressureVertice', 'RetrievalAncillaryData')], pressure_vertice[flag]))

                self.outData[(var_name, 'valKey')] = np.concatenate(
                    (self.outData[varDict['valKey']], data[var_name][flag]))
                self.outData[(var_name, 'errKey')] = np.concatenate(
                    (self.outData[self.varDict[var_name]['errKey']], obs_error[flag]))
                self.outData[(var_name, 'qcKey')] = np.concatenate(
                    (self.outData[self.varDict[var_name]['qcKey']], qa[flag]))

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
        obsvars = {"nitrogendioxideTotal",
                   "nitrogendioxideColumn"}
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
            'Reads GCAS column netCDF files from NASA STAQS campaign'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be concatenated.'),
        formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.print_usage = parser.print_help

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of GCAS observation netCDF input file(s)",
        type=str,
        nargs='+',
        required=True)

    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str,
        required=True)

    required.add_argument(
        '-c', '--column',
        help="type of column: total or tropo",
        type=str, required=True)

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

    # Read in the flight data
    flightData = gcas(args.input, args.column, args.time_range)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, flightData.DimDict)

    # write everything out
    writer.BuildIoda(flightData.outData, varDims, flightData.varAttrs, flightData.AttrData)


if __name__ == '__main__':
    main()
