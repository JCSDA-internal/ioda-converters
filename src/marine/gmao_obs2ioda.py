#!/usr/bin/env python

#
# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import ioda_conv_ncio as iconv
import netCDF4 as nc4
from datetime import datetime
from orddicts import DefaultOrderedDict


class GMAOobs(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date

        # Read GMAO data
        self._read()

        return

    def _read(self):

        data = {}

        nc = nc4.Dataset(self.filename)

        data['nobs'] = len(nc.dimensions['nobs'])

        data['typ'] = nc.variables['typ'][:].data
        data['lon'] = nc.variables['lon'][:].data
        data['lat'] = nc.variables['lat'][:].data
        data['depth'] = nc.variables['depth'][:].data
        data['value'] = nc.variables['value'][:].data
        data['oerr'] = nc.variables['oerr'][:].data

        nc.close()

        self.data = data

        return


class IODA(object):

    def __init__(self, filename, date, varDict, obsList):
        '''
        Initialize IODA writer class,
        transform to IODA data structure and,
        write out to IODA file.
        '''

        self.filename = filename
        self.date = date
        self.varDict = varDict

        self.locKeyList = [
            ("latitude", "float"),
            ("longitude", "float"),
            ("depth", "float"),
            ("datetime", "string")
        ]

        self.AttrData = {
            'odb_version': 1,
            'date_time_string': self.date.strftime("%Y-%m-%dT%H:%M:%SZ")
        }

        self.writer = iconv.NcWriter(self.filename, [], self.locKeyList)

        self.keyDict = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        for key in self.varDict.keys():
            value = self.varDict[key]
            self.keyDict[key]['valKey'] = value, self.writer.OvalName()
            self.keyDict[key]['errKey'] = value, self.writer.OerrName()
            self.keyDict[key]['qcKey'] = value, self.writer.OqcName()

        # data is the dictionary containing IODA friendly data structure
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        recKey = 0

        for obs in obsList:

            if obs.data['nobs'] <= 0:
                print('No GMAO observations for IODA!')
                continue

            for n in range(obs.data['nobs']):

                lat = obs.data['lat'][n]
                lon = obs.data['lon'][n]
                lvl = obs.data['depth'][n]

                locKey = lat, lon, lvl, obs.date.strftime("%Y-%m-%dT%H:%M:%SZ")

                typ = obs.data['typ'][n]
                val = obs.data['value'][n]
                err = obs.data['oerr'][n]
                qc = 0

                valKey = self.keyDict[typ]['valKey']
                errKey = self.keyDict[typ]['errKey']
                qcKey = self.keyDict[typ]['qcKey']

                self.data[recKey][locKey][valKey] = val
                self.data[recKey][locKey][errKey] = err
                self.data[recKey][locKey][qcKey] = qc

        (ObsVars, RecMdata, LocMdata, VarMdata) = self.writer.ExtractObsData(self.data)
        self.writer.BuildNetcdf(ObsVars, RecMdata, LocMdata, VarMdata, self.AttrData)

        return


if __name__ == '__main__':

    desc = 'Convert GMAO ocean data to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the input GMAO ocean obs file',
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output', help='name of the output IODA file',
        type=str, required=True)
    parser.add_argument(
        '-d', '--date', help='file date', metavar='YYYYMMDDHH',
        type=str, required=True)

    args = parser.parse_args()

    fList = args.input
    foutput = args.output
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    obsList = []
    for fname in fList:
        obsList.append(GMAOobs(fname, fdate))

    # varDict is defined as obsid_dict in ocean_obs.py
    varDict = {
        5521: 'sea_water_salinity',  # Salinity
        3073: 'sea_water_temperature',  # Temperature
        5525: 'sea_surface_temperature',  # SST
        5526: 'obs_absolute_dynamic_topography',  # SSH (Not used ...)
        5351: 'obs_absolute_dynamic_topography',  # SSH
        6000: 'sea_ice_area_fraction',  # AICE
        6001: 'sea_ice_thickness'   # HICE
    }

    IODA(foutput, fdate, varDict, obsList)
