#!/usr/bin/env python3

# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

"""
Convert GMAO ocean data to IODA netCDF4 format
"""

from __future__ import print_function
import sys
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import netCDF4 as nc4
import numpy as np
from datetime import datetime
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict


# obsIdDict is defined as obsid_dict in ocean_obs.py
obsIdDict = {
    5521: 'sea_water_salinity',  # Salinity
    3073: 'sea_water_temperature',  # Temperature
    5525: 'sea_surface_temperature',  # SST
    5526: 'obs_absolute_dynamic_topography',  # SSH (Not used ...)
    5351: 'obs_absolute_dynamic_topography',  # SSH
    6000: 'sea_ice_area_fraction',  # AICE
    6001: 'sea_ice_thickness'  # HICE
}


varDict = {
    'sea_water_salinity': 'sal',
    'sea_water_temperature': 'temp',
    'sea_surface_temperature': 'sst',
    'obs_absolute_dynamic_topography': 'adt',
    'sea_ice_area_fraction': 'frac',
    'sea_ice_thickness': 'thick'
}


def flipDict(dictIn):

    dictOut = {}

    for key, value in dictIn.items():
        if value not in dictOut:
            dictOut[value] = [key]
        else:
            dictOut[value].append(key)

    return dictOut


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


class refGMAOobs(object):

    def __init__(self, filename, date, data):

        self.filename = filename
        self.date = date
        self.data = data

        return


class IODA(object):

    def __init__(self, filename, date, varName, obsList):
        '''
        Initialize IODA writer class,
        transform to IODA data structure and,
        write out to IODA file.
        '''

        self.filename = filename
        self.date = date

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

        # Skip out if there are no obs!
        totalObs = 0
        for obs in obsList:
            if obs.data['nobs'] <= 0:
                continue
            totalObs += obs.data['nobs']
        if totalObs == 0:
            print('No %s observations for IODA!' % varName)
            return

        self.writer = iconv.NcWriter(self.filename, self.locKeyList)

        self.keyDict = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        self.keyDict[varName]['valKey'] = varName, self.writer.OvalName()
        self.keyDict[varName]['errKey'] = varName, self.writer.OerrName()
        self.keyDict[varName]['qcKey'] = varName, self.writer.OqcName()

        # data is the dictionary containing IODA friendly data structure
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        recKey = 0

        for obs in obsList:

            if obs.data['nobs'] <= 0:
                continue

            for n in range(obs.data['nobs']):

                oval = obs.data['value'][n]
                oerr = obs.data['oerr'][n]

                if discardOb(varName, oval):
                    continue

                lat = obs.data['lat'][n]
                lon = obs.data['lon'][n]
                lvl = obs.data['depth'][n]

                locKey = lat, lon, lvl, obs.date.strftime("%Y-%m-%dT%H:%M:%SZ")

                valKey = self.keyDict[varName]['valKey']
                errKey = self.keyDict[varName]['errKey']
                qcKey = self.keyDict[varName]['qcKey']

                self.data[recKey][locKey][valKey] = oval
                self.data[recKey][locKey][errKey] = oerr
                self.data[recKey][locKey][qcKey] = 0

        (ObsVars, LocMdata, VarMdata) = self.writer.ExtractObsData(self.data)
        self.writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, self.AttrData)

        return


def separateObs(obsList):

    obsDict = {}
    for key in obsIdDict.keys():

        obsListKey = []

        for obs in obsList:

            date = obs.date
            filename = obs.filename

            ind = np.where(obs.data['typ'] == key)

            data = {}
            data['nobs'] = len(ind[0])
            data['typ'] = obs.data['typ'][ind]
            data['lon'] = obs.data['lon'][ind]
            data['lat'] = obs.data['lat'][ind]
            data['depth'] = obs.data['depth'][ind]
            data['value'] = obs.data['value'][ind]
            data['oerr'] = obs.data['oerr'][ind]

            obsListKey.append(refGMAOobs(filename, date, data))

        obsDict[key] = obsListKey

    return obsDict


def sortDict(obsDictIn):

    # Flip the obsIdDict
    obsIdDictFlipped = flipDict(obsIdDict)

    obsDictOut = {}

    # Loop over flipped obsIdDict
    for key, values in obsIdDictFlipped.items():

        obsList = []
        for value in values:
            obsList.append(obsDictIn[value])

        # Flatten the newly created list of lists
        obsDictOut[key] = [item for sublist in obsList for item in sublist]

    return obsDictOut


def discardOb(varName, obsValue):

    discardOb = True

    if varName in ["sea_water_salinity"]:
        if 0. <= obsValue <= 50.:
            discardOb = False
    elif varName in ["sea_water_temperature", "sea_surface_temperature"]:
        if -2. <= obsValue <= 100.:
            discardOb = False
    elif varName in ["obs_absolute_dynamic_topography"]:
        if -5. <= obsValue <= 5.:
            discardOb = False
    elif varName in ["sea_ice_area_fraction"]:
        if 0. <= obsValue <= 1.:
            discardOb = False
    elif varName in ["sea_ice_thickness"]:
        discardOb = False
    else:
        raise SystemExit("Unknown observation variable %s" % varName)

    return discardOb


def main():

    parser = ArgumentParser(
        description=__doc__,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the input GMAO ocean obs file(s)',
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output', help='template name of the output IODA file (one per type)',
        type=str, required=True)
    parser.add_argument(
        '-d', '--date', help='datetime at the middle of the window', metavar='YYYYMMDDHH',
        type=str, required=True)
    parser.add_argument(
        '--inputdates', help='dates of the input GMAO ocean obs file(s)',
        type=str, nargs='+', required=False, metavar='YYYYMMDDHH')

    args = parser.parse_args()

    fList = args.input
    dList = args.inputdates
    foutput = args.output
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    if dList:
        assert len(dList) == len(fList)
        dList = [datetime.strptime(d, '%Y%m%d%H') for d in dList]
    else:
        dList = [fdate] * len(fList)

    obsList = []
    for fname, idate in zip(fList, dList):
        obsList.append(GMAOobs(fname, idate))

    obsDict = separateObs(obsList)

    obsDictSorted = sortDict(obsDict)

    for key, value in varDict.items():
        fout = '%s_%s.nc' % (foutput, value)
        IODA(fout, fdate, key, obsDictSorted[key])


if __name__ == '__main__':
    main()
