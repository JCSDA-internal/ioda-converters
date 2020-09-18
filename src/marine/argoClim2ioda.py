#!/usr/bin/env python

#
# (C) Copyright 2019-2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

from __future__ import print_function
import sys
import re
import bisect
from datetime import datetime
import netCDF4 as nc4
import numpy as np
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict


class argoClim(object):

    def __init__(self, filename, begindate=None, enddate=None):

        self.filename = filename
        self.begindate = begindate
        self.enddate = enddate

        # Read data from file
        self._readData()

        return

    def _readData(self):
        '''
        Read and store the data.
        This file can be downloaded from:
        http://www.argo.ucsd.edu/Gridded_fields.html
        Global gridded NetCDF Argo only dataset produced by optimal interpolation
        doi:10.1002/2017GL073426
        '''

        try:
            nc = nc4.Dataset(self.filename, 'r')
        except IOError:
            raise IOError('%s file not found!' % self.filename)
        except Exception:
            raise Exception('Unknown error opening %s' % self.filename)

        # First find the variable in this file
        res = [x for x in list(nc.variables.keys()) if re.search("^ARGO_.*MEAN$", x)]

        if res:
            self.varname = res[0].split('_')[1]
            print("%s contains the variable: %s" % (self.filename, self.varname))
        else:
            raise NameError("No useable variable was found in the file %s" % self.filename)

        assert self.varname in ['TEMPERATURE', 'SALINITY'],\
            "%s is not a valid variable name" % self.varname

        lon = nc.variables['LONGITUDE'][:]
        lat = nc.variables['LATITUDE'][:]
        pres = nc.variables['PRESSURE'][:]

        # Get absolute time instead of time since epoch
        dtime = nc.variables['TIME']
        time360day = nc4.num2date(dtime[:], dtime.units, calendar='360_day')

        # Convert Datetime360Day calendar to a regular datetime object
        timeArray = np.array([datetime.strptime(x.strftime('%Y%m%d%H'), '%Y%m%d%H') for x in time360day])

        bI = 0
        if self.begindate is not None and timeArray[0] <= self.begindate <= timeArray[-1]:
            bI = bisect.bisect_left(timeArray, self.begindate)

        eI = -1
        if self.enddate is not None and timeArray[0] <= self.enddate <= timeArray[-1]:
            eI = bisect.bisect_left(timeArray, self.enddate) + 1

        if bI == 0 and eI == -1:  # all times
            anomaly = nc.variables['ARGO_%s_ANOMALY' % self.varname][:]
            time = timeArray[:]
        elif bI == 0 and eI > -1:  # upto a given time
            anomaly = nc.variables['ARGO_%s_ANOMALY' % self.varname][:eI, :]
            time = timeArray[:eI]
        elif bI > 0 and eI == -1:  # from a given time
            anomaly = nc.variables['ARGO_%s_ANOMALY' % self.varname][bI:, :]
            time = timeArray[bI:]
        elif bI > 0 and eI > -1:  # from and till given times
            anomaly = nc.variables['ARGO_%s_ANOMALY' % self.varname][bI:eI, :]
            time = timeArray[bI:eI]

        mean = nc.variables['ARGO_%s_MEAN' % self.varname][:]

        # create a full field from mean and anomaly
        fullField = anomaly + np.tile(mean, (anomaly.shape[0], 1, 1, 1))

        try:
            nc.close()
        except IOError:
            raise IOError('%s file could not be closed!' % self.filename)
        except Exception:
            raise Exception('Unknown error closing %s' % self.filename)

        # self.data is the data structure
        self.data = {}
        self.data['lat'] = lat
        self.data['lon'] = lon
        self.data['pres'] = pres
        self.data['time'] = time
        self.data['field'] = fullField.data

        return


class IODA(object):

    def __init__(self, filename, date, argo):
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
            ("pressure", "float"),
            ("datetime", "string")
        ]

        self.AttrData = {
            'odb_version': 1,
            'date_time_string': self.date.strftime("%Y-%m-%dT%H:%M:%SZ")
        }

        self.writer = iconv.NcWriter(self.filename, self.locKeyList)

        # data is the dictionary containing IODA friendly data structure
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        recKey = 0
        valKey = argo.varname, self.writer.OvalName()
        errKey = argo.varname, self.writer.OerrName()
        qcKey = argo.varname, self.writer.OqcName()

        # There has to be a better way than explicitly looping over 4! dimensions
        for t, time in enumerate(argo.data['time']):
            for y, lat in enumerate(argo.data['lat']):
                for x, lon in enumerate(argo.data['lon']):
                    for z, pres in enumerate(argo.data['pres']):

                        locKey = lat, lon, pres, time.strftime('%Y-%m-%dT%H:%M:%SZ')

                        val = argo.data['field'][t, z, y, x]
                        err = 0.
                        qc = 1

                        self.data[recKey][locKey][valKey] = val
                        self.data[recKey][locKey][errKey] = err
                        self.data[recKey][locKey][qcKey] = qc

                    recKey += 1

        (ObsVars, LocMdata, VarMdata) = self.writer.ExtractObsData(self.data)
        self.writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, self.AttrData)

        return


def main():

    desc = 'Convert ARGO gridded global data to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the Global ARGO file',
        type=str, required=True)
    parser.add_argument(
        '-o', '--output', help='name of the output IODA netCDF ARGO file',
        type=str, required=True, default=None)
    parser.add_argument(
        '-d', '--date', help='file date', metavar='YYYYMMDDHH',
        type=str, required=True)
    parser.add_argument(
        '-b', '--begindate', help='end date for data time window', metavar='YYYYMMDDHH',
        type=str, required=False, default=None)
    parser.add_argument(
        '-e', '--enddate', help='end date for data time window', metavar='YYYYMMDDHH',
        type=str, required=False, default=None)

    args = parser.parse_args()

    filename = args.input
    foutput = args.output
    fdate = datetime.strptime(args.date, '%Y%m%d%H')
    bdate = None if args.begindate is None else datetime.strptime(args.begindate, '%Y%m%d%H')
    edate = None if args.enddate is None else datetime.strptime(args.enddate, '%Y%m%d%H')

    argo = argoClim(filename, begindate=bdate, enddate=edate)

    IODA(foutput, fdate, argo)


if __name__ == '__main__':
    main()
