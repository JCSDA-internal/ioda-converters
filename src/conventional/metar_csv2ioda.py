#!/usr/bin/env python3

#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# author: Greg Thompson gthompsn AT ucar DOT edu
# This script will work with decoded, CSV files created by gthompsn stored at NCAR
#   data-access.ucar.edu:/glade/campaign/ral/aap/gthompsn/METARs/2019/20191231/2019123118_metars.csv.gz
#

import sys
import os
import math
from datetime import datetime
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path
import csv
import netCDF4

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import meteo_utils
import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"


class reformatMetar(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date
        self.meteo_utils = meteo_utils.meteo_utils()
        self.float_fill = netCDF4.default_fillvals['f4']

        # Read in CSV-formatted file of METAR data
        self._rd_metars()

        return

    def _rd_metars(self):

        # data is the dictionary of incoming observation (METAR) data
        data = {}

        data['ob_icao'] = []
        data['ob_lat'] = []
        data['ob_lon'] = []
        data['ob_time'] = []
        data['ob_datetime'] = []
        data['ob_elev'] = []
        data['ob_psfc'] = []
        data['ob_temp'] = []
        data['ob_spfh'] = []
        data['ob_uwnd'] = []
        data['ob_vwnd'] = []

        '''
        Read in the METARs data
        Header contains: ICAO, Unix_time, Latitude, Longitude, Elev, MSLP, Altimeter, Temp, Dewp, Wdir, Wspd
        '''

        # open file in read mode
        with open(self.filename, 'r') as fh:
            # pass the file object to reader() to get the reader object
            csv_dict_reader = csv.DictReader(fh)
            column_names = csv_dict_reader.fieldnames
            print(column_names)
            # Iterate over each row in the csv using reader object
            for row in csv_dict_reader:
                # row variable is a list that represents a row in csv

                if row['ICAO'] is '':
                    continue
                else:
                    icao = str(row['ICAO'])
                try:
                    utime = int(row['Unix_time'])
                    lat = float(row['Latitude'])
                    lon = float(row['Longitude'])
                    elev = float(row['Elev'])
                except csv.Error:
                    continue
                if row['Temp'] is '':
                    temp = self.float_fill
                else:
                    try:
                        temp = float(row['Temp']) + self.meteo_utils.C_2_K
                    except csv.Error:
                        temp = self.float_fill
                if row['Dewp'] is '':
                    dewp = self.float_fill
                else:
                    try:
                        dewp = float(row['Dewp']) + self.meteo_utils.C_2_K
                    except csv.Error:
                        dewp = self.float_fill
                if row['Wdir'] is '':
                    wdir = self.float_fill
                else:
                    try:
                        wdir = float(row['Wdir'])
                    except csv.Error:
                        wdir = self.float_fill
                if row['Wspd'] is '':
                    wspd = self.float_fill
                else:
                    try:
                        wspd = float(row['Wspd']) * self.meteo_utils.KTS_2_MS
                    except csv.Error:
                        wspd = self.float_fill

                if ((wdir is not self.float_fill) and (wspd is not self.float_fill)):
                    if (wdir == 0 and wspd == 0):
                        uwnd = 0.0
                        vwnd = 0.0
                    elif (wdir > 0 and wdir <= 360 and wspd > 0):
                        uwnd, vwnd = self.meteo_utils.dir_speed_2_uv(wdir, wspd)
                    else:
                        uwnd = self.float_fill
                        vwnd = self.float_fill
                else:
                    uwnd = self.float_fill
                    vwnd = self.float_fill

                try:
                    altim = float(row['Altimeter'])
                    psfc = self.meteo_utils.altim_2_sfcPressure(altim, elev)
                except csv.Error:
                    altim = self.float_fill
                    psfc = self.float_fill

                if ((psfc is not self.float_fill) and (temp is not self.float_fill) and (dewp is not self.float_fill)):
                    spfh = self.meteo_utils.specific_humidity(dewp, psfc)
                else:
                    spfh = self.float_fill

                data['ob_icao'].append(icao)
                data['ob_time'].append(utime)
                data['ob_datetime'].append(datetime.fromtimestamp(utime).strftime("%Y-%m-%dT%H:%M:%SZ"))
                data['ob_lat'].append(lat)
                data['ob_lon'].append(lon)
                data['ob_elev'].append(elev)
                data['ob_psfc'].append(psfc)
                data['ob_temp'].append(temp)
                data['ob_spfh'].append(spfh)
                data['ob_uwnd'].append(uwnd)
                data['ob_vwnd'].append(vwnd)

        fh.close()
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
            ("station_id", "string"),
            ("latitude", "float"),
            ("longitude", "float"),
            ("station_elevation", "float"),
            ("datetime", "string")
        ]

        self.AttrData = {
            'odb_version': 1,
            'date_time_string': self.date.strftime("%Y-%m-%dT%H:%M:%SZ")
        }

        self.writer = iconv.NcWriter(self.filename, self.locKeyList)

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

            for n in range(len(obs.data['ob_lat'])):

                icao = obs.data['ob_icao'][n]
                lat = obs.data['ob_lat'][n]
                lon = obs.data['ob_lon'][n]
                elev = obs.data['ob_elev'][n]
                dtg = obs.data['ob_datetime'][n]
                locKey = icao, lat, lon, elev, dtg

                # print ("obs iterate: " + str(n) + ", " + icao + ", " + str(lat) + ", " + str(lon) + ", " + str(elev) + ", " + dtg)

                for key in self.varDict.keys():

                    val = obs.data[key][n]
                    err = 0.0
                    qc = 2

                    valKey = self.keyDict[key]['valKey']
                    errKey = self.keyDict[key]['errKey']
                    qcKey = self.keyDict[key]['qcKey']

                    self.data[recKey][locKey][valKey] = val
                    self.data[recKey][locKey][errKey] = err
                    self.data[recKey][locKey][qcKey] = qc

                recKey += 1

        (ObsVars, LocMdata, VarMdata) = self.writer.ExtractObsData(self.data)
        self.writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, self.AttrData)

        return


def main():

    desc = 'Convert CSV-formatted METAR data to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the input METARs CSV-formatted file',
        type=str, nargs='+', required=True)
    parser.add_argument(
        '-o', '--output', help='name of the output netCDF IODA-ready file',
        type=str, required=True, default=None)
    parser.add_argument(
        '-d', '--date', help='file date', metavar='YYYYMMDDHH',
        type=str, required=True)

    args = parser.parse_args()

    fList = args.input
    foutput = args.output
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    obsList = []
    for fname in fList:
        obs = reformatMetar(fname, fdate)
        obsList.append(obs)

    varDict = {
        'ob_temp': 'air_temperature',
        'ob_spfh': 'specific_humidity',
        'ob_psfc': 'surface_pressure',
        'ob_uwnd': 'eastward_wind',
        'ob_vwnd': 'northward_wind'
    }

    IODA(foutput, fdate, varDict, obsList)


if __name__ == '__main__':
    main()
