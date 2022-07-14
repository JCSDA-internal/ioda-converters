#!/usr/bin/env python3

#
# (C) Copyright 2020, 2021 UCAR
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
import numpy as np
from datetime import datetime
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path
import csv
import netCDF4

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict
import meteo_utils

os.environ["TZ"] = "UTC"

locationKeyList = [
    ("station_id", "string"),
    ("latitude", "float"),
    ("longitude", "float"),
    ("station_elevation", "float"),
    ("height", "float"),
    ("dateTime", "integer"),
]

obsvars = {
    'ob_temp': 'air_temperature',
    'ob_spfh': 'specific_humidity',
    'ob_psfc': 'surface_pressure',
    'ob_uwnd': 'eastward_wind',
    'ob_vwnd': 'northward_wind',
}

obsvars_units = ['K', 'kg kg-1', 'Pa', 'm s-1', 'm s-1']

VarDims = {
    'ob_temp': ['nlocs'],
    'ob_spfh': ['nlocs'],
    'ob_psfc': ['nlocs'],
    'ob_uwnd': ['nlocs'],
    'ob_vwnd': ['nlocs'],
}

AttrData = {
    'converter': os.path.basename(__file__),
    'ioda_version': 2,
    'description': 'METAR surface observation data converted from CSV',
    'source': 'NCAR-RAL METAR database (gthompsn)',
}

DimDict = {
}


class reformatMetar(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date
        self.meteo_utils = meteo_utils.meteo_utils()
        self.float_fill = netCDF4.default_fillvals['f4']
        self.varDict = defaultdict(lambda: DefaultOrderedDict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        AttrData['datetime_reference'] = date.strftime("%Y-%m-%dT%H:%M:%SZ")

        # Read in CSV-formatted file of METAR data
        self._rd_metars()

        return

    def _rd_metars(self):

        n = 0
        for iodavar in obsvars.values():
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = obsvars_units[n]
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = obsvars_units[n]
            self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'
            n += 1

        # Set units of some MetaData variables
        self.varAttrs['station_elevation', 'MetaData']['units'] = 'm'
        self.varAttrs['height', 'MetaData']['units'] = 'm'
        self.varAttrs['dateTime', 'MetaData']['units'] = 'seconds since 1970-01-01T00:00:00Z'

        # data is the dictionary of incoming observation (METAR) data
        data = {}

        data['ob_icao'] = []
        data['ob_lat'] = []
        data['ob_lon'] = []
        data['ob_time'] = []
        data['ob_datetime'] = []
        data['ob_elev'] = []
        data['ob_hght'] = []
        data['ob_psfc'] = []
        data['ob_temp'] = []
        data['ob_spfh'] = []
        data['ob_uwnd'] = []
        data['ob_vwnd'] = []

        '''
        Read in the METARs data
        Header contains: Unix_time,DateString,ICAO,Latitude,Longitude,Elev,Temp,Dewp,Wdir,Wspd,Wgst,Vis,\
        Pcp,Pcp3h,Pcp6h,Pcp24h,QcFlag,WxString,WxCode,Altimeter,Cvg1,Bas1,Cvg2,Bas2,Cvg3,Bas3,Length,Raw
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

                if row['ICAO'] == '':
                    continue
                else:
                    icao = str(row['ICAO'])
                try:
                    utime = int(row['Unix_time'])
                    lat = float(row['Latitude'])
                    lon = float(row['Longitude'])
                    elev = float(row['Elev'])
                    if (elev < -999 or elev > 8450):
                        elev = self.float_fill
                        hght = self.float_fill
                    else:
                        hght = elev + 2.0           # Height of observation assumed 2 meters above station elevation
                except (csv.Error, ValueError):
                    continue
                try:
                    temp = float(row['Temp']) + self.meteo_utils.C_2_K
                except (csv.Error, ValueError):
                    temp = self.float_fill
                try:
                    dewp = float(row['Dewp']) + self.meteo_utils.C_2_K
                except (csv.Error, ValueError):
                    dewp = self.float_fill
                try:
                    wdir = float(row['Wdir'])
                except (csv.Error, ValueError):
                    wdir = self.float_fill
                try:
                    wspd = float(row['Wspd']) * self.meteo_utils.KTS_2_MS
                except (csv.Error, ValueError):
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
                except (csv.Error, ValueError):
                    altim = self.float_fill
                    psfc = self.float_fill

                if ((psfc is not self.float_fill) and (temp is not self.float_fill) and (dewp is not self.float_fill)):
                    spfh = self.meteo_utils.specific_humidity(dewp, psfc)
                else:
                    spfh = self.float_fill

                data['ob_icao'].append(icao)
                data['ob_datetime'].append(utime)
                data['ob_lat'].append(lat)
                data['ob_lon'].append(lon)
                data['ob_elev'].append(elev)
                data['ob_hght'].append(hght)
                data['ob_psfc'].append(psfc)
                data['ob_temp'].append(temp)
                data['ob_spfh'].append(spfh)
                data['ob_uwnd'].append(uwnd)
                data['ob_vwnd'].append(vwnd)

        fh.close()

        nlocs = len(data['ob_datetime'])

        self.outdata[('station_id', 'MetaData')] = np.array(data['ob_icao'], dtype=object)
        self.outdata[('dateTime', 'MetaData')] = np.array(data['ob_datetime'], dtype=np.int64)
        self.outdata[('latitude', 'MetaData')] = np.array(data['ob_lat'], dtype=np.float32)
        self.outdata[('longitude', 'MetaData')] = np.array(data['ob_lon'], dtype=np.float32)
        self.outdata[('station_elevation', 'MetaData')] = np.array(data['ob_elev'], dtype=np.float32)
        self.outdata[('height', 'MetaData')] = np.array(data['ob_hght'], dtype=np.float32)
        iodavar = 'surface_pressure'
        self.outdata[(iodavar, iconv.OvalName())] = np.array(data['ob_psfc'], dtype=np.float32)
        self.outdata[(iodavar, iconv.OerrName())] = np.full((nlocs), 200.0, dtype=np.float32)
        self.outdata[(iodavar, iconv.OqcName())] = np.full((nlocs), 2, dtype=np.int32)
        iodavar = 'air_temperature'
        self.outdata[(iodavar, iconv.OvalName())] = np.array(data['ob_temp'], dtype=np.float32)
        self.outdata[(iodavar, iconv.OerrName())] = np.full((nlocs), 0.2, dtype=np.float32)
        self.outdata[(iodavar, iconv.OqcName())] = np.full((nlocs), 2, dtype=np.int32)
        iodavar = 'specific_humidity'
        self.outdata[(iodavar, iconv.OvalName())] = np.array(data['ob_spfh'], dtype=np.float32)
        self.outdata[(iodavar, iconv.OerrName())] = np.full((nlocs), 0.75E-3, dtype=np.float32)
        self.outdata[(iodavar, iconv.OqcName())] = np.full((nlocs), 2, dtype=np.int32)
        iodavar = 'eastward_wind'
        self.outdata[(iodavar, iconv.OvalName())] = np.array(data['ob_uwnd'], dtype=np.float32)
        self.outdata[(iodavar, iconv.OerrName())] = np.full((nlocs), 0.7, dtype=np.float32)
        self.outdata[(iodavar, iconv.OqcName())] = np.full((nlocs), 2, dtype=np.int32)
        iodavar = 'northward_wind'
        self.outdata[(iodavar, iconv.OvalName())] = np.array(data['ob_vwnd'], dtype=np.float32)
        self.outdata[(iodavar, iconv.OerrName())] = np.full((nlocs), 0.7, dtype=np.float32)
        self.outdata[(iodavar, iconv.OqcName())] = np.full((nlocs), 2, dtype=np.int32)

        DimDict['nlocs'] = nlocs
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])

        return


def main():

    desc = 'Convert CSV-formatted METAR data to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the input METARs CSV-formatted file',
        type=str, required=True, default=None)
    parser.add_argument(
        '-o', '--output', help='name of the output netCDF IODA-ready file',
        type=str, required=True, default=None)
    parser.add_argument(
        '-d', '--date', help='file date', metavar='YYYYMMDDHH',
        type=str, required=True)

    args = parser.parse_args()

    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    obs = reformatMetar(args.input, fdate)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(obs.outdata, VarDims, obs.varAttrs, AttrData)


if __name__ == '__main__':
    main()
