#!/usr/bin/env python

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
#..Temporarily include the ioda_conv_ncio library path explicitly using this: export PYTHONPATH=../lib-python:$PYTHONPATH
#

from __future__ import print_function
import sys
import math
from datetime import datetime
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
from pathlib import Path
from csv import DictReader
import netCDF4

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import meteo_utils
import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict

class reformatMetar(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date
        self.meteo_utils = meteo_utils.meteo_utils()

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
            csv_dict_reader = DictReader(fh)
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
                except:
                    continue
                try:
                    lat = float(row['Latitude'])
                except:
                    continue
                try:
                    lon = float(row['Longitude'])
                except:
                    continue
                try:
                    elev = float(row['Elev'])
                except:
                    continue
                try:
                    temp = float(row['Temp']) + self.meteo_utils.C_2_K
                except:
                    temp = netCDF4.default_fillvals['f4']
                try:
                    dewp = float(row['Dewp']) + self.meteo_utils.C_2_K
                except:
                    dewp = netCDF4.default_fillvals['f4']
                try:
                    wdir = float(row['Wdir'])
                except:
                    wdir = netCDF4.default_fillvals['f4']
                try:
                    wspd = float(row['Wspd']) * self.meteo_utils.KTS_2_MS
                except:
                    wspd = netCDF4.default_fillvals['f4']

                if ( (wdir is not netCDF4.default_fillvals['f4']) and (wspd is not netCDF4.default_fillvals['f4']) ):
                    if (wdir==0 and wspd==0):
                        uwnd = 0.0
                        vwnd = 0.0
                    elif (wdir>0 and wdir<=360 and wspd>0):
                        uwnd,vwnd = self.meteo_utils.dir_speed_2_uv(wdir, wspd)
                    else:
                        uwnd = netCDF4.default_fillvals['f4']
                        vwnd = netCDF4.default_fillvals['f4']
                else:
                    uwnd = netCDF4.default_fillvals['f4']
                    vwnd = netCDF4.default_fillvals['f4']

                try:
                    altim = float(row['Altimeter'])
                    psfc = self.meteo_utils.altim_2_sfcPressure(altim, elev)
                except:
                    altim = netCDF4.default_fillvals['f4']
                    psfc = netCDF4.default_fillvals['f4']

                if ( (psfc is not netCDF4.default_fillvals['f4']) and (temp is not netCDF4.default_fillvals['f4']) and (dewp is not netCDF4.default_fillvals['f4']) ):
                    spfh = self.meteo_utils.specific_humidity(dewp, psfc)
                else:
                    spfh = netCDF4.default_fillvals['f4']

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

        # Testing some utility functions to ensure they work properly.
                '''
                if ( (temp is not netCDF4.default_fillvals['f4']) and (dewp is not netCDF4.default_fillvals['f4']) ):
                    tlcl = self.meteo_utils.t_lcl(temp, dewp)
                    print ("DEBUG, Temp, Dewp, T_at_LCL is: " + str(temp) + ", " + str(dewp) + ", " + str(tlcl))
                else:
                    tlcl = netCDF4.default_fillvals['f4']

                if ( (psfc is not netCDF4.default_fillvals['f4']) and (spfh is not netCDF4.default_fillvals['f4']) ):
                    td = self.meteo_utils.t_dew(psfc, spfh)
                    print ("DEBUG, re-derived dewpoint is: " + str(td))

                if ( (psfc is not netCDF4.default_fillvals['f4']) and (temp is not netCDF4.default_fillvals['f4']) and (spfh is not netCDF4.default_fillvals['f4']) and (tlcl is not netCDF4.default_fillvals['f4']) ):
                    the = self.meteo_utils.theta_e(psfc, temp, spfh, tlcl)
                    print ("DEBUG, Theta-E is: " + str(the))

                if ( (psfc is not netCDF4.default_fillvals['f4']) and (temp is not netCDF4.default_fillvals['f4']) and (tlcl is not netCDF4.default_fillvals['f4']) ):
                    Th = temp * (100000.0/psfc)**(self.meteo_utils.R/self.meteo_utils.Cp)
                    Plcl = 100000.0 * (tlcl/Th)**(self.meteo_utils.Cp/self.meteo_utils.R)
                    the_lcl = self.meteo_utils.theta_e(Plcl, tlcl, spfh, tlcl)
                    T500 = self.meteo_utils.compT_fr_The(the_lcl, 50000.0)
                    print ("DEBUG, Parel T(500mb) is: " + str(T500) + ", " + str(Plcl) + ", " + str(Th))
                '''

        fh.close()

        '''
        pvec = [83900, 82700, 81910, 80200, 79110, 78100, 76370, 73720, 70000, 66220, 61550, 59310, 58400, 57140, 55030, 52970, 50960, 50000, 49010, 48700, 47110, 45900, 45270, 40110, 40000, 39300, 38490, 36920, 36300, 35410, 35000, 33950, 32540, 31900, 30000, 29900, 29870, 28600, 26180, 25000, 22840, 20810, 20000, 18940, 17210, 16400, 15000, 14860, 14130, 14000, 13430, 12770, 12700, 12140, 11540, 11300, 10970, 10420, 10100, 10000]
        tdvec = [6.6, 6.0, 5.5, 4.4, 3.8, 3.2, 2.4, 1.3, -0.4, -2.6, -5.2, -6.5, -7.0, -7.4, -8.1, -8.7, -9.4, -9.7, -10.2, -10.3, -13.0, -15.0, -15.6, -20.5, -20.6, -21.1, -22.6, -25.6, -26.8, -34.5, -38.1, -43.4, -50.7, -53.9, -72.7, -72.9, -72.9, -71.8, -69.9, -68.9, -72.7, -76.3, -77.7, -79.0, -81.0, -82.0, -83.7, -83.9, -84.9, -85.1, -85.0, -84.9, -84.9, -85.6, -86.4, -86.7, -86.3, -85.7, -85.3, -85.5]
        wvec = [None] * len(pvec)

        n = 0
        while n < len(pvec):
            w = self.meteo_utils.r_sub_s(pvec[n]*1.0, tdvec[n]+273.15)
            print ("DEBUG, w is: " + str(w))
            wvec[n] = w
            n = n + 1

        pwat = self.meteo_utils.precipitable_water(pvec, wvec)
        print ("DEBUG, precip water is: " + str(pwat))

        z_test = self.meteo_utils.std_atmos(70000.0)
        print ("DEBUG, z_test is: " + str(z_test))
        p_test = self.meteo_utils.std_atmos_p(3000.0)
        print ("DEBUG, p_test is: " + str(p_test))
        x_test = self.meteo_utils.r_sub_s(90000.0, 275.15)
        print ("DEBUG, x_test is: " + str(x_test))
        y_test = self.meteo_utils.r_sub_i(90000.0, 270.15)
        print ("DEBUG, y_test is: " + str(y_test))
        '''

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

                print ("obs iterate: " + str(n) + ", " + icao + ", " + str(lat) + ", " + str(lon) + ", " + str(elev) + ", " + dtg)

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
