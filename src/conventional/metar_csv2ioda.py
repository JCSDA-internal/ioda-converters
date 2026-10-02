#!/usr/bin/env python3

#
# (C) Copyright 2020-2025 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# author: Greg Thompson gthompsn AT ucar DOT edu
# This script will work with decoded, CSV files created by gthompsn stored at NCAR
#   data-access.ucar.edu:/glade/campaign/ral/aap/gthompsn/METARs/2019/20191231/2019123118_metars.csv.gz
# modified for ISU ASOS-AWOS-METAR files by Ashley Griffin
#

import os
import numpy as np
from datetime import datetime
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import csv
import netCDF4
import logging

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
import pyiodaconv.meteo_utils as meteo_utils

os.environ["TZ"] = "UTC"

locationKeyList = [("stationICAO", "string", ""),
                   ("latitude", "float", "degrees_north"),
                   ("longitude", "float", "degrees_east"),
                   ("stationElevation", "float", "m"),
                   ("height", "float", "m"),
                   ("dateTime", "long", "seconds since 1970-01-01T00:00:00Z")]
meta_keys = [m_item[0] for m_item in locationKeyList]

obsvars = ['airTemperatureAt2M',
           'specificHumidity',
           'stationPressure',
           'windEastward',
           'windNorthward']
obsvars_units = ['K', 'kg kg-1', 'Pa', 'm s-1', 'm s-1']
obserrlist = [1.2, 0.75E-3, 120.0, 1.7, 1.7]

VarDims = {'airTemperatureAt2M': ['Location'],
           'specificHumidity': ['Location'],
           'stationPressure': ['Location'],
           'windEastward': ['Location'],
           'windNorthward': ['Location']}

AttrData = {'converter': os.path.basename(__file__),
            'ioda_object_version': 2,
            'description': 'METAR surface observation data converted from CSV',
            'source': 'NCAR-RAL METAR database (gthompsn)'}

DimDict = {}

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

float_missing_value = netCDF4.default_fillvals['f4']
int_missing_value = netCDF4.default_fillvals['i4']
double_missing_value = netCDF4.default_fillvals['f8']
long_missing_value = netCDF4.default_fillvals['i8']
string_missing_value = '_'

iso8601_string = locationKeyList[meta_keys.index('dateTime')][2]
epoch = datetime.fromisoformat(iso8601_string[14:-1])

missing_vals = {'string': string_missing_value,
                'integer': int_missing_value,
                'long': long_missing_value,
                'float': float_missing_value,
                'double': double_missing_value}
dtypes = {'string': object,
          'integer': np.int32,
          'long': np.int64,
          'float': np.float32,
          'double': np.float64}


class reformatMetar(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date
        self.meteo_utils = meteo_utils.meteo_utils()
        self.varDict = defaultdict(lambda: DefaultOrderedDict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        AttrData['datetimeReference'] = date.strftime("%Y-%m-%dT%H:%M:%SZ_PT1H")

        # Read in CSV-formatted file of METAR data
        self._rd_metars()

        return

    def _rd_metars(self):

        # Set units of the MetaData variables and all _FillValues.
        for key in meta_keys:
            dtypestr = locationKeyList[meta_keys.index(key)][1]
            self.varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]
            this_units = locationKeyList[meta_keys.index(key)][2]
            if this_units:
                self.varAttrs[(key, metaDataName)]['units'] = this_units

        # Set coordinates and units of the ObsValues.
        for n, iodavar in enumerate(obsvars):
            self.varDict[iodavar]['valKey'] = iodavar, obsValName
            self.varDict[iodavar]['errKey'] = iodavar, obsErrName
            self.varDict[iodavar]['qcKey'] = iodavar, qcName
            self.varAttrs[iodavar, obsValName]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, obsErrName]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, qcName]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, obsValName]['units'] = obsvars_units[n]
            self.varAttrs[iodavar, obsErrName]['units'] = obsvars_units[n]

        # data is the dictionary of incoming observation (METAR) data
        data = {}
        for key in meta_keys:
            data[key] = []
        for key in obsvars:
            data[key] = []

        '''
        Read in the METARs data
        Header contains: station,valid,lon,lat,tmpf,dwpf,relh,drct,sknt,p01i,alti,mslp,vsby,gust,skyc1,
        skyc2,skyc3,skyc4,skyl1,skyl2,skyl3,skyl4,wxcodes,ice_accretion_1hr,ice_accretion_3hr,
        ice_accretion_6hr,peak_wind_gust,peak_wind_drct,peak_wind_time,feel,metar,snowdepth
        '''

        # open file in read mode
        with open(self.filename, 'r') as fh:
            missing = float_missing_value
            # pass the file object to reader() to get the reader object
            csv_dict_reader = csv.DictReader(fh)
            column_names = csv_dict_reader.fieldnames
            logging.info("Header, columns = " + ", ".join(column_names))
            # Iterate over each row in the csv using reader object
            for row in csv_dict_reader:
                # row variable is a list that represents a row in csv

                if row['station'] == '':
                    continue
                else:
                    icao = str(row['station'])
                try:
                    valid_time = str(row['valid'])
                    dt = datetime.strptime(valid_time, "%Y-%m-%d %H:%M")
                    utime = dt.timestamp()
                    lat = float(row['lat'])
                    lon = float(row['lon'])
                    elev = float(row['elevation'])
                    if (elev < -999 or elev > 8450):
                        elev = missing
                        hght = missing
                    else:         # Height of observation assumed 2 meters above station elevation
                        hght = elev + 2.0
                except (csv.Error, ValueError):
                    continue
                try:
                    temp = (float(row['tmpf']) - 32) * 5 / 9 + self.meteo_utils.C_2_K
                except (csv.Error, ValueError):
                    temp = missing
                try:
                    dewp = (float(row['dwpf']) - 32) * 5 / 9 + self.meteo_utils.C_2_K
                except (csv.Error, ValueError):
                    dewp = missing
                try:
                    wdir = float(row['drct'])
                except (csv.Error, ValueError):
                    wdir = missing
                try:
                    wspd = float(row['sknt']) * self.meteo_utils.KTS_2_MS
                except (csv.Error, ValueError):
                    wspd = missing

                if ((wdir != missing) and (wspd != missing)):
                    if (wdir == 0 and wspd == 0):
                        uwnd = 0.0
                        vwnd = 0.0
                    elif (wdir > 0 and wdir <= 360 and wspd > 0):
                        uwnd, vwnd = self.meteo_utils.dir_speed_2_uv(wdir, wspd)
                    else:
                        uwnd = missing
                        vwnd = missing
                else:
                    uwnd = missing
                    vwnd = missing

                try:
                    altim = float(row['alti'])
                    if altim <= 0:
                        raise ValueError("Invalid altimeter setting")
                    psfc = self.meteo_utils.altim_2_sfcPressure(altim, elev)
                except (csv.Error, ValueError):
                    altim = missing
                    psfc = missing

                if ((psfc != missing) and (temp != missing) and (dewp != missing)):
                    spfh = self.meteo_utils.specific_humidity(dewp, psfc)
                else:
                    spfh = missing

                data['stationICAO'].append(icao)
                data['dateTime'].append(utime)
                data['latitude'].append(lat)
                data['longitude'].append(lon)
                data['stationElevation'].append(elev)
                data['height'].append(hght)
                data['stationPressure'].append(psfc)
                data['airTemperatureAt2M'].append(temp)
                data['specificHumidity'].append(spfh)
                data['windEastward'].append(uwnd)
                data['windNorthward'].append(vwnd)

        fh.close()

        nlocs = len(data['dateTime'])
        DimDict['Location'] = nlocs

        # Set units of the MetaData variables and all _FillValues.
        for key in meta_keys:
            dtypestr = locationKeyList[meta_keys.index(key)][1]
            self.varAttrs[(key, metaDataName)]['_FillValue'] = missing_vals[dtypestr]
            this_units = locationKeyList[meta_keys.index(key)][2]
            if this_units:
                self.varAttrs[(key, metaDataName)]['units'] = this_units
            self.outdata[(key, metaDataName)] = np.array(data[key], dtype=dtypes[dtypestr])

        # Transfer from the 1-D data vectors and ensure output data (obs_data) types using numpy.
        # The value of 2 for the preQC is NCEP-EMC prepBUFR code table 7 meaning not-checked QC.
        # per source: https://www.emc.ncep.noaa.gov/mmb/data_processing/prepbufr.doc/table_7.htm
        for n, iodavar in enumerate(obsvars):
            self.outdata[(iodavar, obsValName)] = np.array(data[iodavar], dtype=np.float32)
            self.outdata[(iodavar, obsErrName)] = np.full(nlocs, obserrlist[n], dtype=np.float32)
            self.outdata[(iodavar, qcName)] = np.full(nlocs, 2, dtype=np.int32)

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
