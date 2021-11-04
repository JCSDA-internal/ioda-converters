#!/usr/bin/env python3
#
# (C) Copyright 2021 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# This script will work with decoded, CSV files created by NCEI at NOAA/GHCN
# data acess at ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/
#

import time, os, sys
import argparse
import netCDF4 as nc
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from dateutil.parser import parse
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("altitude", "float"),
    ("datetime", "string")
]

obsvars = {
    'snow_depth': 'snowDepth',
}

AttrData = {
    'converter': os.path.basename(__file__),
}

DimDict = {
}

VarDims = {
    'snowDepth': ['nlocs'],
}


class ghcn(object):

    def __init__(self, filename, fixfile, date, mask):
        self.filename = filename
        self.fixfile = fixfile
        self.date = date
        self.mask = mask
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    def _read(self):

        # set up variable names for IODA
        for iodavar in ['snowDepth']:
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'm'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'm'
            self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'

        def assignValue(colrowValue, df400):
            if colrowValue == '' or pd.isnull(colrowValue):
                outList = -999.0
            else:
                ml = df400.loc[df400['ID'] == colrowValue, "DATA_VALUE"]
            # check if the series is empty
            if not(ml.empty):
                outList = ml.iloc[0]  # watersourceSer.append(ml.iloc[0])
            else:
                outList = -999.0
            return outList

        cols = ["ID", "DATETIME", "ELEMENT", "DATA_VALUE", "M_FLAG", "Q_FLAG", "S_FLAG", "OBS_TIME"]
        sub_cols = ["ID", "DATETIME", "ELEMENT", "DATA_VALUE"]
        df30_list = []
        # Fix dtypeWarning with mixed types via set low_memory=False
        df20all = pd.read_csv(self.filename, header=None, names=cols, low_memory=False)
        df20 = df20all[sub_cols]
        df30_list.append(df20)

        df30 = pd.concat(df30_list, ignore_index=True)
        df30 = df30[df30["ELEMENT"] == "SNWD"]
        df30["DATETIME"] = df30.apply(lambda row: parse(str(row["DATETIME"])).date(), axis=1)
        # select data with Start date
        startdate = self.date
        df30 = df30[df30["DATETIME"] == parse(startdate).date()]
        # Read station files
        cols = ["ID", "LATITUDE", "LONGITUDE", "ELEVATION", "STATE", "NAME", "GSN_FLAG", "HCNCRN_FLAG", "WMO_ID"]
        df10all = pd.read_csv(self.fixfile, header=None, sep='\n')
        df10all = df10all[0].str.split('\\s+', expand=True)
        df10 = df10all.iloc[:, 0:4]
        sub_cols = {0: "ID", 1: "LATITUDE", 2: "LONGITUDE", 3: "ELEVATION"}
        df10 = df10.rename(columns=sub_cols)
        df10 = df10.drop_duplicates(subset=["ID"])

        # use stations list as the number of obs points
        # if no data for a station and a given date, leave -999.0
        num_obs = len(df10.index)
        new_date = parse(startdate).date()

        # Initialzed data array
        vals = np.full((num_obs), -999.0)
        lats = np.full((num_obs), -999.0)
        lons = np.full((num_obs), -999.0)
        alts = np.full((num_obs), -999.0)
        site = np.full((num_obs), -9999, dtype=np.int)
        id_array = np.chararray((num_obs))
        id_array[:] = "UNKNOWN"

        lats = df10["LATITUDE"].values
        lons = df10["LONGITUDE"].values
        alts = df10["ELEVATION"].values
        id_array = df10["ID"].values

        df100 = pd.DataFrame(data=id_array, columns=['ID'])
        df100.assign(DATA_VALUE=-999.0)
        df30Temp = df30.loc[df30["DATETIME"] == new_date]
        df100["DATA_VALUE"] = df100.apply(lambda row: assignValue(row['ID'], df30Temp), axis=1)

        vals = df100["DATA_VALUE"].values
        vals = vals.astype('float32')
        lats = lats.astype('float32')
        lons = lons.astype('float32')
        alts = alts.astype('float32')
        qflg = 0*vals.astype('int32')
        errs = 0.0*vals
        times = np.empty_like(vals, dtype=object)

        # use maskout options
        if self.mask == "maskout":

            with np.errstate(invalid='ignore'):
                mask = vals >= 0.0
            vals = vals[mask]
            errs = errs[mask]
            qflg = qflg[mask]
            lons = lons[mask]
            lats = lats[mask]
            alts = alts[mask]
            times = times[mask]

        # get datetime from input
        my_date = datetime.strptime(startdate, "%Y%m%d")
        start_datetime = my_date.strftime('%Y-%m-%d')
        base_datetime = start_datetime + 'T12:00:00Z'
        AttrData['date_time_string'] = base_datetime

        for i in range(len(vals)):
            if vals[i] >= 0.0:
                errs[i] = 0.0
                vals[i] = 0.001*vals[i]  # coverted to m from mm
            times[i] = base_datetime

        self.outdata[('datetime', 'MetaData')] = times
        self.outdata[('latitude', 'MetaData')] = lats
        self.outdata[('longitude', 'MetaData')] = lons
        self.outdata[('altitude', 'MetaData')] = alts

        for iodavar in ['snowDepth']:
            self.outdata[self.varDict[iodavar]['valKey']] = vals
            self.outdata[self.varDict[iodavar]['errKey']] = errs
            self.outdata[self.varDict[iodavar]['qcKey']] = qflg

        DimDict['nlocs'] = len(self.outdata[('datetime', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])


def main():

    parser = argparse.ArgumentParser(
        description=('Read GHCN snow depth file(s) and Converter'
                     ' of native csv format for observations of snow'
                     ' depth from GHCN to IODA netCDF format.')
    )
    parser.add_argument('-i', '--input',
                        help="name of ghcn snow depth input file(s)",
                        type=str, required=True)
    parser.add_argument('-f', '--fixfile',
                        help="name of ghcn station fixed file(s)",
                        type=str, required=True)
    parser.add_argument('-o', '--output',
                        help="name of ioda output file",
                        type=str, required=True)
    parser.add_argument('-d', '--date',
                        help="base date", type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-m', '--mask',
        help="maskout missing values: maskout/default, default=none",
        type=str, required=True)

    args = parser.parse_args()

    # Read in the snow depth data
    snod = ghcn(args.input, args.fixfile, args.date, args.mask)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    snod.varAttrs[('altitude', 'MetaData')]['units'] = 'm'
    # write everything out
    writer.BuildIoda(snod.outdata, VarDims, snod.varAttrs, AttrData)


if __name__ == '__main__':
    main()
