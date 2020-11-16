#!/usr/bin/env python3
#
# (C) Copyright 2020 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# This script will work with decoded, CSV files created by NCEI at NOAA/GHCN
# data acess at ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/
# 

import sys
import os
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

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict

vName = {
    'A': "ghcn_snow_depth",
}

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

AttrData = {}

class ghcn(object):

    def __init__(self, filename, fixfile, date, mask, writer):
        self.filename = filename
        self.fixfile = fixfile
        self.date = date
        self.mask = mask
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.writer = writer
        self._read()

    def _read(self):

        def assignValue(colrowValue, df400):
            if colrowValue == '' or pd.isnull(colrowValue):
                outList = -999.0
            else:
                ml = df400.loc[df400['ID'] == colrowValue, "DATA_VALUE"]
            # check if the series is empty
            if not(ml.empty):
                outList = ml.iloc[0] # watersourceSer.append(ml.iloc[0])
            else:
               outList = -999.0
            return outList

        cols = ["ID", "DATETIME", "ELEMENT","DATA_VALUE", "M_FLAG", "Q_FLAG", "S_FLAG", "OBS_TIME"]
        sub_cols = ["ID", "DATETIME", "ELEMENT","DATA_VALUE"]
        df30_list = []
        # Fix dtypeWarning with mixed types via set low_memory=False 
        df20all = pd.read_csv(self.filename, header=None, names=cols, low_memory=False) 
        df20 = df20all[sub_cols]
        print(df20.head(5))
        df30_list.append(df20)

        df30 = pd.concat(df30_list, ignore_index=True)
        df30 = df30[df30["ELEMENT"]=="SNWD"]
        df30["DATETIME"] = df30.apply(lambda row: parse(str(row["DATETIME"])).date(), axis=1)
        # select data with Start date
        startdate = self.date 
        df30 = df30[df30["DATETIME"] == parse(startdate).date()]
        print(df30.head(5))
        # Read station files 
        cols = ["ID","LATITUDE","LONGITUDE","ELEVATION","STATE","NAME","GSN_FLAG","HCNCRN_FLAG","WMO_ID"]
        print(self.fixfile)
        df10all = pd.read_csv(self.fixfile, header=None, sep='\n')
        df10all = df10all[0].str.split('\s+', expand=True)
        df10 = df10all.iloc[:, 0:3]
        sub_cols = {0:"ID", 1: "LATITUDE", 2:"LONGITUDE"}
        df10 = df10.rename(columns = sub_cols)
        df10 = df10.drop_duplicates(subset=["ID"])
        
        # use stations list as the number of obs points
        # if no data for a station and a given date, leave -999.0
        num_obs = len(df10.index)
        new_date = parse(startdate).date()        

        # Initialzed data array
        vals = np.full((num_obs), -999.0) 
        lats = np.full((num_obs), -999.0)
        lons = np.full((num_obs), -999.0)
        site = np.full((num_obs), -9999, dtype=np.int)
        id_array = np.chararray((num_obs))
        id_array[:] = "UNKNOWN"

        lats = df10["LATITUDE"].values
        lons = df10["LONGITUDE"].values
        id_array = df10["ID"].values

        df100 = pd.DataFrame(data=id_array, columns=['ID'])
        df100.assign(DATA_VALUE=-999.0)
 
        df30Temp = df30.loc[df30["DATETIME"] == new_date]
        df100["DATA_VALUE"] = df100.apply(lambda row: assignValue(row['ID'], df30Temp), axis=1)

        vals = df100["DATA_VALUE"].values
        vals = vals.astype('float')
        errs = vals      
        
        # use maskout options 
        if self.mask == "maskout":

            with np.errstate(invalid='ignore'):  
                mask = vals >= 0.0
            vals = vals[mask]
            lons = lons[mask]
            lats = lats[mask]
            errs = errs[mask]
 
        valKey = vName['A'], self.writer.OvalName()
        errKey = vName['A'], self.writer.OerrName()
        qcKey = vName['A'], self.writer.OqcName()

        # get datetime from input
        my_date = datetime.strptime(startdate, "%Y%m%d")
        start_datetime = my_date.strftime('%Y-%m-%d')
        base_datetime = start_datetime + 'T12:00:00Z'

        for i in range(len(vals)):
            if vals[i] >= 0.0:
                errs[i] = 0.15*vals[i]

            locKey = lats[i], lons[i], base_datetime
            self.data[0][locKey][valKey] = vals[i]
            self.data[0][locKey][errKey] = errs[i]
            self.data[0][locKey][qcKey] = 0

            AttrData["observation_type"] = "Snow Depth"
            AttrData["network"] = "GCOS-GSN"
            AttrData["data_source"] = "GHCN-Daily"
            AttrData['date_time_string'] = base_datetime


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

    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the profiles
    snwd = ghcn(args.input, args.fixfile, args.date, args.mask, writer)

    (ObsVars, LocMdata, VarMdata) = writer.ExtractObsData(snwd.data)

    writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, AttrData)


if __name__ == '__main__':
    main()
