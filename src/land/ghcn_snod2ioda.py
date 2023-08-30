#!/usr/bin/env python3
#
# (C) Copyright 2021-2022 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
#

import os
import argparse
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from dateutil.parser import parse

import pyiodaconv.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import record_time

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("stationElevation", "float"),
    ("dateTime", "string")
]

AttrData = {
}

DimDict = {
}

VarDims = {
    'totalSnowDepth': ['Location'],
}


class ghcn(object):

    def __init__(self, filename, fixfile, date, mask):
        self.filename = filename
        self.fixfile = fixfile
        self.date = date
        self.mask = mask
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.metaDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self._read()

    def _read(self):

        # set up variable names for IODA
        iodavar = 'totalSnowDepth'
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'm'
        self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'm'

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
        df20all = None
        df30_list.append(df20)
        df20 = None

        df30 = pd.concat(df30_list, ignore_index=True)
        df30 = df30[df30["ELEMENT"] == "SNWD"]
        df30["DATETIME"] = df30.apply(lambda row: parse(str(row["DATETIME"])).date(), axis=1)
        # select data with Start date
        startdate = self.date
        valid_date = datetime.strptime(startdate, "%Y%m%d%H")
        select_hour = int(valid_date.strftime('%H'))
        if select_hour < 12:
            valid_date -= timedelta(days=1)
        select_date = valid_date.strftime('%Y%m%d')
        new_date = parse(select_date).date()
        df30 = df30[df30["DATETIME"] == new_date]
        # Read station files
        cols = ["ID", "LATITUDE", "LONGITUDE", "ELEVATION", "STATE", "NAME", "GSN_FLAG", "HCNCRN_FLAG", "WMO_ID"]
        df10all = pd.read_csv(self.fixfile, header=None, sep='\r\n')
        df10all = df10all[0].str.split('\\s+', expand=True)
        df10 = df10all.iloc[:, 0:4]
        df10all = None
        sub_cols = {0: "ID", 1: "LATITUDE", 2: "LONGITUDE", 3: "ELEVATION"}
        df10 = df10.rename(columns=sub_cols)
        df10 = df10.drop_duplicates(subset=["ID"])

        # use stations list as the number of obs points
        # if no data for a station and a given date, leave -999.0
        num_obs = len(df10.index)

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
        df30Temp = None

        vals = df100["DATA_VALUE"].values
        vals = vals.astype('float32')
        lats = lats.astype('float32')
        lons = lons.astype('float32')
        alts = alts.astype('float32')
        qflg = 0*vals.astype('int32')
        errs = 0.0*vals
        sites = np.empty_like(vals, dtype=object)
        times = np.empty_like(vals, dtype=object)
        sites = id_array

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
            sites = sites[mask]
            times = times[mask]

        # get datetime from input
        my_date = datetime.strptime(startdate, "%Y%m%d%H")
        base_datetime = my_date.strftime('%Y-%m-%dT%H:00:00Z')

        # vals[vals >= 0.0] *= 0.001      # mm to meters
        errs[:] = 0.04
        times[:] = base_datetime
        # add metadata variables
        self.outdata[('dateTime', 'MetaData')] = times
        self.outdata[('stationIdentification', 'MetaData')] = sites
        self.outdata[('latitude', 'MetaData')] = lats
        self.outdata[('longitude', 'MetaData')] = lons
        self.outdata[('stationElevation', 'MetaData')] = alts
        self.varAttrs[('stationElevation', 'MetaData')]['units'] = 'm'

        self.outdata[self.varDict[iodavar]['valKey']] = vals
        self.outdata[self.varDict[iodavar]['errKey']] = errs
        self.outdata[self.varDict[iodavar]['qcKey']] = qflg

        DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])


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
                        help="base date (YYYYMMDDHH)", type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-m', '--mask',
        help="maskout missing values: maskout/default, default=none",
        type=str, required=True)

    args = parser.parse_args()

    # start timer
    tic = record_time()

    # Read in the GHCN snow depth data
    snod = ghcn(args.input, args.fixfile, args.date, args.mask)

    # report time
    toc = record_time(tic=tic)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write all data out
    writer.BuildIoda(snod.outdata, VarDims, snod.varAttrs, AttrData)

    # report time
    toc = record_time(tic=tic)


if __name__ == '__main__':
    main()
