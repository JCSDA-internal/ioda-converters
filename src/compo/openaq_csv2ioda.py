#!/usr/bin/env python3

import os
import datetime
import pandas as pd
import numpy as np
import netCDF4 as nc
import requests

import lib_python.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from lib_python.orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "string"),
]

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(1),
}

DimDict = {
}


class openaq(object):
    def __init__(self, filenames, varname, qa_flg, thin, obsVar):
        self.filenames = filenames
        self.varname = varname
        self.obsVar = obsVar
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        iodavar = self.obsVar[self.varname][0]
        units = self.obsVar[self.varname][0]
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = units
        self.varAttrs[iodavar, iconv.OerrName()]['units'] = units

#Index(['locationId', 'location', 'parameter', 'value', 'unit', 'country',
#       'city', 'isMobile', 'isAnalysis', 'entity', 'sensorType', 'date.utc',
#       'date.local', 'coordinates.latitude', 'coordinates.longitude'],
#      dtype='object')

        first = True
        for f in self.filenames:
            df = pd.read_csv(f,sep=',')

            times = pd.to_datetime(df['date.utc'].values).strftime('%Y-%m-%dT%X')
            lats = df['coordinates.latitude'].values
            lons = df['coordinates.longitude'].values
            data = df['value'].values
            err = np.zeros_like(data)
            qc_flag = np.ones_like(data)

            if first: 
                self.outdata[('dateTime', 'MetaData')] = times
                self.outdata[('latitude', 'MetaData')] = lats
                self.outdata[('longitude', 'MetaData')] = lons
                self.outdata[self.varDict[iodavar]['valKey']] = data
                self.outdata[self.varDict[iodavar]['errKey']] = err
                self.outdata[self.varDict[iodavar]['qcKey']] = qc_flag
            else:
                self.outdata[('dateTime', 'MetaData')] = np.concatenate((
                    self.outdata[('dateTime', 'MetaData')], times))
                self.outdata[('latitude', 'MetaData')] = np.concatenate((
                    self.outdata[('latitude', 'MetaData')], lats))
                self.outdata[('longitude', 'MetaData')] = np.concatenate((
                    self.outdata[('longitude', 'MetaData')], lons))
                self.outdata[self.varDict[iodavar]['valKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['valKey']], data))
                self.outdata[self.varDict[iodavar]['errKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['errKey']], err))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['qcKey']], qc_flag))

            DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])
            AttrData['Location'] = np.int32(DimDict['Location'])

def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Fetching OpenAQ data through API'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be produced.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of OpenAQ csv input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)
    required.add_argument(
        '-v', '--variable',
        help="name of varibale, available list: [o3, pm25]",
        type=str, required=True)
    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-n', '--thin',
        help="percentage of random thinning from 0.0 to 1.0. Zero indicates"
        " no thinning is performed. (default: %(default)s)",
        type=float, default=0.0)

    args = parser.parse_args()

    if args.variable == "pm25":
        obsVar = {'pm25': ['particulatematter2p5Surface', 'mg m-3']}
    elif args.variable == "o3":
        obsVar = {'o3': ['ozoneSurface', 'ppmV']}
    varDims = { obsVar[args.variable][0]: ['Location'] }

    # Read in the openaq data
    var = openaq(args.input, args.variable, args.qa_value, args.thin, obsVar)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(var.outdata, varDims, var.varAttrs, AttrData)


if __name__ == '__main__':
    main()
