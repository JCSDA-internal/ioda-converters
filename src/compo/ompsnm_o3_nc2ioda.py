#!/usr/bin/env python3

#
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Standard Python library imports.
import os
import sys
import argparse
import glob
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
from collections import defaultdict, OrderedDict

from pyiodaconv.orddicts import DefaultOrderedDict
import pyiodaconv.ioda_conv_engines as iconv

# Dictionary of essential MetaData
locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

# Name to call the output ozone variable
varname_ozone = 'ozoneTotal'

# Dictionary of variable name found in input file and corresponding output name
obsvars = {
    'integrated_layer_ozone_in_air': varname_ozone,
}

# dictionary to map things we're putting into ioda and taking out of instrument native format
ioda2nc = {}
ioda2nc['latitude'] = 'GeolocationData/Latitude'
ioda2nc['longitude'] = 'GeolocationData/Longitude'
ioda2nc['dateTime'] = 'GeolocationData/Time'
ioda2nc['solarZenithAngle'] = 'GeolocationData/SolarZenithAngle'
ioda2nc['valKey'] = 'ScienceData/ColumnAmountO3'
ioda2nc['ground_pixel_quality'] = 'GeolocationData/GroundPixelQualityFlags'
ioda2nc['quality_flags'] = 'ScienceData/QualityFlags'
ioda2nc['algorithm_flags'] = 'ScienceData/AlgorithmFlags'
ioda2nc['measurement_quality_flags'] = 'ScienceData/MeasurementQualityFlags'
ioda2nc['instrument_quality_flags'] = 'GeolocationData/InstrumentQualityFlags'

AttrData = {
    'converter': os.path.basename(__file__),
    'platformCommonName': 'npp',
    'sensor': 'ompsnm',
}

DimDict = {
}

VarDims = {
    varname_ozone: ['Location'],
}


class ompsnm(object):
    def __init__(self, filenames, sTAI, eTAI):
        self.filenames = filenames
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.startTAI = sTAI
        self.endTAI = eTAI
        self._setVarDict(varname_ozone)

        vars2output = list(ioda2nc.keys())
        vars2output.append('sensorScanPosition')
        for v in vars2output:
            if('quality' in v or 'flags' in v):
                pass
            elif(v != 'valKey'):
                self.outdata[(v, 'MetaData')] = []
        self.outdata[self.varDict[varname_ozone]['valKey']] = []

        self._setVarDict(varname_ozone)
        self.outdata[self.varDict[varname_ozone]['valKey']] = []

        self._read()

    # set ioda variable keys
    def _setVarDict(self, iodavar):
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()

    # set variable attributes for IODA
    def _setVarAttr(self, iodavar):
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'

        varsToAddUnits = list(ioda2nc.keys())
        varsToAddUnits.append('sensorScanPosition')
        for v in varsToAddUnits:
            if(v != 'valKey'):
                vkey = (v, 'MetaData')
                if('pressure' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'Pa'
                elif(v == 'dateTime'):
                    self.varAttrs[vkey]['units'] = 'seconds since 1993-01-01T00:00:00Z'
                elif('angle' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'degrees'
                elif('prior' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'ppmv'
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'DU'

        vkey = ('pressure', 'MetaData')
        self.varAttrs[vkey]['units'] = 'Pa'

    # Read data needed from raw OMPSNM file.
    def _read_nc(self, filename):
        print("Reading: {}".format(filename))
        d = {}
        ncd = nc.Dataset(filename, 'r')
        # use dictionary above to just read fields we want out of the netcdf.
        for k in list(ioda2nc.keys()):
            d[k] = ncd[ioda2nc[k]][...]
        # unmask ground pixel quality to pass fill value.
        d['ground_pixel_quality'].mask = False
        # mesh time and scan_position to get flattened array instead of using loops
        time_vec = d['dateTime']
        scan_position_vec = np.arange(1, d['valKey'].shape[1]+1)
        d['sensorScanPosition'], d['dateTime'] = np.meshgrid(scan_position_vec, time_vec)
        d['sensorScanPosition'] = d['sensorScanPosition'].astype('float32')
        d['measurement_quality_flags'].mask = False
        d['instrument_quality_flags'].mask = False
        d['measurement_quality_flags'] = np.tile(d['measurement_quality_flags'], (scan_position_vec.shape[0], 1)).T
        d['instrument_quality_flags'] = np.tile(d['instrument_quality_flags'], (scan_position_vec.shape[0], 1)).T
        idx = np.where((~d['valKey'].mask) & (d['dateTime'] <= self.endTAI) & (d['dateTime'] >= self.startTAI))
        ncd.close()
        return d, idx

    def _read(self):
        # set up variable names for IODA
        for iodavar in [varname_ozone, ]:
            # self._setVarDict(var)
            self._setVarAttr(iodavar)
        # loop through input filenames
        for f in self.filenames:
            fileData, idx = self._read_nc(f)
            # add metadata variables
            for v in list(fileData.keys()):
                if('quality' in v or 'flags' in v):
                    pass
                elif(v != 'valKey' and v != 'ozone_Apriori' and v != 'layer_efficiency'):
                    #  add metadata variables
                    self.outdata[(v, 'MetaData')].extend(fileData[v][idx].flatten().tolist())
            for ncvar, iodavar in obsvars.items():
                self.outdata[self.varDict[iodavar]['valKey']].extend(fileData['valKey'][idx].flatten().tolist())

        # add dummy air_pressure so UFO will know this is a total column ob, and not partial.
        nlocs = len(self.outdata[('dateTime', 'MetaData')])
        self.outdata[('pressure', 'MetaData')] = np.zeros(nlocs).tolist()

        for k in self.outdata.keys():
            self.outdata[k] = np.asarray(self.outdata[k])
            if(self.outdata[k].dtype == 'float64'):
                self.outdata[k] = self.outdata[k].astype('float32')
            elif(self.outdata[k].dtype == 'int64' and k != ('dateTime', 'MetaData')):
                self.outdata[k] = self.outdata[k].astype('int32')
        DimDict['Location'] = nlocs
        self.outdata[('dateTime', 'MetaData')] = self.outdata[('dateTime', 'MetaData')].astype(np.int64)
        self.outdata[('longitude', 'MetaData')] = self.outdata[('longitude', 'MetaData')] % 360
# end ompsnm object.


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads OMPS-NM O3 HDF5 files provided by NASA (somewhere) '
            'and converts into IODA formatted output files. Multiple '
            'files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of OMPS-NM input file(s)",
        type=str, required=True)
    required.add_argument(
        '-y', '--year',
        help="syn. time year",
        type=int, required=True)
    required.add_argument(
        '-m', '--month',
        help="syn. time month",
        type=int, required=True)
    required.add_argument(
        '-d', '--day',
        help="syn. time day",
        type=int, required=True)
    required.add_argument(
        '-z', '--hour',
        help="syn. time hour.",
        type=int, required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-p', '--prefix',
        help="ompsnm filename prefix (default=OMPS-NPP_NMTO3-L2_v2.1)",
        type=str, required=False, default="OMPS-NPP_NMTO3-L2_v2.1", dest='prefix')
    optional.add_argument(
        '-w', '--window',
        help="assimilation window size in hours",
        type=int, required=False, default=6, dest='window')

    args = parser.parse_args()

    # Get Day of year for current cycle and associated file(s)
    cycle_time = datetime(args.year, args.month, args.day, args.hour)
    if(os.path.isfile(args.input)):
        print('Reading Single File:{}'.format(args.input))
        rawFiles = []
        rawFiles.append(args.input)
    elif(os.path.isdir(args.input)):
        startDateWindow = cycle_time - timedelta(hours=args.window/2)
        endDateWindow = cycle_time + timedelta(hours=args.window/2)
        # effectively round off so we get the number of days between
        startDayWindow = datetime(startDateWindow.year, startDateWindow.month, startDateWindow.day)
        endDayWindow = datetime(endDateWindow.year, endDateWindow.month, endDateWindow.day)
        dT = endDayWindow - startDayWindow
        daysToGo = [startDayWindow + timedelta(days=i) for i in range(dT.days + 1)]
        # iterate over the number of days in window
        rawFiles = []
        for now in daysToGo:
            year = now.year
            month = now.month
            day = now.day
            rawFiles.extend(glob.glob(os.path.join(args.input, args.prefix+"_{}m{}{}".format(year, month, day)+"*.h5")))
        rawFiles.sort()

        # only read files in the window.
        rawFilesOut = []
        for fi, f in enumerate(rawFiles):
            vv = f.split('_')
            # 2020m1216t011958.h5 2020m1215t222840
            startDateFile = datetime.strptime(vv[-3][0:-2], "%Ym%m%dt%H%M")
            endDateFile = startDateFile
            # Check the the start time for the next file to get the end time of current file.
            if(fi != len(rawFiles)-1):
                vv = rawFiles[fi+1].split('_')
                endDateFile = datetime.strptime(vv[-2][0:-7], "%Ym%m%dt%H%M")
            if(startDateWindow <= startDateFile <= endDateWindow or startDateWindow <= endDateFile <= endDateWindow):
                rawFilesOut.append(f)
        rawFiles = rawFilesOut
        if(len(rawFiles) == 0):
            print("No Raw Files Found in:{}".format(args.input))
            sys.exit(os.EX_OSFILE)
    else:
        print("Could not find input file or directory:{}".format(args.input))
        sys.exit(os.EX_OSFILE)
    # get start and end times for qc/cropping data in MLS native time format (TAI seconds since Jan 1, 1993.)
    startTAI = ((cycle_time - timedelta(hours=args.window/2)) - datetime(1993, 1, 1, 0)).total_seconds()
    endTAI = ((cycle_time + timedelta(hours=args.window/2)) - datetime(1993, 1, 1, 0)).total_seconds()

    # Read in the O3 data in window
    o3 = ompsnm(rawFiles, startTAI, endTAI)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    print("Writing: {}".format(args.output))
    writer.BuildIoda(o3.outdata, VarDims, o3.varAttrs, AttrData)


if __name__ == '__main__':
    main()
