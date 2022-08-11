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
from pathlib import Path
from collections import defaultdict, OrderedDict

# Append pyioda paths so ioda_conv_engines can be loaded
IODA_CONV_PATH = Path(__file__).parent/"../lib/pyiodaconv"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
from orddicts import DefaultOrderedDict
import ioda_conv_engines as iconv


def is_bit_set(integer_value, bit_position):
    return (integer_value & (1 << bit_position)) != 0


# Global Dictionaries.

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "integer"),
]

ioda2nc = {}
ioda2nc['latitude'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Geolocation Fields/Latitude'
ioda2nc['longitude'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Geolocation Fields/Longitude'
ioda2nc['dateTime'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Geolocation Fields/Time'
ioda2nc['solar_zenith_angle'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Geolocation Fields/SolarZenithAngle'
ioda2nc['prior_o3'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Data Fields/APrioriLayerO3'
# ioda2nc['Layer_Efficiency'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Data Fields/LayerEfficiency'
ioda2nc['valKey'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Data Fields/ColumnAmountO3'
ioda2nc['quality_flag'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Data Fields/QualityFlags'
ioda2nc['algorithm_flag'] = 'HDFEOS/SWATHS/OMI Column Amount O3/Data Fields/AlgorithmFlags'


obsvars = {
    'integrated_layer_ozone_in_air': 'integrated_layer_ozone_in_air',
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
    'satellite': 'aura',
    'sensor': 'omi',
}

DimDict = {
}

VarDims = {
    'integrated_layer_ozone_in_air': ['nlocs'],
}


class omi(object):
    def __init__(self, filenames, sTAI, eTAI, qcOn):
        self.filenames = filenames
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.startTAI = sTAI
        self.endTAI = eTAI
        self.qcOn = qcOn
        self._setVarDict('integrated_layer_ozone_in_air')

        # initialize dictionary with  empty list for MetaData variable to populate later.
        vars2output = list(ioda2nc.keys())
        vars2output.append('scan_position')
        for v in vars2output:
            if(v != 'valKey'):
                self.outdata[(v, 'MetaData')] = []
        self.outdata[self.varDict['integrated_layer_ozone_in_air']['valKey']] = []
        self._read()

    # set ioda variable keys
    def _setVarDict(self, iodavar):
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()

    # set variable attributes for IODA
    def _setVarAttr(self, iodavar):
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        missing_value = 9.96921e+36
        int_missing_value = -2147483647
        self.varAttrs[iodavar, iconv.OvalName()]['_FillValue'] = missing_value
        varsToAddUnits = list(ioda2nc.keys())
        varsToAddUnits.append('scan_position')
        for v in varsToAddUnits:
            if(v != 'valKey'):
                vkey = (v, 'MetaData')
                if('pressure' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'Pa'
                elif(v == 'dateTime'):
                    self.varAttrs[vkey]['units'] = 'seconds since 1993-01-01T00:00:00Z'
                elif('angle' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'degrees'
                elif('flag' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'unitless'
                elif('prior' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'ppmv'
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'DU'

    def _read_nc(self, filename):
        print("Reading: {}".format(filename))
        ncd = nc.Dataset(filename, 'r')
        d = {}
        # use dictionary above to just read fields we want out of the netcdf.
        for k in list(ioda2nc.keys()):
            d[k] = ncd[ioda2nc[k]][...]
            d[k].mask = False
        ncd.close()
        return d

    def _just_flatten(self, d):
        # make a temporary data dictionary to transfer things into.
        dd = {}
        for k in list(d.keys()):
            if(k == 'dateTime'):
                # for flat array need to make it 2d to match other arrays before flattening again.
                scn = np.arange(1, d['latitude'].shape[1]+1)
                scn_tmp, tmp = np.meshgrid(scn, d[k])
                scn_tmp = scn_tmp.astype('float32')
                tmp = tmp.astype(np.int64)
                dd[k] = tmp.flatten().tolist()
                dd['scan_position'] = scn_tmp.flatten().tolist()
            elif(k == 'Prior_O3'):
                dd[k] = d[k][:, :, 0].flatten().tolist()
            else:
                dd[k] = d[k].flatten().tolist()
        dtA = np.asarray(dd['dateTime'])
        idx = np.where((self.startTAI <= dtA) & (dtA <= self.endTAI))
        for k in list(dd.keys()):
            dd[k] = np.asarray(dd[k])[idx]
            dd[k] = dd[k].tolist()
        return dd

    def _do_qc(self, d):
        # intialize dictonary of qc'd variables
        dd = {}
        flatVars = list(ioda2nc.keys())
        flatVars.append('scan_position')
        for v in flatVars:
            dd[v] = []

        for itime in range(d['latitude'].shape[0]):
            for iscan in range(d['latitude'].shape[1]):
                if(d['dateTime'][itime] < self.startTAI or d['dateTime'][itime] > self.endTAI):
                    continue
                if (d['prior_o3'][itime, iscan, 0] <= 0.0 or d['valKey'][itime, iscan] <= 0.0):
                    continue
                # from Kris' fortran
                # !! flags as recommended by the OMI team (Jul 2019)
                # !! without having to change the GSI code
                # !! use any alqf as long as it's not 0
                # !! The GSI will reject alqf = 3 so if alqf=0 set it to 3, otherwise set it to 1
                if ((d['algorithm_flag'][itime, iscan] == 0) or (d['algorithm_flag'][itime, iscan] == 3)):
                    continue
                # Code from Kris' Fortran########################
                # !! Bits 0-3 combined into an integer: use 0 or 1 only
                # !! Do not use if bits 6, 8 or 9 are set (to 1)
                # !! counting from bit 1 the bad ones are 7, 9 and 10
                # !! Use everything else
                #  decimal = qf(iscan,itime)
                #  qf(iscan,itime) = 0
                #  call dec2bin(decimal,bintoq,16)
                #  first3 = bintoq(1)+2*bintoq(2)+4*bintoq(3)+8*bintoq(4)
                # !  print *, 'quality DEBUG ', first3, bintoq(:)
                ###############################################

                #  let's simplify the above statements:
                #
                #    bits 0-3 Bit 0 is a don't care (can be 0 or 1)
                #    we do care if bit 1, 2 or 3 are set. Sooo, if those are set, kick out.
                #
                # remember things start at zero, since we're in python not fortran
                one_set = is_bit_set(d['quality_flag'][itime, iscan], 1)
                two_set = is_bit_set(d['quality_flag'][itime, iscan], 2)
                three_set = is_bit_set(d['quality_flag'][itime, iscan], 3)
                if (one_set or two_set or three_set):
                    continue
                six_set = is_bit_set(d['quality_flag'][itime, iscan], 6)
                eight_set = is_bit_set(d['quality_flag'][itime, iscan], 8)
                nine_set = is_bit_set(d['quality_flag'][itime, iscan], 9)
                # break out second condition, just because it's mentioned that way for bits 6,8,9
                if (six_set or eight_set or nine_set):
                    continue
                # could simply this further with one if statement possibly more clever use of a bit masking.
                dd['scan_position'].append(float(iscan+1))
                for v in flatVars:
                    if(v == 'dateTime'):
                        dd[v].append(d[v][itime])
                    elif(v == 'prior_o3'):
                        dd[v].append(d[v][itime, iscan, 0])
                    elif(v != 'scan_position'):
                        dd[v].append(d[v][itime, iscan])

        return dd

    def _read(self):
        # set up variable names for IODA
        for iodavar in ['integrated_layer_ozone_in_air', ]:
            # self._setVarDict(var)
            self._setVarAttr(iodavar)
        # loop through input filenames
        for f in self.filenames:
            nc_data = self._read_nc(f)
            if(self.qcOn):
                print('Doing QC.')
                d = self._do_qc(nc_data)
            else:
                print('Not doing QC.')
                d = self._just_flatten(nc_data)
            # add MetaData variables.
            for v in list(d.keys()):
                if(v != 'valKey'):
                    self.outdata[(v, 'MetaData')].extend(d[v])

            for ncvar, iodavar in obsvars.items():
                self.outdata[self.varDict[iodavar]
                             ['valKey']].extend(d['valKey'])
        DimDict['nlocs'] = len(self.outdata[('longitude', 'MetaData')])
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])
        # add dummy air_pressure so UFO will know this is a total column ob, and not partial.
        self.outdata[('air_pressure', 'MetaData')] = np.zeros(
            DimDict['nlocs']).tolist()
        self.varAttrs[('air_pressure', 'MetaData')]['units'] = 'Pa'

        for k in self.outdata.keys():
            self.outdata[k] = np.asarray(self.outdata[k])
            if(self.outdata[k].dtype == 'float64'):
                self.outdata[k] = self.outdata[k].astype('float32')
            elif(self.outdata[k].dtype == 'int64' and k != ('dateTime', 'MetaData')):
                self.outdata[k] = self.outdata[k].astype('int32')
            elif(self.outdata[k].dtype == 'uint16' or self.outdata[k].dtype == 'uint8'):
                self.outdata[k] = self.outdata[k].astype(int)
        self.outdata[('dateTime', 'MetaData')] = self.outdata[('dateTime', 'MetaData')].astype('int64')
        # ensure lon is 0-360
        self.outdata[('longitude', 'MetaData')] = self.outdata[('longitude', 'MetaData')] % 360


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads OMI O3 HDF5 files provided by NASA (somewhere) '
            'and converts into IODA formatted output files. Multiple '
            'files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of MLS input file(s)",
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
        help="omi filename prefix (default=OMI-Aura_L2-OMTO3)",
        type=str, required=False, default="OMI-Aura_L2-OMTO3", dest='prefix')

    optional.add_argument('--qc', dest='qc', action='store_true', default=True)
    optional.add_argument('--no-qc', dest='qc', action='store_false')

    optional.add_argument(
        '-w', '--window',
        help="assimilation window size in hours",
        type=int, required=False, default=6, dest='window')

    args = parser.parse_args()
    cycle_time = datetime(args.year, args.month, args.day, args.hour)
    if(os.path.isfile(args.input)):
        print('Reading Single File:{}'.format(args.input))
        rawFiles = []
        rawFiles.append(args.input)
    elif(os.path.isdir(args.input)):
        # Get current cycle and associated file(s)
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
            rawFiles.extend(glob.glob(os.path.join(args.input, args.prefix+"_{}m{}{}".format(year, month, day)+"*.he5")))
        rawFiles.sort()
        # only read files in window
        rawFilesOut = []
        for fi, f in enumerate(rawFiles):
            vv = f.split('_')
            # v003-2020m1214t060743.he5 2020m1214t0003-o87313
            startDateFile = datetime.strptime(vv[-2][0:-7], "%Ym%m%dt%H%M")
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
    o3 = omi(rawFiles, startTAI, endTAI, args.qc)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    print("Writing: {}".format(args.output))
    writer.BuildIoda(o3.outdata, VarDims, o3.varAttrs, AttrData)


if __name__ == '__main__':
    main()
