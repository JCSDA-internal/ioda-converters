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
from collections import defaultdict, OrderedDict, Counter

# Append pyioda paths so ioda_conv_engines can be loaded
IODA_CONV_PATH = Path(__file__).parent/"../lib/pyiodaconv"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))
from orddicts import DefaultOrderedDict
import ioda_conv_engines as iconv


# Global Dictionaries.
locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("air_pressure", "float"),
    ("dateTime", "integer"),
]

ioda2nc = {}
ioda2nc['latitude'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Latitude'
ioda2nc['longitude'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Longitude'
ioda2nc['dateTime'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Time'
ioda2nc['air_pressure'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Pressure'
ioda2nc['valKey'] = 'HDFEOS/SWATHS/O3/Data Fields/O3'
ioda2nc['precision'] = 'HDFEOS/SWATHS/O3/Data Fields/O3Precision'
ioda2nc['convergence'] = 'HDFEOS/SWATHS/O3/Data Fields/Convergence'
ioda2nc['status'] = 'HDFEOS/SWATHS/O3/Data Fields/Status'
ioda2nc['quality'] = 'HDFEOS/SWATHS/O3/Data Fields/Quality'
ioda2nc['solar_zenith_angle'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/SolarZenithAngle'

obsvars = {
    'mole_fraction_of_ozone_in_air': 'mole_fraction_of_ozone_in_air',
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
}

DimDict = {
}

VarDims = {
    'mole_fraction_of_ozone_in_air': ['nlocs'],
}


class mls(object):
    def __init__(self, filenames, lvmin, lvmax, sTAI, eTAI, nrt, qcOn, errorOn):
        self.filenames = filenames
        self.qcOn = qcOn
        self.errorOn = errorOn
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.lmin = lvmin
        self.lmax = lvmax
        self.startTAI = sTAI
        self.endTAI = eTAI
        self.nrt = nrt
        for v in list(ioda2nc.keys()):
            if(v != 'valKey' and v != 'errKey'):
                self.outdata[(v, 'MetaData')] = []
        self.outdata[('level', 'MetaData')] = []
        self._setVarDict('mole_fraction_of_ozone_in_air')
        self.outdata[self.varDict['mole_fraction_of_ozone_in_air']['valKey']] = []
        if(self.qcOn):
            self.outdata[self.varDict['mole_fraction_of_ozone_in_air']['errKey']] = []

        self._read()

    # set ioda variable keys
    def _setVarDict(self, iodavar):
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        if(self.qcOn):
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()

    # set variable attributes for IODA
    def _setVarAttr(self, iodavar):
        self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'ppmv'
        self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'ppmv'

        varsToAddUnits = list(ioda2nc.keys())
        varsToAddUnits.append('level')
        for v in varsToAddUnits:
            if(v != 'valKey' and v != 'errKey'):
                vkey = (v, 'MetaData')
                if('pressure' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'Pa'
                elif(v == 'dateTime'):
                    self.varAttrs[vkey]['units'] = 'seconds since 1993-01-01T00:00:00Z'
                elif('angle' in v.lower() or 'latitude' in v.lower() or 'longitude' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'degrees'
                elif('flag' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'unitless'
                elif('prior' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'ppmv'
                else:
                    self.varAttrs[vkey]['units'] = 'unitless'

    # Read data needed from raw MLS file.
    def _read_nc(self, filename, ifile, maxfile):
        print("Reading: {}".format(filename))
        ncd = nc.Dataset(filename, 'r')
        end_of_file = len(ncd[ioda2nc['latitude']][:])
        # unles nrt, use all profiles in file.
        start = 0
        end = end_of_file
        # if it is nrt, and the last file use all but first two and last 3 profiles
        if(ifile == maxfile and self.nrt):
            print("last file:{}".format(filename))
            start = 2
            end = end_of_file - 3
        # otherwise if nrt skip first 2 and last 8 profiles to avoid duplicates.
        elif(self.nrt):
            start = 2
            end = end_of_file - 8

        d = {}
        for k in list(ioda2nc.keys()):
            if (k == 'air_pressure'):
                d[k] = ncd[ioda2nc[k]][...]*100.  # convert to Pa
                d[k].mask = False
            else:
                d[k] = ncd[ioda2nc[k]][start:end, ...]
                d[k].mask = False

            if(k == 'valKey' or k == 'precision'):
                d[k] = d[k]*1e6  # convert mol/mol to PPMV
        return d

    def _calc_error(self, o3, o3_prec, lev):
        # Observation error estimates from MLS.
        oe = ['na', 'na', 'na', 'na', 'na', 'na', 'na', 0.02, 0.02, 0.02, 0.02, 0.035, 0.05, 0.05, 0.05,
              0.125, 0.2, 0.2, 0.2, 0.2, 0.2, 0.225, 0.25, 0.275,
              0.3, 0.3, 0.3, 0.3, 0.3, 0.275, 0.25, 0.225, 0.2, 0.2,
              0.2, 0.2, 0.2, 0.15, 0.1, 0.1, 0.1, 0.15, 0.2, 0.2, 0.2,
              0.3, 0.3, 0.3, 0.3]
        ooe = oe[lev]
        # inflate errors for specific levels. (note zero based index)
        if (lev == 7):  # 261 hPa
            ooe = ooe + (0.30 * abs(o3))  # 0.3 is my conservative guess
        elif (lev == 8):  # 215 hPa
            ooe = ooe + (0.20 * abs(o3))
        elif (lev == 9):  # 177 hPa
            ooe = ooe + (0.125 * abs(o3))
        elif (lev == 10 or lev == 11 or lev == 12):  # 150-100 hPa
            ooe = ooe + (0.05 * abs(o3))

        ooe = np.sqrt(max((0.5*ooe)**2+(o3_prec)**2, 1.e-6))
        return ooe

    def _just_flatten(self, d):
        # only output desired levels (lmin through lmax)
        dd = {}
        idx, = np.where((np.asarray(d['dateTime']) >= self.startTAI) & (np.asarray(d['dateTime']) <= self.endTAI))
        dd['valKey'] = d['valKey'][idx, self.lmin:self.lmax+1]
        dd['precision'] = d['precision'][idx, self.lmin:self.lmax+1]
        lvec = np.arange(self.lmin+1, self.lmax+2)
        dd['level'], dd['status'] = np.meshgrid(np.arange(self.lmin+1, self.lmax+2), d['status'][idx])
        dd['air_pressure'], dd['dateTime'] = np.meshgrid(d['air_pressure'][self.lmin:self.lmax+1], d['dateTime'][idx])
        dd['quality'] = np.tile(d['quality'][idx], (lvec.shape[0], 1)).T
        dd['convergence'] = np.tile(d['convergence'][idx], (lvec.shape[0], 1)).T
        dd['status'] = np.tile(d['status'][idx], (lvec.shape[0], 1)).T
        dd['latitude'] = np.tile(d['latitude'][idx], (lvec.shape[0], 1)).T
        dd['longitude'] = np.tile(d['longitude'][idx], (lvec.shape[0], 1)).T
        dd['solar_zenith_angle'] = np.tile(d['solar_zenith_angle'][idx], (lvec.shape[0], 1)).T
        for k in list(dd.keys()):
            dd[k] = np.asarray(dd[k])
            dd[k] = dd[k].flatten().tolist()
        return dd

    def _do_qc(self, d):
        dd = {}
        for k in list(d.keys()):
            dd[k] = []
        dd['errKey'] = []
        dd['level'] = []
        nrec = d['latitude'].shape[0]
        cnt = 0
        for irec in range(nrec):
            if(d['status'][irec] % 2 != 0 or d['convergence'][irec] >= 1.03 or d['quality'][irec] <= 1.0):
                continue
            for ilev in range(self.lmin, self.lmax+1):
                if(d['precision'][irec, ilev] < 0.0):
                    continue
                # if outside the window, don't inculde data
                if(d['dateTime'][irec] < self.startTAI or d['dateTime'][irec] > self.endTAI):
                    continue
                for k in list(d.keys()):
                    if (len(d[k].shape) == 1 and k != 'air_pressure'):
                        dd[k].append(d[k][irec])
                    elif (k == 'air_pressure'):
                        dd[k].append(d[k][ilev])
                    elif k != 'errKey':
                        dd[k].append(d[k][irec, ilev])
                dd['level'].append(ilev+1)
        return dd

    def _read(self):
        # set up variable names for IODA
        self._setVarAttr('mole_fraction_of_ozone_in_air')

        # loop through input filenames
        for i, f in enumerate(self.filenames):
            nc_data = self._read_nc(f, i, len(self.filenames)-1)
            if(self.qcOn):
                print("Performing QC.")
                d = self._do_qc(nc_data)
            else:
                print("Not Performing QC.")
                d = self._just_flatten(nc_data)
            if(self.errorOn):
                print("Calculating Error.")
                d['errKey'] = []
                for ival, val in enumerate(d['valKey']):
                    d['errKey'].append(self._calc_error(
                        val, d['precision'][ival], d['level'][ival]-1))
            for v in list(d.keys()):
                if(v != 'valKey' and v != 'errKey'):
                    self.outdata[(v, 'MetaData')].extend(d[v])
            for ncvar, iodavar in obsvars.items():
                self.outdata[self.varDict[iodavar]
                             ['valKey']].extend(d['valKey'])
                if(self.errorOn):
                    self.outdata[self.varDict[iodavar]['errKey']].extend(d['errKey'])

        DimDict['nlocs'] = np.float32(len(self.outdata[('dateTime', 'MetaData')]))
        AttrData['nlocs'] = np.int32(DimDict['nlocs'])

        for k in self.outdata.keys():
            self.outdata[k] = np.asarray(self.outdata[k])
            if(self.outdata[k].dtype == 'float64'):
                self.outdata[k] = self.outdata[k].astype('float32')
            elif(self.outdata[k].dtype == 'int64' and k != ('dateTime', 'MetaData')):
                self.outdata[k] = self.outdata[k].astype('int32')
        self.outdata[('dateTime', 'MetaData')] = self.outdata[('dateTime', 'MetaData')].astype(np.int64)
        self.outdata[('longitude', 'MetaData')] = self.outdata[('longitude', 'MetaData')] % 360
# end mls object.


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads MLS O3 HDF5 files provided by NASA (somewhere) '
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
        '-s', '--level-start',
        help="mls level to start 1 based index (default=8)",
        type=int, required=False, default=8, dest='lmin')
    optional.add_argument(
        '-e', '--level-end',
        help="mls level to end 1 based index (default=49)",
        type=int, required=False, default=49, dest='lmax')
    optional.add_argument(
        '-p', '--prefix',
        help="mls filename prefix (default=MLS-Aura_L2GP-O3_v05-01)",
        type=str, required=False, default="MLS-Aura_L2GP-O3_v05-01", dest='prefix')
    optional.add_argument('--qc', dest='qc', action='store_true', default=True)
    optional.add_argument('--no-qc', dest='qc', action='store_false')
    optional.add_argument('--error', dest='error', action='store_true', default=True)
    optional.add_argument('--no-error', dest='error', action='store_false')
    optional.add_argument(
        '-w', '--window',
        help="assimilation window size in hours",
        type=int, required=False, default=6, dest='window')

    args = parser.parse_args()

    # check for option to modify levels output
    if(args.lmin < 8):
        print("Sorry, level 8 is low as I go! Setting lmin=8.")
        lmin = 8
    else:
        lmin = args.lmin
    if(args.lmax > 49):
        print("Sorry, level 49 is high as I go! Setting lmax=49.")
        lmax = 49
    else:
        lmax = args.lmax
    if('NRT' in args.prefix):
        nrt = True
    else:
        nrt = False
    # get current cycle time and start/end of the window
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
            doy = now.strftime('%j')
            rawFiles.extend(glob.glob(os.path.join(args.input, args.prefix+"*{}d".format(year)+doy+"*.he5")))
        rawFiles.sort()
        if(nrt):
            # limit files only between start and end of window.
            rawFilesOut = []
            startDateWindow = cycle_time - timedelta(hours=3)
            endDateWindow = cycle_time + timedelta(hours=3)
            for f in rawFiles:
                ftime = datetime.strptime(f[-17::], "%Yd%jt%H%M.he5")
                if(startDateWindow <= ftime <= endDateWindow):
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

    # Read in the O3 data in window over selected levels (if not default 8-49)
    # if nrt is set, assumes near real-time and will skip first two and last 8 profiles for most files
    # (last 3 profiles skipped if it is the last file in window)
    # RTM regarding Near Real time (NRT) in
    # https://discnrt1.gesdisc.eosdis.nasa.gov/data/Aura_NRT/ML2SO2_NRT.005/doc/NRT-user-guide-v5.pdf
    o3 = mls(rawFiles, lmin-1, lmax-1, startTAI, endTAI, nrt, args.qc, args.error)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    print("Writing: {}".format(args.output))
    writer.BuildIoda(o3.outdata, VarDims, o3.varAttrs, AttrData)


if __name__ == '__main__':
    main()
