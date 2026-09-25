#!/usr/bin/env python3

#
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Standard Python library imports.
import os
import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime
from pathlib import Path
from collections import defaultdict, OrderedDict, Counter

from pyiodaconv.orddicts import DefaultOrderedDict
import pyiodaconv.ioda_conv_engines as iconv


# Global Dictionaries.
locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("pressure", "float"),
    ("dateTime", "long"),
]

varname_ozone = 'ozoneProfile'

ioda2nc = {}
ioda2nc['latitude'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Latitude'
ioda2nc['longitude'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Longitude'
ioda2nc['dateTime'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Time'
ioda2nc['pressure'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/Pressure'
ioda2nc['valKey'] = 'HDFEOS/SWATHS/O3/Data Fields/O3'
ioda2nc['precision'] = 'HDFEOS/SWATHS/O3/Data Fields/O3Precision'
ioda2nc['convergence'] = 'HDFEOS/SWATHS/O3/Data Fields/Convergence'
ioda2nc['status'] = 'HDFEOS/SWATHS/O3/Data Fields/Status'
ioda2nc['quality'] = 'HDFEOS/SWATHS/O3/Data Fields/Quality'
ioda2nc['solarZenithAngle'] = 'HDFEOS/SWATHS/O3/Geolocation Fields/SolarZenithAngle'

obsvars = {
    'mole_fraction_of_ozone_in_air': varname_ozone,
}

AttrData = {
    'converter': os.path.basename(__file__)
}

DimDict = {
}

VarDims = {
    varname_ozone: ['Location'],
}

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()

# Observation error tables, taken directly from the NASA-provided Fortran
# ingest programs for each MLS O3 product version. 'lvmin' is the 0-based
# level index (matching self.lbot) at which the 'oe' list begins. 'inflation'
# maps 0-based level index to the extra |O3|-scaled term added at that level.
MLS_ERROR_TABLES = {
    # res/write_mls_netcdf_v5.f90 (v5.04), lvmin=8 lvmax=49 (1-based)
    'res-v5': {
        'lvmin': 7,
        'oe': [0.02, 0.02, 0.02, 0.02, 0.035, 0.05, 0.05, 0.05,
               0.125, 0.2, 0.2, 0.2, 0.2, 0.2, 0.225, 0.25, 0.275,
               0.3, 0.3, 0.3, 0.3, 0.3, 0.275, 0.25, 0.225, 0.2, 0.2,
               0.2, 0.2, 0.2, 0.15, 0.1, 0.1, 0.1, 0.15, 0.2, 0.2, 0.2,
               0.3, 0.3, 0.3, 0.3],
        'inflation': {7: 0.30, 8: 0.20, 9: 0.125, 10: 0.05, 11: 0.05, 12: 0.05},
    },
    # nrt/write_mls_netcdf_v5.f90 (NRT v5.03), lvmin=8 lvmax=43 (1-based)
    'nrt-v5': {
        'lvmin': 7,
        'oe': [0.02, 0.02, 0.02, 0.02, 0.035, 0.05, 0.05, 0.05,
               0.125, 0.2, 0.2, 0.2, 0.2, 0.2, 0.225, 0.25, 0.275,
               0.3, 0.3, 0.3, 0.3, 0.3, 0.275, 0.25, 0.225, 0.2, 0.2,
               0.2, 0.2, 0.2, 0.15, 0.1, 0.1, 0.1, 0.15, 0.2],
        'inflation': {7: 0.30, 8: 0.20, 9: 0.125, 10: 0.05, 11: 0.05, 12: 0.05},
    },
    # res/write_mls_netcdf_v6.f90 (v6.03), lvmin=8 lvmax=49 (1-based)
    'res-v6': {
        'lvmin': 7,
        'oe': [0.0200, 0.0101, 0.0074, 0.0050, 0.0050, 0.0050, 0.0523,
               0.0995, 0.1486, 0.1977, 0.2000, 0.2000, 0.2000, 0.2000,
               0.2448, 0.2966, 0.3483, 0.4000, 0.3753, 0.3506, 0.3259,
               0.3012, 0.2780, 0.2550, 0.2320, 0.2089, 0.2000, 0.2000,
               0.2000, 0.2000, 0.1506, 0.1012, 0.0857, 0.0710, 0.0797,
               0.0900, 0.0857, 0.1443, 0.1000, 0.3081, 0.3919, 0.9000],
        'inflation': {7: 0.10, 8: 0.10, 9: 0.10, 10: 0.07, 11: 0.07, 12: 0.07},
    },
}


class mls(object):
    def __init__(self, filenames, lbot, ltop, sTAI, eTAI, errorOn, mls_version='res-v5'):
        self.filenames = filenames
        self.errorOn = errorOn
        self.mls_version = mls_version
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.lbot = lbot
        self.ltop = ltop
        self.startTAI = sTAI
        self.endTAI = eTAI
        for v in list(ioda2nc.keys()):
            if (v != 'valKey' and v != 'errKey'):
                self.outdata[(v, 'MetaData')] = []
        self.outdata[('referenceLevel', 'MetaData')] = []
        self._setVarDict(varname_ozone)
        self.outdata[self.varDict[varname_ozone]['valKey']] = []
        if (self.errorOn):
            self.outdata[self.varDict[varname_ozone]['errKey']] = []

        self._read()

    # set ioda variable keys
    def _setVarDict(self, iodavar):
        self.varDict[iodavar]['valKey'] = iodavar, obsValName
        if (self.errorOn):
            self.varDict[iodavar]['errKey'] = iodavar, obsErrName
        self.varDict[iodavar]['qcKey'] = iodavar, qcName

    # set variable attributes for IODA
    def _setVarAttr(self, iodavar):
        self.varAttrs[iodavar, obsValName]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, obsErrName]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, qcName]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, obsValName]['units'] = 'ppmv'
        self.varAttrs[iodavar, obsErrName]['units'] = 'ppmv'

        varsToAddUnits = list(ioda2nc.keys())
        for v in varsToAddUnits:
            if (v != 'valKey' and v != 'errKey'):
                vkey = (v, 'MetaData')
                if ('pressure' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'Pa'
                elif (v == 'dateTime'):
                    self.varAttrs[vkey]['units'] = 'seconds since 1993-01-01T00:00:00Z'
                elif ('latitude' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'degree_north'
                elif ('longitude' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'degree_east'
                elif ('angle' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'degree'
                elif ('prior' in v.lower()):
                    self.varAttrs[vkey]['units'] = 'ppmv'
                elif (v == 'precision'):
                    self.varAttrs[vkey]['units'] = 'ppmv'
                elif (v == 'status'):
                    self.varAttrs[vkey]['units'] = '1'
                    self.varAttrs[vkey]['long_name'] = 'MLS Status flag'
                elif (v == 'convergence'):
                    self.varAttrs[vkey]['units'] = '1'
                    self.varAttrs[vkey]['long_name'] = 'MLS Convergence'
                elif (v == 'quality'):
                    self.varAttrs[vkey]['units'] = '1'
                    self.varAttrs[vkey]['long_name'] = 'MLS Quality'

    # Read data needed from raw MLS file. All records are returned as-is;
    # records outside the assimilation window are dropped later by
    # _just_flatten's time slice, so no per-version (NRT vs res) record
    # trimming is needed here. Duplicate profiles that may occur where
    # consecutive NRT granules overlap are left for UFO's DuplicateThinning
    # filter to handle downstream.
    def _read_nc(self, filename):
        print("Reading: {}".format(filename))
        ncd = nc.Dataset(filename, 'r')

        d = {}
        for k in list(ioda2nc.keys()):
            if (k == 'pressure'):
                d[k] = ncd[ioda2nc[k]][...]*100.  # convert to Pa
                d[k].mask = False
            else:
                d[k] = ncd[ioda2nc[k]][...]
                d[k].mask = False

            if (k == 'valKey' or k == 'precision'):
                d[k] = d[k]*1e6  # convert mol/mol to PPMV
        return d

    def _calc_error(self, o3, o3_prec, lev):
        # Observation error estimates from MLS, version-specific (see
        # MLS_ERROR_TABLES). 'lev' is the 0-based level index.
        table = MLS_ERROR_TABLES[self.mls_version]
        ooe = table['oe'][lev - table['lvmin']]
        ooe = ooe + (table['inflation'].get(lev, 0.0) * abs(o3))
        ooe = np.sqrt(max((0.5*ooe)**2+(o3_prec)**2, 1.e-6))
        return ooe

    def _just_flatten(self, d):
        # only output desired levels (lbot through ltop)
        dd = {}
        idx, = np.where((np.asarray(d['dateTime']) >= self.startTAI) & (np.asarray(d['dateTime']) <= self.endTAI))
        dd['valKey'] = d['valKey'][idx, self.lbot:self.ltop+1]
        dd['precision'] = d['precision'][idx, self.lbot:self.ltop+1]
        lvec = np.arange(self.lbot+1, self.ltop+2)
        dd['level'], dd['status'] = np.meshgrid(np.arange(self.lbot+1, self.ltop+2), d['status'][idx])
        dd['pressure'], dd['dateTime'] = np.meshgrid(d['pressure'][self.lbot:self.ltop+1], d['dateTime'][idx])
        dd['quality'] = np.tile(d['quality'][idx], (lvec.shape[0], 1)).T
        dd['convergence'] = np.tile(d['convergence'][idx], (lvec.shape[0], 1)).T
        dd['status'] = np.tile(d['status'][idx], (lvec.shape[0], 1)).T
        dd['latitude'] = np.tile(d['latitude'][idx], (lvec.shape[0], 1)).T
        dd['longitude'] = np.tile(d['longitude'][idx], (lvec.shape[0], 1)).T
        dd['solarZenithAngle'] = np.tile(d['solarZenithAngle'][idx], (lvec.shape[0], 1)).T
        for k in list(dd.keys()):
            dd[k] = np.asarray(dd[k])
            dd[k] = dd[k].flatten().tolist()
        return dd

    def _read(self):
        # set up variable names for IODA
        self._setVarAttr(varname_ozone)

        # loop through input filenames
        # Note: no QC-based rejection is done here (status/convergence/quality/
        # precision thresholds). All profiles/levels within the window and
        # level range are passed through, with status, convergence, quality,
        # and precision written out as MetaData so that filtering can be
        # performed downstream by UFO obs filters instead.
        for f in self.filenames:
            nc_data = self._read_nc(f)
            d = self._just_flatten(nc_data)
            if (self.errorOn):
                print("Calculating Error.")
                d['errKey'] = []
                for ival, val in enumerate(d['valKey']):
                    d['errKey'].append(self._calc_error(
                        val, d['precision'][ival], d['level'][ival]-1))
            for v in list(d.keys()):
                if (v == 'level'):
                    self.outdata[('referenceLevel', 'MetaData')].extend(d[v])
                elif (v != 'valKey' and v != 'errKey'):
                    self.outdata[(v, 'MetaData')].extend(d[v])
            for ncvar, iodavar in obsvars.items():
                self.outdata[self.varDict[iodavar]
                             ['valKey']].extend(d['valKey'])
                if (self.errorOn):
                    self.outdata[self.varDict[iodavar]['errKey']].extend(d['errKey'])

        nlocs = len(self.outdata[('dateTime', 'MetaData')])
        DimDict['Location'] = nlocs

        for k in self.outdata.keys():
            self.outdata[k] = np.asarray(self.outdata[k])
            if (self.outdata[k].dtype == 'float64'):
                self.outdata[k] = self.outdata[k].astype('float32')
            elif (self.outdata[k].dtype == 'int64' and k != ('dateTime', 'MetaData')):
                self.outdata[k] = self.outdata[k].astype('int32')
        self.outdata[('dateTime', 'MetaData')] = self.outdata[('dateTime', 'MetaData')].astype(np.int64)
        self.outdata[('longitude', 'MetaData')] = self.outdata[('longitude', 'MetaData')] % 360
# end mls object.


def _parse_window_bound(value):
    return datetime.strptime(value, "%Y-%m-%dT%H:%M:%SZ")


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
        help=(
            "path(s) of one or more MLS input file(s) covering the desired "
            "assimilation window. File discovery for a DA window (e.g. "
            "selecting the daily 'res' file(s) or NRT granules that overlap "
            "it) is expected to be done by the calling ingest workflow."),
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--window-begin',
        help=(
            "assimilation window start time, ISO8601 format "
            "(e.g. 2026-09-07T09:00:00Z). If omitted, no lower time bound "
            "is applied."),
        type=_parse_window_bound, required=False, default=None, dest='window_begin')
    optional.add_argument(
        '--window-end',
        help=(
            "assimilation window end time, ISO8601 format "
            "(e.g. 2026-09-07T15:00:00Z). If omitted, no upper time bound "
            "is applied."),
        type=_parse_window_bound, required=False, default=None, dest='window_end')
    optional.add_argument(
        '-b', '--level-bottom',
        help="mls level to start 1 based index (default=8)",
        type=int, required=False, default=8, dest='lbot')
    optional.add_argument(
        '-t', '--level-top',
        help="mls level to end 1 based index (default=49)",
        type=int, required=False, default=49, dest='ltop')
    optional.add_argument('--error', dest='error', action='store_true', default=True)
    optional.add_argument('--no-error', dest='error', action='store_false')
    optional.add_argument(
        '--mls-version',
        help="MLS product version, selects the observation-error table (default=res-v5)",
        type=str, required=False, default='res-v5',
        choices=list(MLS_ERROR_TABLES.keys()), dest='mls_version')

    args = parser.parse_args()

    rawFiles = sorted(args.input)

    # The observation-error table only covers levels [table_lvmin, table_lvmax]
    # (1-based); indexing outside that range would either raise an IndexError
    # (ltop too high) or silently wrap to the wrong table entry via negative
    # indexing (lbot too low). Fail loudly here instead.
    if (args.error):
        table = MLS_ERROR_TABLES[args.mls_version]
        table_lvmin = table['lvmin'] + 1
        table_lvmax = table['lvmin'] + len(table['oe'])
        if not (table_lvmin <= args.lbot <= args.ltop <= table_lvmax):
            parser.error(
                "--level-bottom/--level-top ({}-{}) must be within [{}-{}] for "
                "--mls-version {} (or pass --no-error).".format(
                    args.lbot, args.ltop, table_lvmin, table_lvmax, args.mls_version))

    # get start and end times for cropping data in MLS native time format
    # (TAI seconds since Jan 1, 1993.). Unbounded on either side if not given.
    startTAI = ((args.window_begin - datetime(1993, 1, 1, 0)).total_seconds()
                if args.window_begin is not None else -np.inf)
    endTAI = ((args.window_end - datetime(1993, 1, 1, 0)).total_seconds()
              if args.window_end is not None else np.inf)

    # Read in the O3 data over selected levels (if not default 8-49), sliced
    # down to the requested time window. All records from all input files are
    # read in full and then time-sliced uniformly, whether the files are NRT
    # granules or daily 'res' files; any duplicate profiles from overlapping
    # NRT granules are expected to be removed downstream by UFO's
    # DuplicateThinning filter.
    o3 = mls(rawFiles, args.lbot-1, args.ltop-1, startTAI, endTAI, args.error, args.mls_version)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    print("Writing: {}".format(args.output))
    writer.BuildIoda(o3.outdata, VarDims, o3.varAttrs, AttrData)


if __name__ == '__main__':
    main()
