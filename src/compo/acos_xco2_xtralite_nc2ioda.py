#!/usr/bin/env python3

#
# (C) Copyright 2024 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0
#
# Converts xtralite ACOS XCO2 output (CoDAS NetCDF) to IODA format.
#
# ACOS uses different variable/dimension names from the standard CoDAS layout
# and therefore cannot use the generic xtralite_codas_nc2ioda.py converter:
#   - Record dimension is 'sounding_id', not 'nsound'
#   - Variables are 'xco2_final', 'xco2_uncert', 'xco2_avgker' (not obs/uncert/avgker)
#   - Date/time are 'sounding_date' / 'sounding_time' (not date/time)
#   - QC flag is 'qcflag' (0=good), not 'isbad'
#   - Has pressure weighting function ('pwf') and surface pressure ('psurf')
#   - Optional: 'operation_mode' (GOSAT only)
#
# Supported sub-products (-p):
#   acos_gosat   - ACOS GOSAT XCO2
#   acos_oco2    - ACOS OCO-2 XCO2
#   acos_oco3    - ACOS OCO-3 XCO2
#
# Usage:
#   python acos_xco2_xtralite_nc2ioda.py \
#       -p acos_oco2 \
#       -i acos_oco2_20230115_03z.nc [file2.nc ...] \
#       -o acos_xco2_ioda.nc \
#       -r 2023011500 2023011506
#

import argparse
import netCDF4 as nc
import numpy as np
import os
from collections import defaultdict, OrderedDict
from datetime import datetime, timedelta

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude",  "float"),
    ("longitude", "float"),
    ("dateTime",  "long"),
]

_SENSOR_PLATFORM = {
    'acos_gosat': ('ACOS',  'GOSAT'),
    'acos_oco2':  ('ACOS',  'OCO-2'),
    'acos_oco3':  ('ACOS',  'OCO-3'),
}

_EPOCH = datetime(1993, 1, 1, 0, 0, 0)

DimDict  = {}
AttrData = {
    'converter':    os.path.basename(__file__),
    'nvars':        np.int32(1),
}

IODA_VARNAME = 'carbonDioxideColumn'


def _to_ioda_datetime(date_vals, time_vals):
    offsets = np.empty(len(date_vals), dtype=np.int64)
    for i, (d, t) in enumerate(zip(date_vals, time_vals)):
        ds = str(int(d))
        ts = str(int(t)).zfill(6)
        dt = datetime(int(ds[0:4]), int(ds[4:6]), int(ds[6:8]),
                      int(ts[0:2]), int(ts[2:4]), int(ts[4:6]))
        offsets[i] = int((dt - _EPOCH).total_seconds())
    return offsets


def _time_filter(ioda_times, date_start, date_end):
    dts = [_EPOCH + timedelta(seconds=int(s)) for s in ioda_times]
    return np.array([(d >= date_start) and (d < date_end) for d in dts])


class ACOSConverter(object):

    def __init__(self, product, filenames, time_range):
        self.product    = product
        self.filenames  = filenames
        self.date_start = datetime.strptime(time_range[0], "%Y%m%d%H")
        self.date_end   = datetime.strptime(time_range[1], "%Y%m%d%H")

        self.varDict  = defaultdict(lambda: defaultdict(dict))
        self.outdata  = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))

        sensor, platform = _SENSOR_PLATFORM.get(product, ('ACOS', 'unknown'))
        AttrData['sensor']   = sensor
        AttrData['platform'] = platform

        self._setup_var_keys()
        self._read()

    def _setup_var_keys(self):
        iodavar = IODA_VARNAME
        self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
        self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
        self.varDict[iodavar]['qcKey']  = iodavar, iconv.OqcName()

        for grp in [iconv.OvalName(), iconv.OerrName()]:
            self.varAttrs[iodavar, grp]['units']       = 'ppm'
            self.varAttrs[iodavar, grp]['coordinates'] = 'longitude latitude'
        self.varAttrs[iodavar, iconv.OqcName()]['units']       = 'unitless'
        self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'

        self.varAttrs[('averagingKernel',  'RetrievalAncillaryData')]['units']       = '1'
        self.varAttrs[('averagingKernel',  'RetrievalAncillaryData')]['coordinates'] = 'longitude latitude'
        self.varAttrs[('pressureVertice',  'RetrievalAncillaryData')]['units']       = 'Pa'
        self.varAttrs[('pressureVertice',  'RetrievalAncillaryData')]['coordinates'] = 'longitude latitude'

    def _read(self):
        iodavar = IODA_VARNAME
        first   = True
        nlay    = None

        for f in self.filenames:
            ncd = nc.Dataset(f, 'r')

            lats       = ncd.variables['lat'][:]
            lons       = ncd.variables['lon'][:]
            date_vals  = ncd.variables['sounding_date'][:]
            time_vals  = ncd.variables['sounding_time'][:]
            xco2       = ncd.variables['xco2_final'][:]
            xco2_unc   = ncd.variables['xco2_uncert'][:]
            avgker     = ncd.variables['xco2_avgker'][:]
            pwf        = ncd.variables['pwf'][:]
            psurf      = ncd.variables['psurf'][:]
            qcflag     = ncd.variables['qcflag'][:]

            ncd.close()

            nlay = avgker.shape[1]

            # Build pressure vertice array: surface pressure + fixed pressure levels
            # psurf is the first vertice; avgker levels give interior edges
            # Construct (nsound, nlay+1) in Pa: [psurf, p1, p2, ..., ptop]
            nsound = len(lats)
            pvert  = np.zeros((nsound, nlay + 1), dtype='float32')
            pvert[:, 0] = psurf * 100.0  # hPa -> Pa
            for k in range(nlay):
                # pwf[:,k] = dp_k / g / rho_dry  — reconstruct edge from pwf
                # For ACOS, peavg edges are not directly stored; use pwf as proxy.
                # Store pwf in pressureVertice for now so UFO can reconstruct dp.
                pvert[:, k + 1] = pwf[:, k]

            ioda_times = _to_ioda_datetime(date_vals, time_vals)

            # Time window filter + QC (qcflag == 0 means good)
            mask = _time_filter(ioda_times, self.date_start, self.date_end)
            mask = mask & (np.array(qcflag) == 0)

            def f32(arr): return np.array(arr[mask], dtype='float32')

            lats_s   = f32(lats)
            lons_s   = f32(lons)
            times_s  = ioda_times[mask]
            xco2_s   = f32(xco2)
            unc_s    = f32(xco2_unc)
            ak_s     = f32(avgker)
            pvert_s  = f32(pvert)
            preqc_s  = np.zeros(xco2_s.shape, dtype='int32')  # 0 = passed QC

            if first:
                self.outdata[('latitude',  'MetaData')] = lats_s
                self.outdata[('longitude', 'MetaData')] = lons_s
                self.outdata[('dateTime',  'MetaData')] = times_s

                self.outdata[self.varDict[iodavar]['valKey']] = xco2_s
                self.outdata[self.varDict[iodavar]['errKey']] = unc_s
                self.outdata[self.varDict[iodavar]['qcKey']]  = preqc_s

                self.outdata[('averagingKernel', 'RetrievalAncillaryData')] = ak_s
                self.outdata[('pressureVertice', 'RetrievalAncillaryData')] = pvert_s

            else:
                def cat(key, arr):
                    self.outdata[key] = np.concatenate((self.outdata[key], arr))

                cat(('latitude',  'MetaData'), lats_s)
                cat(('longitude', 'MetaData'), lons_s)
                cat(('dateTime',  'MetaData'), times_s)

                cat(self.varDict[iodavar]['valKey'], xco2_s)
                cat(self.varDict[iodavar]['errKey'], unc_s)
                cat(self.varDict[iodavar]['qcKey'],  preqc_s)

                cat(('averagingKernel', 'RetrievalAncillaryData'), ak_s)
                cat(('pressureVertice', 'RetrievalAncillaryData'), pvert_s)

            first = False

        nlocs = len(self.outdata[('dateTime', 'MetaData')])
        DimDict['Location'] = nlocs
        DimDict['Layer']    = nlay
        DimDict['Vertice']  = nlay + 1

        AttrData['Location'] = np.int32(nlocs)
        AttrData['Layer']    = np.int32(nlay)
        AttrData['Vertice']  = np.int32(nlay + 1)
        AttrData['date_time_string'] = datetime.strftime(
            _EPOCH + timedelta(seconds=int(self.outdata[('dateTime', 'MetaData')][0])),
            "%Y-%m-%dT%H:%M:%SZ")


def main():
    parser = argparse.ArgumentParser(
        description=(
            'Reads xtralite ACOS XCO2 CoDAS NetCDF files and converts to IODA format.'
        )
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-p', '--product',
        help=f'product key; one of: {sorted(_SENSOR_PLATFORM)}',
        type=str, required=True)
    required.add_argument(
        '-i', '--input',
        help='path(s) to xtralite ACOS CoDAS NetCDF file(s)',
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help='path of IODA output file',
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-r', '--time_range',
        help='retain only observations within [begindate, enddate); '
             'format: YYYYMMDDHH YYYYMMDDHH',
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('1970010100', '2170010100'))

    args = parser.parse_args()

    converter = ACOSConverter(args.product, args.input, args.time_range)

    VarDims = {
        IODA_VARNAME:       ['Location'],
        'averagingKernel':  ['Location', 'Layer'],
        'pressureVertice':  ['Location', 'Vertice'],
    }

    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)
    writer.BuildIoda(converter.outdata, VarDims, converter.varAttrs, AttrData)


if __name__ == '__main__':
    main()
