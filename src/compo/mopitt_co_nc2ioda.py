#!/usr/bin/env python3

#
# (C) Copyright 2022 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import argparse
import netCDF4 as nc
import numpy as np
from datetime import datetime, timedelta
import os

import lib_python.ioda_conv_engines as iconv
from collections import defaultdict, OrderedDict
from lib_python.orddicts import DefaultOrderedDict

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

obsvars = {
    'carbonmonoxide_total_column': 'carbonmonoxideTotal',
}

AttrData = {
    'converter': os.path.basename(__file__),
    'nvars': np.int32(len(obsvars)),
}

DimDict = {
}

VarDims = {
    'carbonmonoxideTotal': ['Location'],
    'averagingKernel': ['Location', 'Layer'],
    'pressureVertice': ['Location', 'Vertice']
}

# constants
avogadro = 6.02214076E23
scm2sm = 1E4
vmr2col = 2.12E13  # following Deeter 2009 MOPITT documentation
hPa2Pa = 1E2


class mopitt(object):
    def __init__(self, filenames, time_range):
        self.filenames = filenames
        self.time_range = time_range
        self.varDict = defaultdict(lambda: defaultdict(dict))
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        # set up variable names for IODA
        for iodavar in ['carbonmonoxideTotal']:
            self.varDict[iodavar]['valKey'] = iodavar, iconv.OvalName()
            self.varDict[iodavar]['errKey'] = iodavar, iconv.OerrName()
            self.varDict[iodavar]['qcKey'] = iodavar, iconv.OqcName()
            self.varAttrs[iodavar, iconv.OvalName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OerrName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OqcName()]['coordinates'] = 'longitude latitude'
            self.varAttrs[iodavar, iconv.OvalName()]['units'] = 'mol m-2'
            self.varAttrs[iodavar, iconv.OerrName()]['units'] = 'mol m-2'
            self.varAttrs[iodavar, iconv.OqcName()]['units'] = 'unitless'
        # loop through input filenames
        first = True
        for f in self.filenames:

            ncd = nc.Dataset(f, 'r')

            # AttrData['date_time_string']
            StartDateTime = str(ncd.groups['HDFEOS'].groups['ADDITIONAL'].groups['FILE_ATTRIBUTES'].StartDateTime)
            AttrData['sensor'] = 'MOPITT'
            AttrData['platform'] = 'Terra'
            geo = ncd.groups['HDFEOS'].groups['SWATHS'].groups['MOP02'].groups['Geolocation Fields']
            lats = geo.variables['Latitude'][:]
            lons = geo.variables['Longitude'][:]
            secd = geo.variables['SecondsinDay'][:]
            nlevs = ncd.groups['HDFEOS'].groups['SWATHS'].groups['MOP02'].dimensions['nPrs2'].size
            dat = ncd.groups['HDFEOS'].groups['SWATHS'].groups['MOP02'].groups['Data Fields']

            # get and concatenate pressure grid
            pr_sf = dat.variables['SurfacePressure'][:]
            pr_sf = pr_sf[..., np.newaxis]
            pr_gd = np.tile(dat.variables['PressureGrid'][:], (len(lats), 1))
            pr_gd = np.concatenate((pr_sf, pr_gd), axis=1)
            pr_gd = np.concatenate((pr_gd, np.zeros(len(lats))[..., np.newaxis]), axis=1).astype('float32')

            # if one of the flags (channels first 4 or avk no 5) is set to one the data should be flaged
            qa = dat.variables['RetrievalAnomalyDiagnostic'][:].sum(axis=1)

            # time data, we don't need precision beyond the second
            inittime = datetime.strptime(StartDateTime, "%Y-%m-%dT%H:%M:%S.%fZ")
            times = np.array([datetime.strftime(
                inittime + timedelta(seconds=int(i)) - timedelta(seconds=int(secd[0])),
                "%Y-%m-%dT%H:%M:%S")+"Z" for i in secd], dtype=object)
            AttrData['date_time_string'] = times[0]

            # get ak
            AttrData['averaging_kernel_levels'] = np.int32(nlevs)
            # ak to be applied on surface density molecules/m2
            ak_tc_dimless = dat.variables['TotalColumnAveragingKernelDimless'][:]
            ak_tc_dimless.mask = np.ma.nomask

            # get apriori quantities, profile is needed to compute (I-A)x_a
            xa_tc = dat.variables['APrioriCOTotalColumn'][:]
            xa_sf = dat.variables['APrioriCOSurfaceMixingRatio'][:][:, 0]
            xa_sf = xa_sf[..., np.newaxis]
            xa_gd = dat.variables['APrioriCOMixingRatioProfile'][:][:, :, 0]
            xa_gd = np.concatenate((xa_sf, xa_gd), axis=1)

            # get the retrieved values
            xr_tc = dat.variables['RetrievedCOTotalColumn'][:, 0]

            # sum smoothing and measurement error
            er_tc = dat.variables['RetrievedCOTotalColumnDiagnostics'][:].sum(axis=1)

            # convert all concentrations and column to correct units to avoid single precision issues
            u_conv = avogadro / scm2sm
            xa_gd = xa_gd * vmr2col / u_conv
            xa_tc = xa_tc / u_conv
            xr_tc = xr_tc / u_conv
            er_tc = er_tc / u_conv

            # mopitt number of levels is dependent on surface pressure, for data points with sp<900hPa
            # nlevs<10. IODA and UFO cannot handle variable nlayers_kernel for a given instrument
            # make layers of 0 thickness for surface pressure above those fixed MOPITT layers and
            # move the surface xa values (and xr if profile da) up
            for lev in range(nlevs-1):
                zlev = pr_gd[:, lev]-pr_gd[:, lev+1]
                pr_gd[:, lev+1][zlev < 0] = pr_gd[:, lev][zlev < 0]
                xa_gd[:, lev+1][zlev < 0] = xa_gd[:, lev][zlev < 0]
                ak_tc_dimless[:, lev][zlev < 0] = 0

            # now calculate the apriori term to pass to UFO and ensure single precision
            ap_tc = np.zeros(len(lats))
            for lev in range(nlevs):
                ap_tc = ap_tc + ak_tc_dimless[:, lev] * (pr_gd[:, lev] - pr_gd[:, lev+1]) * xa_gd[:, lev]
            ap_tc = xa_tc - ap_tc
            ap_tc = ap_tc.astype('float32')

            # set flag: rule out all anomalous data
            qaf = qa == 0

            # date range to fit DA window
            date_start = datetime.strptime(self.time_range[0], "%Y%m%d%H")
            date_end = datetime.strptime(self.time_range[1], "%Y%m%d%H")
            date_list = [datetime.strptime(date, "%Y-%m-%dT%H:%M:%SZ") for date in times]
            tsf = [(date_i >= date_start) & (date_i < date_end) for date_i in date_list]

            flg = np.logical_and(qaf, tsf)

            # flip stuff for ufo conventions: top -> bottom
            ak_tc_dimless = np.flip(ak_tc_dimless, axis=1)
            pr_gd = np.flip(pr_gd, axis=1)

            if first:
                # add metadata variables
                self.outdata[('dateTime', 'MetaData')] = times[flg]
                self.outdata[('latitude', 'MetaData')] = lats[flg]
                self.outdata[('longitude', 'MetaData')] = lons[flg]

                self.outdata[('aprioriTerm', 'RetrievalAncillaryData')] = ap_tc[flg]
                self.outdata[('averagingKernel', 'RetrievalAncillaryData')] = ak_tc_dimless[flg]
                self.outdata[('pressureVertice', 'RetrievalAncillaryData')] = hPa2Pa * pr_gd[flg]

                self.outdata[self.varDict[iodavar]['valKey']] = xr_tc[flg]
                self.outdata[self.varDict[iodavar]['errKey']] = er_tc[flg]
                self.outdata[self.varDict[iodavar]['qcKey']] = qa[flg]

            else:
                self.outdata[('dateTime', 'MetaData')] = np.concatenate((
                    self.outdata[('dateTime', 'MetaData')], times[flg]))
                self.outdata[('latitude', 'MetaData')] = np.concatenate((
                    self.outdata[('latitude', 'MetaData')], lats[flg]))
                self.outdata[('longitude', 'MetaData')] = np.concatenate((
                    self.outdata[('longitude', 'MetaData')], lons[flg]))

                self.outdata[('aprioriTerm', 'RetrievalAncillaryData')] = np.concatenate((
                    self.outdata[('aprioriTerm', 'RetrievalAncillaryData')], ap_tc[flg]))
                self.outdata[('averagingKernel', 'RetrievalAncillaryData')] = np.concatenate((
                    self.outdata[('averagingKernel', 'RetrievalAncillaryData')], ak_tc_dimless[flg]))
                self.outdata[('pressureVertice', 'RetrievalAncillaryData')] = np.concatenate((
                    self.outdata[('pressureVertice', 'RetrievalAncillaryData')], hPa2Pa * pr_gd[flg]))

                self.outdata[self.varDict[iodavar]['valKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['valKey']], xr_tc[flg]))
                self.outdata[self.varDict[iodavar]['errKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['errKey']], er_tc[flg]))
                self.outdata[self.varDict[iodavar]['qcKey']] = np.concatenate(
                    (self.outdata[self.varDict[iodavar]['qcKey']], qa[flg]))

            first = False

        DimDict['Location'] = len(self.outdata[('dateTime', 'MetaData')])
        AttrData['Location'] = np.int32(DimDict['Location'])
        DimDict['Layer'] = nlevs
        AttrData['Layer'] = np.int32(DimDict['Layer'])
        DimDict['Vertice'] = nlevs + 1
        AttrData['Vertice'] = np.int32(DimDict['Vertice'])

        varname = 'pressureVertice'
        vkey = (varname, 'RetrievalAncillaryData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = 'Pa'

        varname = 'averagingKernel'
        vkey = (varname, 'RetrievalAncillaryData')
        self.varAttrs[vkey]['coordinates'] = 'longitude latitude'
        self.varAttrs[vkey]['units'] = ''


def main():

    # get command line arguments
    parser = argparse.ArgumentParser(
        description=(
            'Reads MOPITT CO hdf5 files provided by NASA/UCAR'
            'and converts into IODA formatted output files. Multiple'
            'files are able to be concatenated.')
    )

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path of MOPITT L2 CO observation hdf5 input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-r', '--time_range',
        help="extract a date range to fit the data assimilation window"
        "format -r YYYYMMDDHH YYYYMMDDHH",
        type=str, metavar=('begindate', 'enddate'), nargs=2,
        default=('1970010100', '2170010100'))

    args = parser.parse_args()

    # Read in the MOPITT CO data
    co = mopitt(args.input, args.time_range)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    # write everything out
    writer.BuildIoda(co.outdata, VarDims, co.varAttrs, AttrData)


if __name__ == '__main__':
    main()
