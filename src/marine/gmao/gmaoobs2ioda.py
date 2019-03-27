#!/usr/bin/env python

from __future__ import print_function
from argparse import ArgumentParser, ArgumentDefaultsHelpFormatter
import ioda_conv_ncio as iconv
import netCDF4 as nc4
from datetime import datetime
from collections import defaultdict


class GMAOobs(object):

    def __init__(self, filename, date):

        self.filename = filename
        self.date = date

        # obsid_dict as defined in ocean_obs.py
        self.obsid_dict = {
            5521: 'sea_water_salinity',  # Salinity
            3073: 'sea_water_temperature',  # Temperature
            5525: 'sea_surface_temperature',  # SST
            5526: 'obs_absolute_dynamic_topography',  # SSH (Not used ...)
            5351: 'obs_absolute_dynamic_topography',  # SSH
            6000: 'sea_ice_area_fraction',  # AICE
            6001: 'sea_ice_thickness'   # HICE
        }

        self._read()

        return

    def _read(self):

        odata = {}

        nc = nc4.Dataset(self.filename)

        odata['nobs'] = len(nc.dimensions['nobs'])

        odata['typ'] = nc.variables['typ'][:].data
        odata['lon'] = nc.variables['lon'][:].data
        odata['lat'] = nc.variables['lat'][:].data
        odata['depth'] = nc.variables['depth'][:].data
        odata['value'] = nc.variables['value'][:].data
        odata['oerr'] = nc.variables['oerr'][:].data

        nc.close()

        self.odata = odata

        return

    def to_ioda(self, foutput):

        if self.odata['nobs'] <= 0:
            print('No GMAO observations for IODA!')
            return

        locationKeyList = [
            ("latitude", "float"),
            ("longitude", "float"),
            ("level", "float"),
            ("date_time", "string")
        ]

        AttrData = {
            'odb_version': 1,
            'date_time_string': self.date.strftime("%Y-%m-%dT%H:%M:%SZ")
        }

        writer = iconv.NcWriter(foutput, [], locationKeyList)

        keyDict = defaultdict(lambda: defaultdict(dict))
        for key in self.obsid_dict.keys():
            keyDict[key]['name'] = self.obsid_dict[key]
            keyDict[key]['valKey'] = keyDict[key]['name'], writer.OvalName()
            keyDict[key]['errKey'] = keyDict[key]['name'], writer.OerrName()
            keyDict[key]['qcKey'] = keyDict[key]['name'], writer.OqcName()

        # idata is the dictionary containing IODA friendly data structure
        idata = defaultdict(lambda: defaultdict(dict))

        for n in range(self.odata['nobs']):

            lat = self.odata['lat'][n]
            lon = self.odata['lon'][n]
            lvl = self.odata['depth'][n]

            locKey = lat, lon, lvl, self.date.strftime("%Y-%m-%dT%H:%M:%SZ")

            typ = self.odata['typ'][n]
            val = self.odata['value'][n]
            oerr = self.odata['oerr'][n]

            idata[0][locKey][keyDict[typ]['valKey']] = val
            idata[0][locKey][keyDict[typ]['errKey']] = oerr
            idata[0][locKey][keyDict[typ]['qcKey']] = 0

        writer.BuildNetcdf(idata, AttrData)

        return


if __name__ == '__main__':

    desc = 'Convert GMAO ocean observations to IODA netCDF4 format'
    parser = ArgumentParser(
        description=desc,
        formatter_class=ArgumentDefaultsHelpFormatter)
    parser.add_argument(
        '-i', '--input', help='name of the input GMAO ocean obs file',
        type=str, required=True)
    parser.add_argument(
        '-o', '--output', help='name of the output IODA file',
        type=str, required=True)
    parser.add_argument(
        '-d', '--date', help='file date', metavar='YYYYMMDDHH',
        type=str, required=True)

    args = parser.parse_args()

    finput = args.input
    foutput = args.output
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    gmao = GMAOobs(finput, fdate)
    gmao.to_ioda(foutput)
