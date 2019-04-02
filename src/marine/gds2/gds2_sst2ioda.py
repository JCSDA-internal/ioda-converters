#!/usr/bin/env python

from __future__ import print_function
import argparse
import ioda_conv_ncio as iconv
import netCDF4 as nc
from datetime import datetime, timedelta
import dateutil.parser
from collections import defaultdict
import numpy as np


sst_name = "sea_surface_temperature",
sst_skin_name = "sea_surface_skin_temperature",


input_vars = (
    'quality_level',
    'sst_dtime',
    'sses_bias',
    'sses_standard_deviation',
    'sea_surface_temperature')


class Observation(object):

    def __init__(self, filename, date, writer):

        self.filename = filename
        self.date = date
        self.data = defaultdict(lambda: defaultdict(dict))
        self.writer = writer
        self._read()

    def _read(self):

        sst_valKey = sst_name, self.writer.OvalName()
        sst_errKey = sst_name, self.writer.OerrName()
        sst_qcKey = sst_name, self.writer.OqcName()

        sst_skin_valKey = sst_skin_name, self.writer.OvalName()
        sst_skin_errKey = sst_skin_name, self.writer.OerrName()
        sst_skin_qcKey = sst_skin_name, self.writer.OqcName()

        np.random.seed(int((self.date-datetime(1970, 1, 1)).total_seconds()))

        for f in self.filename:
            ncd = nc.Dataset(f, 'r')
            lvl = ncd.processing_level
            time_base = ncd.variables['time'][:]
            basetime = dateutil.parser.parse(ncd.variables['time'].units[-20:])

            print(f)

            # other global attributes we might want saved to the output file
            for v in ('source', 'platform', 'sensor'):
                AttrData[v] = ncd.getncattr(v)

            # Determine the lat/lon grid.
            # If L3, we need to convert 1D lat/lon to 2D lat/lon.
            # If len(time) > 1, also need to repeat the lat/lon vals
            lons = ncd.variables['lon'][:].ravel()
            lats = ncd.variables['lat'][:].ravel()
            if lvl[:2] == 'L3':
                lons, lats = np.meshgrid(lons, lats)
                lons = lons.ravel()
                lats = lats.ravel()
            lons = np.tile(lons, len(time_base)).ravel()
            lats = np.tile(lats, len(time_base)).ravel()

            # calculate the time offsets
            time = np.tile(np.zeros(len(lons) / len(time_base)),
                           (len(time_base), 1))
            for i in range(len(time_base)):
                time[i, :] = time_base[i]
            time = time.ravel()

            # load in all the other data
            data = {}
            for v in input_vars:
                data[v] = ncd.variables[v][:].ravel()

            # TODO: this is a currently a hack.
            #  all questionable obs are removed.
            #  This should be changed to include all obs and their qc flags
            #  once the preqc filter works
            mask = data['quality_level'] == 5

            # TODO: this is also currently a hack.
            #  jedi is not handling thinning the way I need, so
            #  obs are optionally thinned here.
            #  This should be removed once OOPS thins in a better way.
            mask_thin = np.random.uniform(size=len(mask)) > args.thin
            mask = np.logical_and(mask, mask_thin)

            # sometimes the input variables have masks
            for v in input_vars:
                if np.ma.is_masked(data[v]):
                    mask = np.logical_and(mask,
                                          np.logical_not(data[v].mask))

            # apply the masks
            time = time[mask]
            lons = lons[mask]
            lats = lats[mask]
            for v in input_vars:
                data[v] = data[v][mask]

            # calculate output values
            sst_skin = data['sea_surface_temperature'] - 273.15
            sst_bulk = sst_skin - data['sses_bias']

            # for each observation, save it to the dictionary
            for i in range(len(lons)):
                obs_date = basetime + \
                    timedelta(seconds=float(time[i]+data['sst_dtime'][i]))
                locKey = lats[i], lons[i], obs_date.strftime(
                    "%Y-%m-%dT%H:%M:%SZ")

                self.data[0][locKey][sst_valKey] = sst_bulk[i]
                self.data[0][locKey][sst_errKey] = data['sses_standard_deviation'][i]
                self.data[0][locKey][sst_qcKey] = 5 - data['quality_level'][i]

                self.data[0][locKey][sst_skin_valKey] = sst_skin[i]
                self.data[0][locKey][sst_skin_errKey] = data['sses_standard_deviation'][i]
                self.data[0][locKey][sst_skin_qcKey] = 5 - \
                    data['quality_level'][i]


locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("date_time", "string")
]

AttrData = {
    'odb_version': 1,
}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(
        description=('')
    )
    parser.add_argument('-i', '--input',
                        help="GHRSST GDS2.0 generic SST obs input files (wild cards or list)",
                        type=str, nargs='+', required=True)
    parser.add_argument('-o', '--output',
                        help="name of ioda output file",
                        type=str, required=True)
    parser.add_argument('-d', '--date',
                        help="base date", type=str, required=True)
    parser.add_argument('-t', '--thin',
                        help="amount of random thinning, from 0.0 to 1.0",
                        type=float, default=0.0)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')
    writer = iconv.NcWriter(args.output, [], locationKeyList)

    if args.thin != 0.0:
        AttrData['thinning'] = args.thin

    # Read in the altimeter
    altim = Observation(args.input, fdate, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    writer.BuildNetcdf(altim.data, AttrData)
