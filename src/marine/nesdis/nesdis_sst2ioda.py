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

        for f in self.filename:
            ncd = nc.Dataset(f, 'r')
            lvl = ncd.processing_level
            time = ncd.variables['time'][0]
            time_units = ncd.variables['time'].units[-20:]
            basetime = dateutil.parser.parse(time_units)
            basetime += timedelta(seconds=float(time))

            lons = ncd.variables['lon'][:].flatten()
            lats = ncd.variables['lat'][:].flatten()
            sst_qc   = ncd.variables['quality_level'][:].flatten()

            if lvl[:2] == 'L3':
                # If L3, we need to convert 1D lat/lon to 2D lat/lon
                lons,lats=np.meshgrid(lons,lats)
                lons=lons.flatten()
                lats=lats.flatten()
                mask = np.logical_not(sst_qc.mask)
            else:
                mask = sst_qc >= 0
                
            # TODO: this is a currently a hack.
            #  all questionable obs are removed.
            #  This should be changed to include all obs and their qc flags
            #  once the preqc filter works
            mask = np.logical_and(mask, sst_qc == 5)

            lons = lons[mask]
            lats = lats[mask]
            sst_qc = sst_qc[mask]

            dtime = ncd.variables['sst_dtime'][:].flatten()[mask]
            sst_skin = ncd.variables['sea_surface_temperature'][:].flatten()[mask] -273.15
            sst_bias = ncd.variables['sses_bias'][:].flatten()[mask]
            sst_err  = ncd.variables['sses_standard_deviation'][:].flatten()[mask]
                
            sst_bulk = sst_skin - sst_bias

            # for each observation
            for i in range(len(lons)):
                # I'm not sure why the L3 sets have bad sst err fields... investigate
                if sst_err[i] is np.ma.masked:
                    continue
                
                obs_date = basetime + timedelta(seconds=float(dtime[i]))
                locKey = lats[i], lons[i], obs_date.strftime("%Y-%m-%dT%H:%M:%SZ")

                #print(sst_err[i], sst_bulk[i], sst_skin[i])
                
                self.data[0][locKey][sst_valKey] = sst_bulk[i]
                self.data[0][locKey][sst_errKey] = sst_err[i]
                self.data[0][locKey][sst_qcKey] = 0 # TODO, fill this in

                self.data[0][locKey][sst_skin_valKey] = sst_skin[i]
                self.data[0][locKey][sst_skin_errKey] = sst_err[i]
                self.data[0][locKey][sst_skin_qcKey] = 0 # TODO, fill this in


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
    parser.add_argument('-i',
                        help="NESDIS SST obs input files (wild cards or list)",
                        type=str, nargs='+', required=True)
    parser.add_argument('-o',
                        help="name of ioda output file",
                        type=str, required=True)
    parser.add_argument('-d', '--date',
                        help="base date", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')
    writer = iconv.NcWriter(args.o, [], locationKeyList)

    # Read in the altimeter
    altim = Observation(args.i, fdate, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    writer.BuildNetcdf(altim.data, AttrData)
