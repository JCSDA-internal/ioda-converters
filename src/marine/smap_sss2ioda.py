#!/usr/bin/env python3

#
# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import sys
import argparse
import numpy as np
from datetime import datetime, timedelta
import netCDF4 as nc
import re
import dateutil.parser
from pathlib import Path

IODA_CONV_PATH = Path(__file__).parent/"@SCRIPT_LIB_PATH@"
if not IODA_CONV_PATH.is_dir():
    IODA_CONV_PATH = Path(__file__).parent/'..'/'lib-python'
sys.path.append(str(IODA_CONV_PATH.resolve()))

import ioda_conv_ncio as iconv
from orddicts import DefaultOrderedDict


vName = "sea_surface_salinity"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("datetime", "string")
]

AttrData = {
    'odb_version': 1,
}


class Salinity(object):
    def __init__(self, filenames, date, writer):
        self.filenames = filenames
        self.date = date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.writer = writer
        self._read()

    # Open obs file and read/load relevant info
    def _read(self):
        valKey = vName, self.writer.OvalName()
        errKey = vName, self.writer.OerrName()
        qcKey = vName, self.writer.OqcName()

        for f in self.filenames:
            print(" Reading file: ", f)
            ncd = nc.Dataset(f, 'r')

            # determine if this is JPL or RSS file.
            # the variable names depend on which type of file it is.
            source = ncd.institution
            if re.search("^Remote Sensing Systems.*", source) is not None:
                source = 'RSS'
                source_var_name = {
                    'time': 'time',
                    'lat': 'cellat',
                    'lon': 'cellon',
                    'sss': 'sss_smap',
                    'sss_qc': 'iqc_flag',
                }
            elif re.search("^JPL.*", source) is not None:
                source = 'JPL'
                source_var_name = {
                    'time': 'row_time',
                    'lat': 'lat',
                    'lon': 'lon',
                    'sss': 'smap_sss',
                    'sss_err': 'smap_sss_uncertainty',
                    'sss_qc': 'quality_flag'
                }
            else:
                print("Error: unknown source: " + source)
                print("Only JPL or RSS sources are handled")
                sys.exit(1)

            # make sure this is lvl 2
            # TODO: handle L3 files at somepoint?)
            if ncd.processing_level[:2] != 'L2':
                print("Error: only L2 files handled for now.")
                sys.exit(1)

            # get base date for file
            s = ' '.join(
                ncd.variables[source_var_name['time']].units.split(' ')[2:3])
            basetime = dateutil.parser.parse(s)

            # read in the fields
            data = {}
            for v in source_var_name:
                if v == 'sss_qc':
                    data[v] = ncd.variables[source_var_name[v]][:].flatten().astype(int)
                else:
                    data[v] = ncd.variables[source_var_name[v]][:].flatten()

            # JPL files have a time for each row,
            # RSS has time for each cell, account for this
            if source == 'JPL':
                col = len(data['lon']) / len(data['time'])
                data['time'] = np.tile(
                    np.array(data['time']), (int(col), 1)).T.flatten()

            # remove masked gridpoints
            mask = np.logical_not(data['sss'].mask)
            for v in source_var_name:
                data[v] = data[v][mask]

            # for each observation
            for i in range(len(data['time'])):
                obs_date = basetime + timedelta(seconds=float(data['time'][i]))
                locKey = data['lat'][i], data['lon'][i], obs_date.strftime(
                    "%Y-%m-%dT%H:%M:%SZ")
                self.data[0][locKey][valKey] = data['sss'][i]
                # if source == 'JPL':          #RTOFS-DA
                #   if data['sss_qc'][i] <= 4:
                #      data['sss_qc'][i] = 0
                #   else:
                #      data['sss_qc'][i] = 1
                self.data[0][locKey][qcKey] = data['sss_qc'][i]
                if 'sss_err' in data:
                    self.data[0][locKey][errKey] = data['sss_err'][i]
                else:
                    self.data[0][locKey][errKey] = 1.0
            ncd.close()


def main():

    parser = argparse.ArgumentParser(
        description=(
            'Read JPL/RSS SMAP sea surface salinity (SSS) file(s) and convert'
            ' to a concatenated IODA formatted output file.')
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="name of sss input file(s)",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="name of ioda output file",
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help="base date for the center of the window",
        metavar="YYYYMMDDHH", type=str, required=True)
    args = parser.parse_args()
    fdate = datetime.strptime(args.date, '%Y%m%d%H')

    writer = iconv.NcWriter(args.output, locationKeyList)

    # Read in the salinity
    sal = Salinity(args.input, fdate, writer)

    # write them out
    AttrData['date_time_string'] = fdate.strftime("%Y-%m-%dT%H:%M:%SZ")

    (ObsVars, LocMdata, VarMdata) = writer.ExtractObsData(sal.data)
    writer.BuildNetcdf(ObsVars, LocMdata, VarMdata, AttrData)


if __name__ == '__main__':
    main()
