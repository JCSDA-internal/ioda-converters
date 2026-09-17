#!/usr/bin/env python3

#
# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import os
import sys
import argparse
import numpy as np
from datetime import datetime, timedelta, timezone
import netCDF4 as nc
import re
import dateutil.parser
from glob import glob
from pathlib import Path
import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict

os.environ["TZ"] = "UTC"

vName = "seaSurfaceSalinity"

locationKeyList = [
    ("latitude", "float"),
    ("longitude", "float"),
    ("dateTime", "long"),
]

GlobalAttrs = {}

iso8601_string = 'seconds since 1970-01-01T00:00:00Z'
epoch = datetime.fromisoformat(iso8601_string[14:-1])


class Salinity(object):
    def __init__(self, filenames, start_date, end_date):
        self.filenames = filenames
        self.start_date = start_date
        self.end_date = end_date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self):
        valKey = vName, iconv.OvalName()
        errKey = vName, iconv.OerrName()
        qcKey = vName, iconv.OqcName()

        for f in self.filenames:
            print(" Reading file: ", f)
            ncd = nc.Dataset(f, 'r')

            if 'L2B_SSS' not in f:
                raise ValueError(
                    f"Unsupported file type for '{f}'. This converter expects JPL SMAP 2B h5 files. "
                    "Use smap2c_sss2ioda.py for RSS or SMAP 2C files."
                )

            source = 'JPL'
            source_var_name = {
                'time': 'row_time',
                'lat': 'lat',
                'lon': 'lon',
                'sss': 'smap_sss',
                'sss_err': 'smap_sss_uncertainty',
                'sss_qc': 'quality_flag',
            }

            data = {}
            for v in source_var_name:
                if v == 'sss_qc':
                    data[v] = ncd.variables[source_var_name[v]][:].flatten().astype(np.int32)
                else:
                    data[v] = ncd.variables[source_var_name[v]][:].flatten()

            x = ncd.dimensions['phony_dim_0'].size
            data['time'] = np.tile(data['time'], (x, 1)).flatten()
            match = re.search(r'_(\d{8})T\d{6}', f)
            if not match:
                raise ValueError("Could not parse date from filename.")
            file_date = datetime.strptime(match.group(1), '%Y%m%d').replace(tzinfo=timezone.utc)
            epoch_offset = file_date.timestamp()

            data['time'] = data['time'] + epoch_offset
            time_mask = ((data['time'] >= self.start_date.timestamp()) &
                         (data['time'] <= self.end_date.timestamp()))
            mask = np.logical_not(data['sss'].mask) & time_mask
            for v in source_var_name:
                data[v] = data[v][mask]

            for i in range(len(data['time'])):
                locKey = data['lat'][i], data['lon'][i], data['time'][i]
                self.data[locKey][valKey] = data['sss'][i]
                self.data[locKey][qcKey] = data['sss_qc'][i]
                self.data[locKey][errKey] = data['sss_err'][i]
            ncd.close()


def extract_date(filepath, pattern=r'_(\d{8}T\d{6})', date_format='%Y%m%dT%H%M%S'):
    filename = os.path.basename(filepath)
    match = re.compile(pattern).search(filename)
    if match:
        return datetime.strptime(match.group(1), date_format).replace(tzinfo=timezone.utc)
    raise ValueError(f"Could not extract date from filename: {filename}")


def get_range(date, window):
    dt_dic = {
        'PT6H': timedelta(hours=3),
        'PT12H': timedelta(hours=6),
        'PT24H': timedelta(hours=12),
    }
    if window not in dt_dic:
        raise ValueError(f'{window} window not defined')
    if isinstance(date, str):
        mid_date = datetime.strptime(date, '%Y%m%d%H').replace(tzinfo=timezone.utc)
    else:
        mid_date = date
    return mid_date - dt_dic[window], mid_date + dt_dic[window]


def get_files_in_date_range(base_dir: str, start_date: datetime, end_date: datetime):
    #all_files = np.array(sorted(glob(f'{base_dir}/*.h5')))
    #dates = np.array([extract_date(file) for file in all_files])
    #mask = (dates >= start_date) & (dates <= end_date)
    #return list(all_files[mask])
    """Find h5 files recursively in directory within specified date range.
    
    Args:
        base_dir: Directory path containing .h5 files (and subdirectories)
        start_date: Minimum file date (UTC)
        end_date: Maximum file date (UTC)
        
    Returns:
        List of sorted file paths matching date criteria
        
    Raises:
        ValueError: If no files are found at all, or if no files match the date range.
    """
    file_paths = Path(base_dir).rglob('*.h5')
    all_files = np.array(sorted(str(p) for p in file_paths))
    
    # Check if any files were found in the directory tree
    if all_files.size == 0:
        raise ValueError(f"No '.h5' files found in {base_dir} or its subdirectories.")
        
    dates = np.array([extract_date(file) for file in all_files])
    mask = (dates >= start_date) & (dates <= end_date)
    
    matched_files = list(all_files[mask])
    
    # Check if any files matched the specific date range mask
    if not matched_files:
        raise ValueError(
            f"No files found between {start_date.strftime('%Y-%m-%d')} "
            f"and {end_date.strftime('%Y-%m-%d')}."
        )
        
    return matched_files

def main():
    parser = argparse.ArgumentParser(
        description=(
            'Read JPL/RSS SMAP seaSurfaceSalinity (SSS) 2B h5 file(s) and convert'
            ' to a concatenated IODA formatted output file.'
        )
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help='directory of SMAP 2B h5 observations',
        type=str, required=True)
    required.add_argument(
        '-o', '--output',
        help='name of ioda output file',
        type=str, required=True)
    required.add_argument(
        '-d', '--date',
        help='base date for the center of the window',
        metavar='YYYYMMDDHH', type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '-w', '--window',
        help='assimilation window format -w PT24H',
        type=str,
        default='PT6H')
    args = parser.parse_args()

    start_date, end_date = get_range(args.date, args.window)
    file_list = get_files_in_date_range(args.input, start_date, end_date)
    sal = Salinity(file_list, start_date, end_date)

    ObsVars, Location = iconv.ExtractObsData(sal.data, locationKeyList)
    DimDict = {'Location': Location}
    writer = iconv.IodaWriter(args.output, locationKeyList, DimDict)

    VarAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
    VarAttrs[('dateTime', 'MetaData')]['units'] = iso8601_string
    VarAttrs[('seaSurfaceSalinity', 'ObsValue')]['units'] = 'PSU'
    VarAttrs[('seaSurfaceSalinity', 'ObsError')]['units'] = 'PSU'
    VarAttrs[('seaSurfaceSalinity', 'ObsValue')]['_FillValue'] = 999
    VarAttrs[('seaSurfaceSalinity', 'ObsError')]['_FillValue'] = 999
    VarAttrs[('seaSurfaceSalinity', 'PreQC')]['_FillValue'] = 999
    writer.BuildIoda(ObsVars, {'seaSurfaceSalinity': ['Location']}, VarAttrs, GlobalAttrs)


if __name__ == '__main__':
    main()
