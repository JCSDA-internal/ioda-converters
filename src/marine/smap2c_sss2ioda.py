#!/usr/bin/env python3

#
# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

import os
import argparse
from pathlib import Path
from typing import Tuple, List
import numpy as np
from datetime import datetime, timedelta, timezone
import netCDF4 as nc
import re
import dateutil.parser
from glob import glob

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
epoch = datetime(1970, 1, 1, tzinfo=timezone.utc).replace(tzinfo=timezone.utc)


class Salinity(object):
    """Read and filter SMAP seaSurfaceSalinity observations from NetCDF files.
    
    Supports both JPL and RSS SMAP data sources. Filters observations by
    quality flags and applies temporal window filtering.
    """
    
    def __init__(self, filenames: List[str], start_date: datetime, end_date: datetime):
        """Initialize reader with input files and time window.
        
        Args:
            filenames: List of input NetCDF file paths
            start_date: Minimum observation time (UTC)
            end_date: Maximum observation time (UTC)
        """
        self.filenames = filenames
        self.start_date = start_date
        self.end_date = end_date
        self.data = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self._read()

    def _read(self) -> None:
        """Read and process observations from input files.
        
        Extracts salinity, latitude, longitude, time, error, and quality control
        flags from input files. Applies quality masks and temporal filtering.
        Handles both JPL and RSS SMAP data source formats.
        """
        valKey = vName, iconv.OvalName()
        errKey = vName, iconv.OerrName()
        qcKey = vName, iconv.OqcName()

        for f in self.filenames:
            print(" Reading file: ", f)
            try:
                ncd = nc.Dataset(f, 'r')
            except Exception as e:
                raise RuntimeError(f"Failed to open {f}: {e}")

            source = ncd.institution or ""
            if re.search(r"^Remote Sensing Systems.*", source) is None:
                raise ValueError(
                    f"Unsupported file source '{source}' for '{f}'. This converter expects RSS SMAP 2C nc files. "
                    "Use smap2_sss2ioda.py for JPL 2B files."
                )

            source = 'RSS'
            source_var_name = {
                'time': 'time',
                'lat': 'cellat',
                'lon': 'cellon',
                'sss': 'sss_smap',
                'sss_qc': 'iqc_flag',
            }

            if ncd.processing_level[:2] != 'L2':
                raise ValueError("Error: only L2 files handled for now.")

            # Extract base time from units string (format: "seconds since YYYY-MM-DDTHH:MM:SSZ")
            time_units = ncd.variables[source_var_name['time']].units
            basetime_str = time_units.split('since ')[-1]
            try:
                basetime = dateutil.parser.parse(basetime_str).replace(tzinfo=timezone.utc).replace(tzinfo=timezone.utc)
            except:
                basetime = datetime.strptime(basetime_str, "%Y-%m-%d %H:%M:%S %f")
            data = {}
            for v in source_var_name:
                if v == 'sss_qc':
                    data[v] = ncd.variables[source_var_name[v]][:].flatten().astype(np.int32)
                # elif v == 'sss':
                #     data[v] = np.nanmean(ncd.variables[source_var_name[v]][:], axis=-1).flatten()
                else:
                    data[v] = ncd.variables[source_var_name[v]][:].flatten()

            # Build a valid-data mask using finiteness of the averaged sss values
            sss_arr = np.array(data['sss'])
            valid_mask = np.isfinite(sss_arr)
            for v in source_var_name:
                data[v] = np.array(data[v])[valid_mask]

            for i in range(len(data['time'])):
                obs_date = basetime + timedelta(seconds=float(data['time'][i]))
                obs_date = obs_date.replace(tzinfo=timezone.utc)
                if obs_date < self.start_date or obs_date > self.end_date:
                    continue
                time_offset = round((obs_date - epoch).total_seconds())
                locKey = data['lat'][i], data['lon'][i], time_offset
                self.data[locKey][valKey] = data['sss'][i]
                self.data[locKey][qcKey] = data['sss_qc'][i]
                if 'sss_err' in data:
                    self.data[locKey][errKey] = data['sss_err'][i]
                else:
                    self.data[locKey][errKey] = 1.0
            ncd.close()


def extract_date(filepath: str, pattern: str = r'_(\d{8}T\d{6})', 
                 date_format: str = '%Y%m%dT%H%M%S') -> datetime:
    """Extract date from filename using regex pattern.
    
    Args:
        filepath: Full path or filename to parse
        pattern: Regex pattern to match date string
        date_format: strptime format for parsing matched date
        
    Returns:
        datetime: Parsed date with UTC timezone
        
    Raises:
        ValueError: If date cannot be extracted from filename
    """
    filename = os.path.basename(filepath)
    match = re.compile(pattern).search(filename)
    if match:
        return datetime.strptime(match.group(1), date_format).replace(tzinfo=timezone.utc)
    raise ValueError(f"Could not extract date from filename: {filename}")


def get_range(date: str | datetime, window: str) -> Tuple[datetime, datetime]:
    """Calculate start and end times for assimilation window.
    
    Args:
        date: Center date as string (YYYYMMDDHH) or datetime object
        window: Window size (PT6H, PT12H, or PT24H)
        
    Returns:
        Tuple of (start_date, end_date) as UTC datetime objects
        
    Raises:
        ValueError: If window size is not defined
    """
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


def get_files_in_date_range(base_dir: str, start_date: datetime, end_date: datetime) -> List[str]:
    """Find NetCDF files in directory within specified date range.
    
    Args:
        base_dir: Directory path containing .nc files
        start_date: Minimum file date (UTC)
        end_date: Maximum file date (UTC)
        
    Returns:
        List of sorted file paths matching date criteria
    """
    file_paths = Path(base_dir).rglob('*.nc')
    all_files = np.array(sorted(str(p) for p in file_paths))
    #all_files = np.array(sorted(glob(f'{base_dir}/*.nc')))
    dates = np.array([extract_date(file) for file in all_files])
    mask = (dates >= start_date) & (dates <= end_date)
    matched_files = list(all_files[mask])
    # Check if any files matched the specific date range mask
    if not matched_files:
        raise ValueError(
            f"Check the provied path. This converter is from SMAP nc files. "
            f"For SMAP h5 files use different converter.\n "
            f"No files found between {start_date.strftime('%Y-%m-%d')} "
            f"and {end_date.strftime('%Y-%m-%d')}."
        )
        
    return matched_files

def main():
    parser = argparse.ArgumentParser(
        description=(
            'Read JPL/RSS SMAP seaSurfaceSalinity (SSS) 2C nc file(s) and convert'
            ' to a concatenated IODA formatted output file.'
        )
    )
    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help='directory of SMAP 2C nc observations',
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

    # Validate input directory exists
    input_dir = Path(args.input)
    if not input_dir.exists():
        raise FileNotFoundError(f"Input directory not found: {args.input}")
    if not input_dir.is_dir():
        raise NotADirectoryError(f"Input path is not a directory: {args.input}")

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
