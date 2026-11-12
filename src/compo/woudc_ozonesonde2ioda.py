#!/usr/bin/env python3

#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#

"""
Convert WOUDC ozonesonde files (extended CSV, form 1 and form 2) to IODA.

WOUDC (World Ozone and Ultraviolet Radiation Data Centre) archives ozonesonde
profiles in its extended CSV ("extCSV") format: plain text holding several
tables, each starting with a ``#TABLE_NAME`` line, one header row and data
rows. A small parser is included here so that no WOUDC Python package is
needed.

Output: one IODA location per profile level.

- ``ObsValue/ozoneInsitu``: ozone mole fraction [mol mol-1], computed as
  O3PartialPressure [mPa] / Pressure [hPa] * 1e-5.
- ``ObsError/ozoneInsitu``: from the PROFILE_UNCERTAINTY table when present,
  otherwise a relative error (see the ``--rel-error-*`` options) with a floor.
- ``MetaData``: latitude, longitude, dateTime (launch time + Duration),
  pressure [Pa], height [m] (Height when present, otherwise GPHeight),
  stationIdentification (WOUDC platform type and ID, e.g. ``STN076``),
  releaseTime (UTC launch time, unique per profile together with
  stationIdentification), instrumentIdentifier (integer code, see
  ``INSTRUMENT_CODES``), airTemperature [K] and relativeHumidity [1].

Levels with missing or non-positive pressure or ozone, the descent part of
the flight, repeated pressure levels and levels above ``--top-pressure`` are
removed. When the same station and launch time is found in several files, the
file with the highest DATA_GENERATION Version (then the latest Date) is kept.
A profile is kept if its launch time is in (window begin, window end]; all of
its levels are kept, even those measured after the window end.

Usage::

    woudc_ozonesonde2ioda.py -i <file1.csv> [<file2.csv> ...] -o out.nc4
        [--window-begin 2025-03-01T03:00:00Z --window-end 2025-03-01T09:00:00Z]
"""

import argparse
import csv
import os
import sys
from collections import defaultdict, OrderedDict
from datetime import datetime, timedelta, timezone

import numpy as np

import pyiodaconv.ioda_conv_engines as iconv
from pyiodaconv.orddicts import DefaultOrderedDict
from pyiodaconv.def_jedi_utils import epoch, iso8601_string


# Unit conversions
MPA_PER_HPA_TO_MOLMOL = 1.0e-5   # [mPa] / [hPa] -> [mol mol-1]
HPA2PA = 1.0e2
CELSIUS2KELVIN = 273.15
PERCENT2FRACTION = 1.0e-2
PPBV2MOLMOL = 1.0e-9

# Plausibility limits used to detect files whose units differ from the WOUDC
# definitions (hPa, mPa, degC). A file outside these limits stops the run.
MAX_PRESSURE_HPA = 1100.0
MAX_O3_PARTIAL_PRESSURE_MPA = 50.0
MAX_TEMPERATURE_C = 70.0

varname_ozone = 'ozoneInsitu'

# MetaData/instrumentIdentifier codes (instrument Name in #INSTRUMENT).
INSTRUMENT_CODES = OrderedDict([
    ('OTHER', 0),
    ('ECC', 1),
    ('BREWER-MAST', 2),
    ('CARBON-IODINE', 3),
])

locationKeyList = [
    ("latitude", "float", "degrees_north"),
    ("longitude", "float", "degrees_east"),
    ("pressure", "float", "Pa"),
    ("dateTime", "long", iso8601_string),
]

VarDims = {
    varname_ozone: ['Location'],
}

AttrData = {
    'converter': os.path.basename(__file__),
}

float_missing_value = iconv.get_default_fill_val(np.float32)
int_missing_value = iconv.get_default_fill_val(np.int32)
long_missing_value = iconv.get_default_fill_val(np.int64)

metaDataName = iconv.MetaDataName()
obsValName = iconv.OvalName()
obsErrName = iconv.OerrName()
qcName = iconv.OqcName()


class ExtCsvUnitError(Exception):
    """Raised when the values of a file contradict the WOUDC units."""


# ----------------------------------------------------------------------------
# extCSV parsing
# ----------------------------------------------------------------------------

def read_extcsv(filename):
    """Read a WOUDC extCSV file into its tables.

    Lines starting with ``*`` are comments and blank lines are skipped. Fields
    are stripped of surrounding blanks; empty fields stay empty strings
    (missing values).

    Args:
        filename: path of the extCSV file.

    Returns:
        dict mapping the upper-case table name (without ``#``) to the list of
        its occurrences, in file order. Each occurrence is a dict with keys
        ``header`` (list of field names) and ``rows`` (list of lists of str).
    """
    tables = defaultdict(list)
    current = None
    with open(filename, 'r', encoding='utf-8-sig', errors='replace') as fh:
        for raw in fh:
            line = raw.strip()
            if not line or line.startswith('*'):
                continue
            if line.startswith('#'):
                name = line[1:].split(',')[0].strip().upper()
                current = {'header': None, 'rows': []}
                tables[name].append(current)
                continue
            if current is None:
                continue
            fields = [f.strip() for f in next(csv.reader([line], skipinitialspace=True))]
            if current['header'] is None:
                while fields and not fields[-1]:
                    fields.pop()
                current['header'] = fields
            else:
                current['rows'].append(fields)
    return tables


def _column_index(table, name):
    """Return the index of field ``name`` (case-insensitive) in a table header, or None."""
    for i, field in enumerate(table['header'] or []):
        if field.lower() == name.lower():
            return i
    return None


def first_row_value(tables, table_name, field):
    """Return the value of ``field`` in the first data row of the first
    ``table_name`` table, or '' if the table, field or row is missing."""
    if not tables.get(table_name):
        return ''
    table = tables[table_name][0]
    idx = _column_index(table, field)
    if idx is None or not table['rows'] or idx >= len(table['rows'][0]):
        return ''
    return table['rows'][0][idx]


def _to_float(value):
    """Convert a field to float; empty or unparsable fields become NaN (never zero)."""
    try:
        return float(value) if value != '' else np.nan
    except ValueError:
        return np.nan


def column_as_float(table, name):
    """Return column ``name`` of ``table`` as a float64 array (NaN = missing),
    or None if the table has no such column."""
    idx = _column_index(table, name)
    if idx is None:
        return None
    return np.array([_to_float(row[idx]) if idx < len(row) else np.nan
                     for row in table['rows']], dtype=np.float64)


def parse_utc_offset(value):
    """Parse a WOUDC UTCOffset (``+HH:MM:SS``, ``-HH:MM``...) into a timedelta."""
    value = value.strip()
    if not value:
        raise ValueError('empty UTCOffset')
    sign = -1 if value[0] == '-' else 1
    parts = [float(p) for p in value.lstrip('+-').split(':')]
    parts += [0.0] * (3 - len(parts))
    return sign * timedelta(hours=parts[0], minutes=parts[1], seconds=parts[2])


def parse_launch_time(tables):
    """Return the UTC launch time from the first #TIMESTAMP table.

    UTC = local time - UTCOffset. Raises ValueError if Date, Time or
    UTCOffset is missing or malformed.
    """
    date = first_row_value(tables, 'TIMESTAMP', 'Date')
    time = first_row_value(tables, 'TIMESTAMP', 'Time')
    offset = first_row_value(tables, 'TIMESTAMP', 'UTCOffset')
    if not date or not time:
        raise ValueError('missing launch Date or Time in #TIMESTAMP')
    hms = [float(p) for p in time.split(':')]
    hms += [0.0] * (3 - len(hms))
    local = datetime.strptime(date, '%Y-%m-%d') + timedelta(hours=hms[0], minutes=hms[1], seconds=hms[2])
    return (local - parse_utc_offset(offset)).replace(tzinfo=timezone.utc)


# ----------------------------------------------------------------------------
# One ozonesonde profile
# ----------------------------------------------------------------------------

class SondeProfile(object):
    """One ozonesonde flight read from a WOUDC extCSV file.

    Only the #PROFILE table (and #PROFILE_UNCERTAINTY when present) is used
    for profile data; #PRELAUNCH, #DESELECTED_DATA and the other optional
    form-2 tables are ignored.

    Args:
        filename: path of the extCSV file.

    Raises:
        ValueError: the file is not a usable ozonesonde file (it is skipped).
        ExtCsvUnitError: the values contradict the WOUDC units (the run stops).
    """

    def __init__(self, filename):
        self.filename = filename
        tables = read_extcsv(filename)

        category = first_row_value(tables, 'CONTENT', 'Category')
        if category.lower() != 'ozonesonde':
            raise ValueError(f'#CONTENT Category is "{category}", not OzoneSonde')
        try:
            self.form = int(float(first_row_value(tables, 'CONTENT', 'Form')))
        except ValueError:
            raise ValueError('missing or invalid #CONTENT Form')
        if self.form not in (1, 2):
            raise ValueError(f'unsupported #CONTENT Form {self.form}')

        for name in ('PLATFORM', 'LOCATION', 'TIMESTAMP', 'PROFILE'):
            if not tables.get(name):
                raise ValueError(f'missing #{name} table')

        self.platform_type = first_row_value(tables, 'PLATFORM', 'Type').upper()
        self.platform_id = first_row_value(tables, 'PLATFORM', 'ID')
        if not self.platform_type or not self.platform_id:
            raise ValueError('missing #PLATFORM Type or ID')
        self.station = f'{self.platform_type}{self.platform_id}'

        self.instrument = first_row_value(tables, 'INSTRUMENT', 'Name')
        self.instrument_code = INSTRUMENT_CODES.get(self.instrument.upper(), INSTRUMENT_CODES['OTHER'])

        self.version = _to_float(first_row_value(tables, 'DATA_GENERATION', 'Version'))
        self.generation_date = first_row_value(tables, 'DATA_GENERATION', 'Date')

        self.launch_time = parse_launch_time(tables)
        self.launch_lat = _to_float(first_row_value(tables, 'LOCATION', 'Latitude'))
        self.launch_lon = _to_float(first_row_value(tables, 'LOCATION', 'Longitude'))
        self.launch_height = _to_float(first_row_value(tables, 'LOCATION', 'Height'))

        # Flight summary (Dobson units): kept as information only.
        self.flight_summary = {
            k: first_row_value(tables, 'FLIGHT_SUMMARY', k)
            for k in ('IntegratedO3', 'SondeTotalO3', 'CorrectionCode', 'CorrectionFactor', 'NormalizationFactor')}

        if len(tables['PROFILE']) > 1:
            print(f'  WARNING: {filename}: {len(tables["PROFILE"])} #PROFILE tables, using the first one')
        profile = tables['PROFILE'][0]
        self.columns = {}
        for name in ('Pressure', 'O3PartialPressure', 'Temperature', 'Duration', 'GPHeight',
                     'RelativeHumidity', 'Latitude', 'Longitude', 'Height'):
            self.columns[name] = column_as_float(profile, name)
        if self.columns['Pressure'] is None or self.columns['O3PartialPressure'] is None:
            raise ValueError('#PROFILE has no Pressure or O3PartialPressure column')
        self.nlevels_in_file = len(profile['rows'])

        # Per-level ozone uncertainty (form 2, optional); rows must match #PROFILE.
        # A table with a header but no rows is treated as absent.
        self.o3_uncertainty = None
        if tables.get('PROFILE_UNCERTAINTY') and tables['PROFILE_UNCERTAINTY'][0]['rows']:
            unc = column_as_float(tables['PROFILE_UNCERTAINTY'][0], 'O3PartialPressure')
            if unc is not None and len(unc) == self.nlevels_in_file:
                self.o3_uncertainty = unc
            else:
                nrows = 'no O3PartialPressure column' if unc is None else f'{len(unc)} rows'
                print(f'  {filename}: #PROFILE_UNCERTAINTY ({nrows}) does not match #PROFILE '
                      f'({self.nlevels_in_file} rows), default ozone errors used')

        self._check_units()

    def _check_units(self):
        """Stop if the values contradict the WOUDC units (hPa, mPa, degC)."""
        p = self.columns['Pressure']
        o3 = self.columns['O3PartialPressure']
        t = self.columns['Temperature']
        problems = []
        if np.any(p[np.isfinite(p)] > MAX_PRESSURE_HPA):
            problems.append(f'Pressure up to {np.nanmax(p)} (expected hPa, <= {MAX_PRESSURE_HPA})')
        if np.any(o3[np.isfinite(o3)] > MAX_O3_PARTIAL_PRESSURE_MPA):
            problems.append(f'O3PartialPressure up to {np.nanmax(o3)} (expected mPa, <= {MAX_O3_PARTIAL_PRESSURE_MPA})')
        if t is not None and np.any(t[np.isfinite(t)] > MAX_TEMPERATURE_C):
            problems.append(f'Temperature up to {np.nanmax(t)} (expected degC, <= {MAX_TEMPERATURE_C})')
        if problems:
            raise ExtCsvUnitError(f'{self.filename}: values contradict WOUDC units: ' + '; '.join(problems))

    @property
    def key(self):
        """De-duplication key: station and UTC launch time."""
        return (self.station, self.launch_time)

    @property
    def priority(self):
        """Sort key for duplicates: highest Version, then latest generation Date."""
        version = self.version if np.isfinite(self.version) else -np.inf
        return (version, self.generation_date)

    def level_mask(self, top_pressure_hpa):
        """Return the indices of the #PROFILE rows kept, and log each filter.

        Filters, in order: missing or non-positive pressure or ozone; descent
        (rows after the minimum pressure); repeated pressure levels (the first
        occurrence is kept); levels with pressure below ``top_pressure_hpa``.
        """
        p = self.columns['Pressure']
        o3 = self.columns['O3PartialPressure']
        idx = np.arange(len(p))
        counts = OrderedDict()

        good = np.isfinite(p) & (p > 0) & np.isfinite(o3) & (o3 > 0)
        counts['missing/non-positive'] = int(np.sum(~good))
        idx = idx[good]

        if idx.size:
            last = int(np.argmin(p[idx]))
            counts['descent'] = idx.size - last - 1
            idx = idx[:last + 1]
        else:
            counts['descent'] = 0

        _, first = np.unique(p[idx], return_index=True)
        keep = np.zeros(idx.size, dtype=bool)
        keep[first] = True
        counts['duplicate pressure'] = int(np.sum(~keep))
        idx = idx[keep]

        above = p[idx] < top_pressure_hpa
        counts[f'above {top_pressure_hpa:g} hPa'] = int(np.sum(above))
        idx = idx[~above]

        removed = ', '.join(f'{v} {k}' for k, v in counts.items())
        print(f'  {self.station} {self.launch_time:%Y-%m-%dT%H:%M:%SZ} ({os.path.basename(self.filename)}): '
              f'{self.nlevels_in_file} levels, removed {removed}; kept {idx.size}')
        return idx

    def levels(self, args):
        """Return a dict of per-level arrays for the levels kept.

        Keys: ozone, error [mol mol-1]; pressure [hPa]; latitude, longitude;
        dateTime [s since epoch]; height [m]; airTemperature [K];
        relativeHumidity [1]. Missing values are NaN.
        """
        idx = self.level_mask(args.top_pressure)
        c = self.columns
        n = idx.size
        p = c['Pressure'][idx]
        out = {'pressure': p}
        out['ozone'] = c['O3PartialPressure'][idx] / p * MPA_PER_HPA_TO_MOLMOL

        # Error: reported uncertainty when available, otherwise relative error with a floor.
        rel = np.where(p > args.error_pressure_threshold, args.rel_error_lower, args.rel_error_upper)
        default_err = np.maximum(rel * out['ozone'], args.error_floor * PPBV2MOLMOL)
        if self.o3_uncertainty is not None:
            unc = self.o3_uncertainty[idx] / p * MPA_PER_HPA_TO_MOLMOL
            use = np.isfinite(unc) & (unc > 0)
            out['error'] = np.where(use, unc, default_err)
        else:
            out['error'] = default_err

        # Position: per-level balloon position when present (form 2), otherwise the launch site.
        for name, launch_value in (('Latitude', self.launch_lat), ('Longitude', self.launch_lon)):
            values = np.full(n, launch_value)
            if c[name] is not None:
                level_values = c[name][idx]
                values = np.where(np.isfinite(level_values), level_values, values)
            out[name.lower()] = values

        # Time: launch time + Duration (launch time when Duration is missing).
        launch = (self.launch_time - epoch).total_seconds()
        duration = c['Duration'][idx] if c['Duration'] is not None else np.full(n, np.nan)
        out['dateTime'] = np.rint(launch + np.where(np.isfinite(duration), duration, 0.0))

        # Height: Height when present (form 2), otherwise GPHeight.
        height = np.full(n, np.nan)
        if c['GPHeight'] is not None:
            height = c['GPHeight'][idx]
        if c['Height'] is not None:
            height = np.where(np.isfinite(c['Height'][idx]), c['Height'][idx], height)
        out['height'] = height

        out['airTemperature'] = (c['Temperature'][idx] + CELSIUS2KELVIN
                                 if c['Temperature'] is not None else np.full(n, np.nan))
        out['relativeHumidity'] = (c['RelativeHumidity'][idx] * PERCENT2FRACTION
                                   if c['RelativeHumidity'] is not None else np.full(n, np.nan))

        if args.layer_average:
            out = layer_average(out, args.layer_edges)
        return out


def layer_average(levels, edges_hpa):
    """Average levels into fixed pressure layers.

    Pressure is averaged in log space; ozone, error and the other fields are
    arithmetic means of the levels in the layer (errors are not reduced, as
    level errors in one layer are strongly correlated). Empty layers are
    dropped.

    Args:
        levels: dict of per-level arrays, as returned by SondeProfile.levels.
        edges_hpa: layer edges [hPa], any order.

    Returns:
        dict with the same keys, one value per non-empty layer.
    """
    edges = np.sort(np.asarray(edges_hpa, dtype=np.float64))
    layer = np.digitize(levels['pressure'], edges)
    inside = (layer > 0) & (layer < len(edges))
    out = {k: [] for k in levels}
    for ilayer in np.unique(layer[inside]):
        sel = layer == ilayer
        for k, v in levels.items():
            values = v[sel]
            if k == 'pressure':
                out[k].append(np.exp(np.mean(np.log(values))))
            elif np.any(np.isfinite(values)):
                out[k].append(np.nanmean(values))
            else:
                out[k].append(np.nan)
    out = {k: np.asarray(v, dtype=np.float64) for k, v in out.items()}
    out['dateTime'] = np.rint(out['dateTime'])
    return out


# ----------------------------------------------------------------------------
# Converter
# ----------------------------------------------------------------------------

class woudc_ozonesonde(object):
    """Read WOUDC ozonesonde files and build the IODA output dictionaries.

    Args:
        args: parsed command-line arguments (see ``get_parser``).
    """

    def __init__(self, args):
        self.args = args
        self.outdata = defaultdict(lambda: DefaultOrderedDict(OrderedDict))
        self.varAttrs = DefaultOrderedDict(lambda: DefaultOrderedDict(dict))
        self.DimDict = {}
        self._setVarAttrs()
        self._read()

    def _setVarAttrs(self):
        """Units and fill values of the output variables."""
        for group in (obsValName, obsErrName):
            self.varAttrs[(varname_ozone, group)]['units'] = 'mol mol-1'
            self.varAttrs[(varname_ozone, group)]['_FillValue'] = float_missing_value
        self.varAttrs[(varname_ozone, qcName)]['_FillValue'] = int_missing_value
        units = {'latitude': 'degrees_north', 'longitude': 'degrees_east', 'pressure': 'Pa',
                 'height': 'm', 'airTemperature': 'K', 'relativeHumidity': '1',
                 'dateTime': iso8601_string, 'releaseTime': iso8601_string}
        for name, unit in units.items():
            self.varAttrs[(name, metaDataName)]['units'] = unit
        for name in ('latitude', 'longitude', 'pressure', 'height', 'airTemperature', 'relativeHumidity'):
            self.varAttrs[(name, metaDataName)]['_FillValue'] = float_missing_value
        for name in ('dateTime', 'releaseTime'):
            self.varAttrs[(name, metaDataName)]['_FillValue'] = long_missing_value
        self.varAttrs[('instrumentIdentifier', metaDataName)]['long_name'] = \
            'sonde type: ' + ', '.join(f'{v}={k}' for k, v in INSTRUMENT_CODES.items())

    def _read_profiles(self):
        """Read all input files, de-duplicate and keep the profiles in the window."""
        profiles = {}
        for filename in self.args.input:
            try:
                profile = SondeProfile(filename)
            except ExtCsvUnitError as exc:
                sys.exit(f'ERROR: {exc}. Stopping: check the units of this file.')
            except (ValueError, IndexError, OSError) as exc:
                print(f'Skipping {filename}: {exc}')
                continue
            known = profiles.get(profile.key)
            if known is None or profile.priority > known.priority:
                if known is not None:
                    print(f'Duplicate {profile.station} {profile.launch_time:%Y-%m-%dT%H:%M:%SZ}: '
                          f'using {profile.filename} instead of {known.filename}')
                profiles[profile.key] = profile
            else:
                print(f'Duplicate {profile.station} {profile.launch_time:%Y-%m-%dT%H:%M:%SZ}: '
                      f'keeping {known.filename}, ignoring {profile.filename}')

        selected = [p for p in profiles.values()
                    if self.args.window_begin < p.launch_time <= self.args.window_end]
        print(f'{len(selected)} of {len(profiles)} profile(s) launched in the window '
              f'({self.args.window_begin:%Y-%m-%dT%H:%M:%SZ}, {self.args.window_end:%Y-%m-%dT%H:%M:%SZ}]')
        return sorted(selected, key=lambda p: p.key)

    def _read(self):
        """Fill self.outdata with the levels of all selected profiles."""
        columns = defaultdict(list)
        for profile in self._read_profiles():
            levels = profile.levels(self.args)
            n = levels['pressure'].size
            if n == 0:
                print(f'  {profile.station} {profile.launch_time:%Y-%m-%dT%H:%M:%SZ}: no valid level, skipped')
                continue
            for k, v in levels.items():
                columns[k].append(v)
            columns['stationIdentification'].append(np.full(n, profile.station, dtype=object))
            columns['releaseTime'].append(np.full(n, (profile.launch_time - epoch).total_seconds()))
            columns['instrumentIdentifier'].append(np.full(n, profile.instrument_code))

        if not columns:
            print('No WOUDC ozonesonde profile in the window: no output file written')
            sys.exit(0)

        data = {k: np.concatenate(v) for k, v in columns.items()}
        nlocs = data['pressure'].size

        def as_float32(values):
            return np.where(np.isfinite(values), values, float_missing_value).astype(np.float32)

        self.outdata[(varname_ozone, obsValName)] = as_float32(data['ozone'])
        self.outdata[(varname_ozone, obsErrName)] = as_float32(data['error'])
        self.outdata[(varname_ozone, qcName)] = np.zeros(nlocs, dtype=np.int32)
        self.outdata[('latitude', metaDataName)] = as_float32(data['latitude'])
        self.outdata[('longitude', metaDataName)] = as_float32(data['longitude'])
        self.outdata[('pressure', metaDataName)] = as_float32(data['pressure'] * HPA2PA)
        self.outdata[('height', metaDataName)] = as_float32(data['height'])
        self.outdata[('airTemperature', metaDataName)] = as_float32(data['airTemperature'])
        self.outdata[('relativeHumidity', metaDataName)] = as_float32(data['relativeHumidity'])
        self.outdata[('dateTime', metaDataName)] = data['dateTime'].astype(np.int64)
        self.outdata[('releaseTime', metaDataName)] = data['releaseTime'].astype(np.int64)
        self.outdata[('stationIdentification', metaDataName)] = data['stationIdentification']
        self.outdata[('instrumentIdentifier', metaDataName)] = data['instrumentIdentifier'].astype(np.int32)

        self.DimDict['Location'] = nlocs
        print(f'{nlocs} location(s) from {len(columns["pressure"])} profile(s)')


# ----------------------------------------------------------------------------
# Command line
# ----------------------------------------------------------------------------

def _parse_window_bound(value):
    """Parse an ISO 8601 window bound (e.g. 2025-03-01T09:00:00Z) as UTC."""
    return datetime.strptime(value, "%Y-%m-%dT%H:%M:%SZ").replace(tzinfo=timezone.utc)


def get_parser():
    """Return the command-line parser of this converter."""
    parser = argparse.ArgumentParser(
        description=(
            'Reads WOUDC ozonesonde extended CSV files (form 1 and 2) and '
            'converts them into one IODA file with one location per level. '
            'Multiple files are able to be concatenated.'),
        formatter_class=argparse.ArgumentDefaultsHelpFormatter)

    required = parser.add_argument_group(title='required arguments')
    required.add_argument(
        '-i', '--input',
        help="path(s) of WOUDC extCSV input file(s); file names are not used for metadata",
        type=str, nargs='+', required=True)
    required.add_argument(
        '-o', '--output',
        help="path of IODA output file",
        type=str, required=True)

    optional = parser.add_argument_group(title='optional arguments')
    optional.add_argument(
        '--window-begin',
        help="assimilation window start time, ISO8601 format (e.g. 2025-03-01T03:00:00Z); "
             "profiles launched at or before this time are excluded",
        type=_parse_window_bound, default=datetime(1900, 1, 1, tzinfo=timezone.utc), dest='window_begin')
    optional.add_argument(
        '--window-end',
        help="assimilation window end time, ISO8601 format (e.g. 2025-03-01T09:00:00Z); "
             "profiles launched up to and including this time are kept",
        type=_parse_window_bound, default=datetime(2200, 1, 1, tzinfo=timezone.utc), dest='window_end')
    optional.add_argument(
        '--top-pressure',
        help="levels with a pressure lower than this [hPa] are removed",
        type=float, default=5.0, dest='top_pressure')
    optional.add_argument(
        '--rel-error-lower',
        help="relative ozone error for levels with pressure > --error-pressure-threshold "
             "(used when the file has no PROFILE_UNCERTAINTY)",
        type=float, default=0.05, dest='rel_error_lower')
    optional.add_argument(
        '--rel-error-upper',
        help="relative ozone error for levels with pressure <= --error-pressure-threshold",
        type=float, default=0.10, dest='rel_error_upper')
    optional.add_argument(
        '--error-pressure-threshold',
        help="pressure [hPa] separating --rel-error-lower and --rel-error-upper",
        type=float, default=10.0, dest='error_pressure_threshold')
    optional.add_argument(
        '--error-floor',
        help="minimum ozone error [ppbv] when the relative error is used",
        type=float, default=2.0, dest='error_floor')
    optional.add_argument(
        '--layer-average',
        help="average the levels into the pressure layers given by --layer-edges",
        action='store_true', dest='layer_average')
    optional.add_argument(
        '--layer-edges',
        help="pressure layer edges [hPa] for --layer-average "
             "(default: 40 layers evenly spaced in log pressure from 1100 to 5 hPa)",
        type=float, nargs='+', default=list(np.geomspace(1100.0, 5.0, 41)), dest='layer_edges')

    return parser


def main():

    args = get_parser().parse_args()
    if args.window_end <= args.window_begin:
        sys.exit('ERROR: --window-end must be after --window-begin')

    sondes = woudc_ozonesonde(args)

    # setup the IODA writer
    writer = iconv.IodaWriter(args.output, locationKeyList, sondes.DimDict)

    # write everything out
    print("Writing: {}".format(args.output))
    writer.BuildIoda(sondes.outdata, VarDims, sondes.varAttrs, AttrData)


if __name__ == '__main__':
    main()
