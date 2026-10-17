#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
"""GPM IMERG half-hourly precipitation (3B-HHR, V07, Early, Late or Final run; NASA GES DISC HDF5) -> IODA.

Every 0.1 degree cell of each input file with a valid rate (the product flags missing cells negative; they are left out) is one
location: ObsValue/intensityOfPrecipitation is the product's 'precipitation' (mm/h, float32 as stored), ObsError the product's
'randomError', MetaData latitude and longitude the cell centre and dateTime the middle of the half hour (Grid/time_bnds).
Several files (e.g. the two half hours of an hour) go into one output file in the order given; a scorer rebuilds each half hour's
grid from the cell centres and dateTime.
Usage: imerg_2ioda.py OUTFILE FILE.HDF5 [FILE.HDF5 ...]"""
import sys, os, re, datetime as dt, numpy as np, h5py

if len(sys.argv) < 3:
    sys.exit(__doc__)
out, files = sys.argv[1], sys.argv[2:]
GPS0 = int((dt.datetime(1980, 1, 6) - dt.datetime(1970, 1, 1)).total_seconds())  # IMERG times: seconds since 1980-01-06 UTC

cols = {k: [] for k in ('lat', 'lon', 'time', 'rate', 'err')}
product = set()
for fn in files:
    with h5py.File(fn, 'r') as h:
        g = h['Grid']
        lat, lon = g['lat'][:], g['lon'][:]
        p = g['precipitation'][0].T  # (lon, lat) -> (lat, lon)
        e = g['randomError'][0].T
        t = int(GPS0 + g['time_bnds'][0].astype('i8').sum() // 2)
        hdr = h.attrs['FileHeader'].decode() if isinstance(h.attrs['FileHeader'], bytes) else str(h.attrs['FileHeader'])
    m = re.search(r'DOIshortName=([^;]*);', hdr)
    product.add(m.group(1) if m else 'IMERG')
    j, i = np.nonzero(p >= 0)
    cols['lat'].append(lat[j])
    cols['lon'].append(lon[i])
    cols['time'].append(np.full(j.size, t, 'i8'))
    cols['rate'].append(p[j, i])
    cols['err'].append(np.where(e[j, i] >= 0, e[j, i], np.float32(-999.0)))
    print(f'{os.path.basename(fn)}: {j.size} cells with a rate of {p.size}, {np.mean(p[j, i] > 0) * 100:.1f}% > 0, '
          f'{dt.datetime.utcfromtimestamp(t):%Y-%m-%dT%H:%M:%S}')
X = {k: np.concatenate(v) for k, v in cols.items()}
n = X['lat'].size
os.makedirs(os.path.dirname(os.path.abspath(out)), exist_ok=True)
with h5py.File(out, 'w') as h:
    h.attrs['_ioda_layout'] = np.bytes_('ObsGroup')
    h.attrs['_ioda_layout_version'] = np.int32(0)
    h.attrs['platform'] = np.bytes_('GPM')
    h.attrs['sensor'] = np.bytes_('IMERG')
    h.attrs['product'] = np.bytes_(';'.join(sorted(product)))
    h.attrs['source_files'] = np.bytes_(';'.join(os.path.basename(f) for f in files))
    h.attrs['converter'] = np.bytes_('imerg_2ioda.py')
    h.create_dataset('Location', data=np.arange(n, dtype='i8'), compression='gzip', compression_opts=4, shuffle=True, chunks=True)
    h['Location'].make_scale('Location')
    for g_ in ('MetaData', 'ObsValue', 'ObsError'):
        h.create_group(g_)

    def put(name, data, units=None, fill=None):
        ds = h.create_dataset(name, data=data, compression='gzip', compression_opts=4, shuffle=True, chunks=True)
        if units:
            ds.attrs['units'] = np.bytes_(units)
        if fill is not None:
            ds.attrs['_FillValue'] = np.array(fill, dtype=data.dtype)
        ds.dims[0].attach_scale(h['Location'])

    put('MetaData/latitude', X['lat'].astype('f4'), 'degrees_north', -999.0)
    put('MetaData/longitude', X['lon'].astype('f4'), 'degrees_east', -999.0)
    put('MetaData/dateTime', X['time'], 'seconds since 1970-01-01T00:00:00Z')
    put('ObsValue/intensityOfPrecipitation', X['rate'].astype('f4'), 'mm h-1', -999.0)
    put('ObsError/intensityOfPrecipitation', X['err'].astype('f4'), 'mm h-1', -999.0)
print(f'{out}: {n} locations from {len(files)} files')
