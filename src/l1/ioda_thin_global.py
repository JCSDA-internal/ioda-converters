#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Time window and global equal-area thinning of an IODA (HDF5) radiance file: one location per D-km
# box, nearest the box centre, then nearest the analysis time (src/satpy/thin_global.py). Every
# Location-dimensioned variable is subset, in input order; Channel variables and attributes are copied.
# --km 0 applies only the window and the box.
#
# Usage: ioda_thin_global.py IN.nc4 OUT.nc4 --km D [--t0 YYYY-mm-ddTHH:MM --t1 YYYY-mm-ddTHH:MM]
#                            [--tref YYYY-mm-ddTHH:MM] [--box S N W E]
#

import argparse
import datetime as dt
import os
import sys

import h5py
import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'satpy'))
from thin_global import thin_index


def epoch(s):
    return (dt.datetime.fromisoformat(s.replace('Z', '')) - dt.datetime(1970, 1, 1)).total_seconds()


def main():
    ap = argparse.ArgumentParser(description='Time window and global equal-area thinning of an IODA radiance file')
    ap.add_argument('inp')
    ap.add_argument('out')
    ap.add_argument('--km', type=float, required=True)
    ap.add_argument('--t0')
    ap.add_argument('--t1')
    ap.add_argument('--tref')
    ap.add_argument('--box', nargs=4, type=float)
    a = ap.parse_args()

    f = h5py.File(a.inp, 'r')
    nloc = f['Location'].shape[0]
    la = f['MetaData/latitude'][:].astype('f8')
    lo = f['MetaData/longitude'][:].astype('f8')
    t = f['MetaData/dateTime'][:].astype('f8') if 'MetaData/dateTime' in f else None
    ok = np.isfinite(la) & (np.abs(la) <= 90) & (np.abs(lo) <= 360)
    if t is not None and a.t0:
        ok &= t >= epoch(a.t0)
    if t is not None and a.t1:
        ok &= t < epoch(a.t1)
    if a.box:
        ok &= (la >= a.box[0]) & (la <= a.box[1]) & (lo >= a.box[2]) & (lo <= a.box[3])
    tref = epoch(a.tref) if a.tref else None
    idx = thin_index(la, lo, a.km, t, tref, ok) if a.km > 0 else np.nonzero(ok)[0]

    g = h5py.File(a.out, 'w')
    for k, v in f.attrs.items():
        g.attrs[k] = v
    g.create_dataset('Location', data=np.arange(len(idx), dtype='i4'))
    g['Location'].make_scale('Location')
    if 'Channel' in f:
        g.create_dataset('Channel', data=f['Channel'][:])
        g['Channel'].make_scale('Channel')
    nch = g['Channel'].shape[0] if 'Channel' in g else -1

    def copy(name, obj):
        if isinstance(obj, h5py.Group):
            gg = g.require_group(name)
            for k, v in obj.attrs.items():
                gg.attrs[k] = v
            return
        if name in ('Location', 'Channel'):
            return
        parent = g.require_group(name.rsplit('/', 1)[0]) if '/' in name else g
        chanvar = name.startswith('MetaData/sensorChannel') or name.startswith('MetaData/sensorCentral')
        isloc = bool(obj.shape) and obj.shape[0] == nloc and not chanvar
        data = obj[...][idx] if isloc else obj[...]
        ds = parent.create_dataset(name.rsplit('/', 1)[-1], data=data, dtype=obj.dtype,
                                   **({'compression': 'gzip', 'compression_opts': 1} if data.size > 1000 else {}))
        for k, v in obj.attrs.items():
            if k not in ('DIMENSION_LIST', 'REFERENCE_LIST', 'CLASS', 'NAME'):
                ds.attrs[k] = v
        if isloc:
            ds.dims[0].attach_scale(g['Location'])
        elif obj.shape and obj.shape[0] == nch:
            ds.dims[0].attach_scale(g['Channel'])
        if ds.ndim == 2 and ds.shape[1] == nch:
            ds.dims[1].attach_scale(g['Channel'])

    f.visititems(copy)
    g.close()
    print(f"{os.path.basename(a.inp)}: {np.count_nonzero(ok)} of {nloc} in window/box, kept {len(idx)}"
          + (f" at {a.km:g} km" if a.km > 0 else '') + f" -> {a.out}")


if __name__ == '__main__':
    main()
