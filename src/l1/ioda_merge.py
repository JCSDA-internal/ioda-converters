#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Merge IODA (HDF5) files along the Location dimension, in the order given. Variables dimensioned
# only by Channel (e.g. sensorChannelNumber) are copied from the first file; attributes and group
# structure are kept. float64 variables become float32 (IODA rejects float64 MetaData, e.g. the
# ATMS converter's sensorViewAngle).
#
# Usage: ioda_merge.py OUT.nc4 IN1.nc4 [IN2.nc4 ...]
#

import sys

import h5py
import numpy as np


def main():
    out, ins = sys.argv[1], sys.argv[2:]
    srcs = [h5py.File(f, 'r') for f in ins]

    def loc_len(f):
        return f['Location'].shape[0]

    nloc = sum(loc_len(f) for f in srcs)
    with h5py.File(out, 'w') as o:
        f0 = srcs[0]
        for k, v in f0.attrs.items():
            o.attrs[k] = v
        # dimension scales
        o.create_dataset('Location', data=np.arange(nloc, dtype=f0['Location'].dtype))
        o['Location'].make_scale('Location')
        for k, v in f0['Location'].attrs.items():
            if k not in ('CLASS', 'NAME', 'REFERENCE_LIST'):
                o['Location'].attrs[k] = v
        if 'Channel' in f0:
            o.create_dataset('Channel', data=f0['Channel'][...])
            o['Channel'].make_scale('Channel')

        def visit(name, obj):
            if isinstance(obj, h5py.Group):
                g = o.require_group(name)
                for k, v in obj.attrs.items():
                    g.attrs[k] = v
                return
            if name in ('Location', 'Channel'):
                return
            loc_axis = 0 if obj.shape and obj.shape[0] == loc_len(f0) else None
            if loc_axis is None:
                data = obj[...]
            else:
                parts = [s[name][...] for s in srcs]
                data = np.concatenate(parts, axis=0)
            if data.dtype == np.float64:
                data = data.astype(np.float32)
            kw = dict(compression='gzip', compression_opts=4, shuffle=True) if data.ndim and data.dtype.kind != 'O' else {}
            d = o.create_dataset(name, data=data, **kw)
            for k, v in obj.attrs.items():
                if k in ('DIMENSION_LIST',):
                    continue
                d.attrs[k] = (np.asarray(v).astype(np.float32) if (k == '_FillValue' and data.dtype == np.float32) else v)
            if data.ndim >= 1 and loc_axis == 0:
                d.dims[0].attach_scale(o['Location'])
            if data.ndim == 2 and 'Channel' in o and data.shape[1] == o['Channel'].shape[0]:
                d.dims[1].attach_scale(o['Channel'])
            elif data.ndim == 1 and 'Channel' in o and loc_axis is None and data.shape[0] == o['Channel'].shape[0]:
                d.dims[0].attach_scale(o['Channel'])

        f0.visititems(visit)
    print(f"merged {len(ins)} files -> {out}: {nloc} locations")


if __name__ == '__main__':
    main()
