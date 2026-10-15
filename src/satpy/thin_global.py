#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Global equal-area thinning for satellite radiances, used by geo_satpy_2ioda.py.
#
# Boxes: latitude bands of height D km (D / 111.2 degrees); band i holds
# max(1, round(360 cos(lat_i) / dlat)) boxes in longitude, so every box is ~D x D km at any
# latitude. One observation is kept per box: the one nearest the box centre (great-circle
# distance), ties broken by the smaller |time - T| when times are given. Nothing looks at the
# radiances, so cloudy and clear scenes are thinned alike (all-sky).
#
#     from thin_global import thin_index
#     keep = thin_index(lat, lon, 60.0, time=t, t_ref=T)     # indices into lat/lon, sorted
#

import numpy as np

R_EARTH = 6371.0


def box_id(lat, lon, d_km):
    """Equal-area box index and the box centre (lat, lon) for each point."""
    dlat = d_km / 111.2
    nb = int(np.ceil(180.0 / dlat))
    i = np.clip(np.floor((np.asarray(lat, 'f8') + 90.0) / dlat).astype('i8'), 0, nb - 1)
    latc = -90.0 + (i + 0.5) * dlat
    nlon = np.maximum(1, np.round(360.0 * np.cos(np.deg2rad(np.clip(latc, -89.9, 89.9))) / dlat)).astype('i8')
    lon = (np.asarray(lon, 'f8') + 180.0) % 360.0 - 180.0
    j = np.minimum(np.floor((lon + 180.0) / 360.0 * nlon).astype('i8'), nlon - 1)
    lonc = -180.0 + (j + 0.5) * 360.0 / nlon
    return i * 100000 + j, latc, lonc


def thin_index(lat, lon, d_km, time=None, t_ref=None, valid=None):
    """Indices of the kept observations (one per D-km box, nearest the box centre; then nearest t_ref in time)."""
    lat = np.asarray(lat, 'f8')
    lon = np.asarray(lon, 'f8')
    ok = np.isfinite(lat) & np.isfinite(lon) & (np.abs(lat) <= 90.0)
    if valid is not None:
        ok &= np.asarray(valid, bool)
    idx = np.nonzero(ok)[0]
    if idx.size == 0:
        return idx
    key, latc, lonc = box_id(lat[idx], lon[idx], d_km)
    la1, lo1, la2, lo2 = map(np.deg2rad, (lat[idx], lon[idx], latc, lonc))
    dist = R_EARTH * np.arccos(np.clip(np.sin(la1) * np.sin(la2) + np.cos(la1) * np.cos(la2) * np.cos(lo1 - lo2), -1, 1))
    dt = np.zeros_like(dist) if time is None or t_ref is None else np.abs(np.asarray(time, 'f8')[idx] - float(t_ref))
    order = np.lexsort((dt, dist, key))          # by box, then distance to centre, then time
    first = np.ones(order.size, bool)
    first[1:] = key[order][1:] != key[order][:-1]
    return np.sort(idx[order[first]])
