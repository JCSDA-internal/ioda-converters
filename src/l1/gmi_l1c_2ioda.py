#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
"""GPM GMI level-1C (V08, netCDF, one full orbit per granule) -> IODA v2, one file per CONUS overpass.
Layout follows eum_mw_l1b_2ioda.py (ObsGroup, MetaData/latitude,longitude,dateTime[s since 1970],sensorChannelNumber,
sensorCentralFrequency,sensorZenithAngle,sensorAzimuthAngle,sensorViewAngle,solarZenithAngle,solarAzimuthAngle,sensorScanPosition,
ObsValue|ObsError|PreQC/brightnessTemperature, fill -999). CRTM sensor id: gmi_gpm (13 channels, same order as here).

Input file: groups S1 (10.65-89 GHz, 9 channels, incidence ~52.8 deg) and S2 (166-183 GHz, 4 channels, incidence ~49.2 deg), each with
its own Latitude/Longitude/Quality/incidenceAngle/Tc(nscan,npixel,nchan)/ScanTime/SCstatus. Tc is float K, fill -9999.9 (no scale factor);
Quality byte: 0 good, >0 warning, <0 error (fill -99). There is no sensor azimuth, solar geometry, surface type or elevation in the
file: sensor azimuth is the bearing from the footprint to the sub-satellite point (SCstatus), sensorViewAngle from the incidence angle
and SCaltitude (spherical earth), solar zenith/azimuth from the NOAA solar-position equations at the scan time.

Channel merging: all 13 channels are written on the S1 footprints. For every S1 location the nearest S2 location (KD-tree on unit
vectors, great-circle distance) supplies channels 10-13; matches farther than --maxdist km (default 15) are set to fill. Channel order
1-13 = S1 1-9 then S2 1-4: 10.65V/H, 18.7V/H, 23.8V, 36.64V/H, 89.0V/H, 166.0V/H, 183.31+-3V, 183.31+-7V. sensorCentralFrequency holds
the nominal centre (183.31 for both 183 channels; the sideband is in the channel_description attribute). CAVEAT: one geometry per
location, so channels 10-13 carry the S1 incidence angle (~52.8) instead of their own (~49.2); the S2 angle and the match distance are
kept as MetaData/sensorZenithAngleS2 and MetaData/matchDistanceS2 for diagnostics.

Output: <outdir>/gmi_gpm_conus_YYYYMMDDHHMM.nc4 for each granule with >= --minloc S1 locations in the box (default 500), HHMM = mid-time
0.5*(tmin+tmax) of the in-box locations (same rule as mw_pass_stamp.py). --passdir DIR also maintains DIR/gmipasses_YYYYMMDD.txt
(mwpasses format: time|tag|sensor|file|sim channels|assim channels|GMIERR|max-omb), replacing lines of the same tag.
Usage: gmi_l1c_2ioda.py [--box S N W E] [--minloc 500] [--maxdist 15] [--obserr 1.0] [--outdir DIR] [--passdir DIR] FILE.nc [FILE.nc ...]"""
import os, sys, argparse, datetime as dt, numpy as np, netCDF4, h5py
from scipy.spatial import cKDTree

ap = argparse.ArgumentParser()
ap.add_argument('files', nargs='+')
ap.add_argument('--box', nargs=4, type=float, default=[20, 50, -129, -65])
ap.add_argument('--minloc', type=int, default=500)
ap.add_argument('--maxdist', type=float, default=15.0)
ap.add_argument('--obserr', type=float, default=1.0)
ap.add_argument('--outdir')
ap.add_argument('--passdir')
ap.add_argument('--sid', default='gmi_gpm')
ap.add_argument('--simch', default='1-13')
ap.add_argument('--assimch', default='5-13')
ap.add_argument('--maxomb', default='3.0')
a = ap.parse_args()
S, N, W, E = a.box
RE = 6371.0
FILL = -999.0
EP = dt.datetime(1970, 1, 1)
FREQ = np.array([10.65, 10.65, 18.7, 18.7, 23.8, 36.64, 36.64, 89.0, 89.0, 166.0, 166.0, 183.31, 183.31], dtype='f4')
CHDESC = '1:10.65V 2:10.65H 3:18.7V 4:18.7H 5:23.8V 6:36.64V 7:36.64H 8:89.0V 9:89.0H 10:166.0V 11:166.0H 12:183.31+-3V 13:183.31+-7V (GHz)'
NCH = 13


def scan_epoch(g):
    """ScanTime group -> float64 unix seconds per scan (nan where fill)."""
    y, mo, d, h, mi, s, ms = (
        np.ma.filled(g[k][:].astype('f8'), np.nan) for k in ('Year', 'Month', 'DayOfMonth', 'Hour', 'Minute', 'Second', 'MilliSecond')
    )
    out = np.full(y.shape, np.nan)
    for i in range(y.size):
        if np.isfinite(y[i]) and y[i] > 0:
            out[i] = (dt.datetime(int(y[i]), int(mo[i]), int(d[i]), int(h[i]), int(mi[i]), int(s[i])) - EP).total_seconds() + (
                ms[i] if np.isfinite(ms[i]) else 0
            ) / 1000.0
    return out


def solar(t, lat, lon):
    """NOAA solar position: zenith and north-clockwise azimuth (deg) at unix time t (s), lat/lon (deg)."""
    T = (t / 86400.0 + 2440587.5 - 2451545.0) / 36525.0
    r = np.deg2rad
    L0 = (280.46646 + T * (36000.76983 + 0.0003032 * T)) % 360
    M = 357.52911 + T * (35999.05029 - 0.0001537 * T)
    e = 0.016708634 - T * (0.000042037 + 0.0000001267 * T)
    C = (
        np.sin(r(M)) * (1.914602 - T * (0.004817 + 0.000014 * T))
        + np.sin(r(2 * M)) * (0.019993 - 0.000101 * T)
        + np.sin(r(3 * M)) * 0.000289
    )
    om = 125.04 - 1934.136 * T
    lam = L0 + C - 0.00569 - 0.00478 * np.sin(r(om))
    eps = 23 + (26 + (21.448 - T * (46.815 + T * (0.00059 - T * 0.001813))) / 60) / 60 + 0.00256 * np.cos(r(om))
    decl = np.arcsin(np.sin(r(eps)) * np.sin(r(lam)))
    y = np.tan(r(eps) / 2) ** 2
    eqt = 4 * np.rad2deg(
        y * np.sin(2 * r(L0))
        - 2 * e * np.sin(r(M))
        + 4 * e * y * np.sin(r(M)) * np.cos(2 * r(L0))
        - 0.5 * y * y * np.sin(4 * r(L0))
        - 1.25 * e * e * np.sin(2 * r(M))
    )
    ha = r(((t % 86400) / 60.0 + eqt + 4 * lon) / 4.0 - 180.0)
    la = r(lat)
    zen = np.arccos(np.clip(np.sin(la) * np.sin(decl) + np.cos(la) * np.cos(decl) * np.cos(ha), -1, 1))
    az = (np.rad2deg(np.arctan2(np.sin(ha), np.cos(ha) * np.sin(la) - np.tan(decl) * np.cos(la))) + 180.0) % 360.0
    return np.rad2deg(zen), az


def bearing(lat1, lon1, lat2, lon2):
    """Initial great-circle bearing from point 1 to point 2, deg clockwise from north."""
    p1, p2, dl = np.deg2rad(lat1), np.deg2rad(lat2), np.deg2rad(lon2 - lon1)
    return np.rad2deg(np.arctan2(np.sin(dl) * np.cos(p2), np.cos(p1) * np.sin(p2) - np.sin(p1) * np.cos(p2) * np.cos(dl))) % 360.0


def xyz(lat, lon):
    la, lo = np.deg2rad(lat), np.deg2rad(lon)
    return np.column_stack((np.cos(la) * np.cos(lo), np.cos(la) * np.sin(lo), np.sin(la)))


def read_swath(g):
    lat = np.ma.filled(g['Latitude'][:].astype('f8'), np.nan)
    lon = np.ma.filled(g['Longitude'][:].astype('f8'), np.nan)
    inc = np.ma.filled(g['incidenceAngle'][:, :, 0].astype('f8'), np.nan)
    q = np.ma.filled(g['Quality'][:], -99).astype('i4')
    tc = np.ma.filled(g['Tc'][:].astype('f4'), np.nan)
    t = scan_epoch(g['ScanTime'])
    sc = g['SCstatus']
    sclat, sclon, scalt = (np.ma.filled(sc[k][:].astype('f8'), np.nan) for k in ('SClatitude', 'SClongitude', 'SCaltitude'))
    return dict(lat=lat, lon=lon, inc=inc, q=q, tc=tc, t=t, sclat=sclat, sclon=sclon, scalt=scalt)


written = []
for fn in a.files:
    d = netCDF4.Dataset(fn)
    s1 = read_swath(d['S1'])
    s2 = read_swath(d['S2'])
    d.close()
    ns, nf = s1['lat'].shape
    ok = (
        np.isfinite(s1['lat'])
        & np.isfinite(s1['lon'])
        & np.isfinite(s1['t'])[:, None]
        & (s1['lat'] > S)
        & (s1['lat'] < N)
        & (s1['lon'] > W)
        & (s1['lon'] < E)
    )
    n = int(ok.sum())
    base = fn.split('/')[-1]
    if n < a.minloc:
        print(f'{base[:60]}: {ns} scans, {n} in box < {a.minloc}, skipped')
        continue
    si, pi = np.nonzero(ok)
    lat = s1['lat'][ok]
    lon = s1['lon'][ok]
    t = s1['t'][si]
    # S2 -> S1 collocation: candidates within a 1 deg margin of the box, nearest by chord distance on the unit sphere
    ok2 = (
        np.isfinite(s2['lat'])
        & np.isfinite(s2['lon'])
        & (s2['lat'] > S - 1)
        & (s2['lat'] < N + 1)
        & (s2['lon'] > W - 1)
        & (s2['lon'] < E + 1)
    )
    bt = np.full((n, NCH), np.nan, dtype='f4')
    qc = np.zeros((n, NCH), dtype='i4')
    inc2 = np.full(n, np.nan)
    dist = np.full(n, np.nan)
    bt[:, :9] = s1['tc'][ok]
    qc[:, :9] = s1['q'][ok][:, None]
    if ok2.any():
        dd, ii = cKDTree(xyz(s2['lat'][ok2], s2['lon'][ok2])).query(xyz(lat, lon))
        dist = 2 * RE * np.arcsin(np.clip(dd / 2, 0, 1))
        near = dist <= a.maxdist
        j = ii[near]
        bt[near, 9:] = s2['tc'][ok2][j]
        qc[near, 9:] = s2['q'][ok2][j][:, None]
        inc2[near] = s2['inc'][ok2][j]
        qc[~near, 9:] = -99
    bt[~np.isfinite(bt) | (bt < 50) | (bt > 400) | (qc < 0)] = FILL
    zen = s1['inc'][ok]
    alt = s1['scalt'][si]
    view = np.rad2deg(np.arcsin(RE / (RE + alt) * np.sin(np.deg2rad(zen))))
    saz = bearing(lat, lon, s1['sclat'][si], s1['sclon'][si])
    sozen, soaz = solar(t, lat, lon)
    mid = EP + dt.timedelta(seconds=int(0.5 * (t.min() + t.max())))
    stamp = mid.strftime('%Y%m%d%H%M')
    outdir = a.outdir or os.path.dirname(os.path.abspath(fn))
    out = os.path.join(outdir, f'gmi_gpm_conus_{stamp}.nc4')
    with h5py.File(out, 'w') as o:
        o.attrs['_ioda_layout'] = np.bytes_('ObsGroup')
        o.attrs['_ioda_layout_version'] = np.int32(0)
        o.attrs['platform'] = np.bytes_('GPM')
        o.attrs['sensor'] = np.bytes_('GMI')
        o.attrs['source_files'] = np.bytes_(base)
        o.attrs['converter'] = np.bytes_('gmi_l1c_2ioda.py')
        o.attrs['crtm_sensor_id'] = np.bytes_(a.sid)
        o.attrs['channel_description'] = np.bytes_(CHDESC)
        o.attrs['box_S_N_W_E'] = np.array(a.box, dtype='f4')
        o.attrs['crossing_midtime'] = np.bytes_(mid.strftime('%Y-%m-%dT%H:%M:%SZ'))
        o.attrs['channel_merging'] = np.bytes_(
            f'Channels 1-9 are the S1 swath (10.65-89 GHz) at the S1 footprints; channels 10-13 are the S2 swath (166-183 GHz) taken from the '
            f'nearest S2 footprint to each S1 footprint (great-circle KD-tree match), set to fill where the nearest S2 footprint is > {a.maxdist} km away. '
            f'All 13 channels share the S1 geometry (sensorZenithAngle = S1 incidence angle, ~52.8 deg); the S2 incidence angle (~49.2 deg) is kept in '
            f'MetaData/sensorZenithAngleS2 and the match distance in MetaData/matchDistanceS2 (km).'
        )
        o.attrs['geometry_note'] = np.bytes_(
            'sensorAzimuthAngle = bearing from footprint to sub-satellite point; sensorViewAngle from incidence angle and SCaltitude (spherical earth); '
            'solar angles from the NOAA solar-position equations; PreQC = L1C Quality (0 good, >0 warning, <0 error; BT filled where <0).'
        )
        o.create_dataset('Location', data=np.arange(n, dtype='i8'))
        o['Location'].make_scale('Location')
        o.create_dataset('Channel', data=np.arange(1, NCH + 1, dtype='i4'))
        o['Channel'].make_scale('Channel')
        for g_ in ('MetaData', 'ObsValue', 'ObsError', 'PreQC'):
            o.create_group(g_)

        def put(name, data, units=None, fill=None, dims=('Location',)):
            kw = dict(compression='gzip', compression_opts=4, shuffle=True, chunks=True) if data.ndim else {}
            ds = o.create_dataset(name, data=data, **kw)
            if units:
                ds.attrs['units'] = np.bytes_(units)
            if fill is not None:
                ds.attrs['_FillValue'] = np.array(fill, dtype=data.dtype)
            for i, dn in enumerate(dims):
                ds.dims[i].attach_scale(o[dn])

        def f4(x):
            return np.where(np.isfinite(x), x, FILL).astype('f4')

        put('MetaData/latitude', f4(lat), 'degrees_north', FILL)
        put('MetaData/longitude', f4(lon), 'degrees_east', FILL)
        put('MetaData/dateTime', np.round(t).astype('i8'), 'seconds since 1970-01-01T00:00:00Z')
        put('MetaData/sensorChannelNumber', np.arange(1, NCH + 1, dtype='i4'), dims=('Channel',))
        put('MetaData/sensorCentralFrequency', FREQ, 'GHz', dims=('Channel',))
        put('MetaData/sensorZenithAngle', f4(zen), 'degree', FILL)
        put('MetaData/sensorAzimuthAngle', f4(saz), 'degree', FILL)
        put('MetaData/sensorViewAngle', f4(view), 'degree', FILL)
        put('MetaData/solarZenithAngle', f4(sozen), 'degree', FILL)
        put('MetaData/solarAzimuthAngle', f4(soaz), 'degree', FILL)
        put('MetaData/sensorScanPosition', (pi + 1).astype('i4'))
        put('MetaData/sensorZenithAngleS2', f4(inc2), 'degree', FILL)
        put('MetaData/matchDistanceS2', f4(dist), 'km', FILL)
        put('ObsValue/brightnessTemperature', bt, 'K', FILL, dims=('Location', 'Channel'))
        put('ObsError/brightnessTemperature', np.full((n, NCH), a.obserr, dtype='f4'), 'K', FILL, dims=('Location', 'Channel'))
        put('PreQC/brightnessTemperature', qc, None, None, dims=('Location', 'Channel'))
    nm = int(np.sum(np.isfinite(dist) & (dist <= a.maxdist)))
    print(
        f'{base[:60]}: {ns} scans x {nf} fovs, {n} in box, S2 matched {nm} ({100.0*nm/n:.1f}%, median {np.nanmedian(dist):.1f} km), BT valid '
        f'{100*np.mean(bt[:, :9] > 0):.1f}% S1 / {100*np.mean(bt[:, 9:] > 0):.1f}% S2, '
        f'{dt.datetime.utcfromtimestamp(int(t.min())):%H:%M}..{dt.datetime.utcfromtimestamp(int(t.max())):%H:%M} mid {stamp[-4:]} -> {out}'
    )
    written.append((stamp, out, n))
if a.passdir:
    os.makedirs(a.passdir, exist_ok=True)
    for stamp, out, n in written:
        pf = os.path.join(a.passdir, f'gmipasses_{stamp[:8]}.txt')
        tag = f'gmi_{stamp[8:]}'
        line = f'{stamp}|{tag}|{a.sid}|{os.path.abspath(out)}|{a.simch}|{a.assimch}|GMIERR|{a.maxomb}'
        old = [ln.rstrip('\n') for ln in open(pf)] if os.path.exists(pf) else []
        keep = [ln for ln in old if ln and ln.split('|')[1] != tag]
        keep.append(line)
        keep.sort()
        open(pf, 'w').write('\n'.join(keep) + '\n')
        print(f'passlist {pf}: {line}')
