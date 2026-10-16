#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
"""GPM level-1C (V08, netCDF, one full orbit per granule) conical imagers -> IODA v2, one file per overpass of the domain.
Generalises gmi_l1c_2ioda.py (same IODA layout, geometry, solar angles and pass-list maintenance) to AMSR2 and SSMIS with --sensor:

  --sensor amsr2      CRTM amsr2_gcom-w1, 14 channels: 1-4 = 6.925/7.3 GHz V/H (not in 1C: fill), 5/6 = 10.65 V/H, 7/8 = 18.7 V/H,
                      9/10 = 23.8 V/H, 11/12 = 36.5 V/H, 13/14 = 89.0 V/H.
                      1C groups: S1 10.65, S2 18.7, S3 23.8, S4 36.5 (243 pixels), S5 89 A-scan, S6 89 B-scan (486 pixels); Tc(...,0) = V,
                      Tc(...,1) = H in every group (verified 2026-09-29: over water V-H = +79/+63/+33/+58/+19 K at 10.65/18.7/23.8/36.5/89).
                      S6 is the second 89 GHz feedhorn (B-scan): footprints 14 km from the same-index S5 footprints, same Tb as S5 where they
                      coincide (S5-S6 -0.03 K, std 2 K at < 3 km), incidence 54.36 vs 54.86 deg -> not used (would only double the 89 GHz sampling).
                      Base locations: S5; S1-S4 attached by nearest neighbour.
  --sensor ssmis_f16|ssmis_f17|ssmis_f18
                      CRTM ssmis_f1x, 24 channels: 8 = 150H, 9/10/11 = 183.31+-6.6/+-3/+-1 H, 12/13 = 19.35 H/V, 14 = 22.235V, 15/16 = 37 H/V,
                      17/18 = 91.655 V/H; 1-7 and 19-24 (LAS/UAS temperature channels) are not in 1C: fill.
                      1C groups: S1 = 19V, 19H, 22V; S2 = 37V, 37H; S3 = 150H, 183+-1, 183+-3, 183+-6.6; S4 = 91V, 91H (verified 2026-09-29 over
                      the domain: 183+-1 coldest (~244 K), then +-3 (~257-264 K), then +-6.6 (~267-277 K); V warmer than H over water).
                      Base locations: S3 (150/183 GHz, 180 pixels); S1, S2, S4 attached by nearest neighbour.
                      Known channel failures in these data (1C Quality codes, GPM file spec 1CSSMIS): F17 S2 -127 = 37V failed (37H valid);
                      F18 S3 -125 = 150H failed (183 channels valid). Those codes fill only the failed channel.

Quality handling (1C Quality, per group and pixel; GPM file spec): 0 good; >0 warnings (1 sun glint, 2 RFI, 118 SSMIS solar intrusion into
the warm load, corrected) are kept with the code in PreQC except the codes in --fill-flags (default 2 = RFI), which are set to fill;
<0 errors fill every channel of the group, except the channel-failure codes -125 (150H) and -127 (37V), which fill only the failed
channel (its Tc is already fill) and leave the other channels of the group with PreQC 0.

Geometry: one geometry per location (the base group's): sensorZenithAngle = base incidence angle; sensorAzimuthAngle = bearing from the
footprint to the sub-satellite point (SCstatus); sensorViewAngle from the incidence angle and SCaltitude (spherical earth); solar zenith/
azimuth from the NOAA solar-position equations at the scan time. The incidence angle of each attached group and its match distance are kept
as MetaData/sensorZenithAngle<G> and MetaData/matchDistance<G> (km). Nearest-neighbour matches farther than --maxdist km (default 15) are fill.

Domain: locations inside --box (default: the mesh bounding box with --mesh, else 20..50N, 129..65W) and, with --mesh INIT.nc, within
--meshdist km (default 12) of a mesh cell centre (regional MPAS mesh: drops the box corners outside the mesh).
Output: <outdir>/<sensor>_conus_YYYYMMDDHHMM.nc4 for each granule with >= --minloc base locations in the domain, HHMM = mid-time
0.5*(tmin+tmax) of the in-domain locations (as gmi_l1c_2ioda.py / mw_pass_stamp.py). --passdir DIR maintains DIR/amsr2passes_YYYYMMDD.txt or
DIR/ssmispasses_YYYYMMDD.txt (mwpasses format: time|tag|sensor|file|sim channels|assim channels|ERRTOKEN|max-omb, tags amsr2_HHMM,
ssmis_f17_HHMM, ...), replacing lines of the same tag. Default channel lists (sim|assim): amsr2 5-14|7-14, ssmis_f16 8-18|8-18,
ssmis_f17 8-15,17-18 (no 37V), ssmis_f18 9-18 (no 150H).
Usage: gpm1c_2ioda.py --sensor amsr2|ssmis_f16|ssmis_f17|ssmis_f18 [--mesh INIT.nc] [--box S N W E] [--minloc 500] [--maxdist 15]
                      [--obserr 1.0] [--outdir DIR] [--passdir DIR] [--simch ..] [--assimch ..] [--maxomb 3.0] FILE.nc [FILE.nc ...]"""
import os, sys, argparse, datetime as dt, numpy as np, netCDF4, h5py
from scipy.spatial import cKDTree

SENSORS = {
    'amsr2': dict(
        sid='amsr2_gcom-w1',
        platform='GCOM-W1',
        inst='AMSR2',
        nch=14,
        base='S5',
        family='amsr2',
        tag='amsr2',
        err='AMSR2ERR',
        simch='5-14',
        assimch='7-14',
        freq=[6.925, 6.925, 7.3, 7.3, 10.65, 10.65, 18.7, 18.7, 23.8, 23.8, 36.5, 36.5, 89.0, 89.0],
        groups={
            'S1': [5, 6],
            'S2': [7, 8],
            'S3': [9, 10],
            'S4': [11, 12],
            'S5': [13, 14],
        },  # 1C group -> CRTM channel of Tc(...,0), Tc(...,1)
        desc='1:6.925V 2:6.925H 3:7.3V 4:7.3H (fill) 5:10.65V 6:10.65H 7:18.7V 8:18.7H 9:23.8V 10:23.8H 11:36.5V 12:36.5H 13:89.0V 14:89.0H (GHz)',
    ),
    'ssmis': dict(
        nch=24,
        base='S3',
        family='ssmis',
        err='SSMISERR',
        simch='8-18',
        assimch='8-18',
        platform=None,
        inst='SSMIS',
        freq=[
            50.3,
            52.8,
            53.596,
            54.4,
            55.5,
            57.29,
            59.4,
            150.0,
            183.31,
            183.31,
            183.31,
            19.35,
            19.35,
            22.235,
            37.0,
            37.0,
            91.655,
            91.655,
            63.283248,
            60.792668,
            60.792668,
            60.792668,
            60.792668,
            60.792668,
        ],
        groups={'S1': [13, 12, 14], 'S2': [16, 15], 'S3': [8, 11, 10, 9], 'S4': [17, 18]},
        desc='1-7 LAS 50.3-59.4 (fill) 8:150H 9:183.31+-6.6H 10:183.31+-3H 11:183.31+-1H 12:19.35H 13:19.35V 14:22.235V 15:37H 16:37V '
        '17:91.655V 18:91.655H 19-24 UAS (fill) (GHz)',
    ),
}
CHFAIL = (-125, -127)  # SSMIS channel-failure codes (150H, 37V failed): only the failed channel is missing

ap = argparse.ArgumentParser()
ap.add_argument('files', nargs='+')
ap.add_argument('--sensor', required=True, choices=['amsr2', 'ssmis_f16', 'ssmis_f17', 'ssmis_f18'])
ap.add_argument('--box', nargs=4, type=float)
ap.add_argument('--mesh')
ap.add_argument('--meshdist', type=float, default=12.0)
ap.add_argument('--minloc', type=int, default=500)
ap.add_argument('--maxdist', type=float, default=15.0)
ap.add_argument('--obserr', type=float, default=1.0)
ap.add_argument(
    '--fill-flags', default='2', help='comma list of positive 1C Quality codes whose Tc is set to fill (default 2 = possible RFI)'
)
ap.add_argument('--outdir')
ap.add_argument('--passdir')
ap.add_argument('--simch')
ap.add_argument('--assimch')
ap.add_argument('--maxomb', default='3.0')
a = ap.parse_args()
C = dict(SENSORS['amsr2' if a.sensor == 'amsr2' else 'ssmis'])
if a.sensor != 'amsr2':
    C.update(sid=a.sensor, platform='DMSP-' + a.sensor[-3:].upper(), tag=a.sensor)
FAILED = {
    'ssmis_f17': '8-15,17-18',
    'ssmis_f18': '9-18',
}  # channel lists without the failed channel (F17 37V = ch16, F18 150H = ch8: all fill in 1C V08)
if a.sensor in FAILED:
    C.update(simch=FAILED[a.sensor], assimch=FAILED[a.sensor])
simch = a.simch or C['simch']
assimch = a.assimch or C['assimch']
NCH = C['nch']
FREQ = np.array(C['freq'], dtype='f4')
BASE = C['base']
fillq = [int(x) for x in a.fill_flags.split(',') if x.strip()]
RE = 6371.0
FILL = -999.0
EP = dt.datetime(1970, 1, 1)


def xyz(lat, lon):
    la, lo = np.deg2rad(lat), np.deg2rad(lon)
    return np.column_stack((np.cos(la) * np.cos(lo), np.cos(la) * np.sin(lo), np.sin(la)))


mtree = None
if a.mesh:
    m = netCDF4.Dataset(a.mesh)
    mlat = np.degrees(m['latCell'][:])
    mlon = np.degrees(m['lonCell'][:])
    mlon = np.where(mlon > 180, mlon - 360, mlon)
    m.close()
    mtree = cKDTree(xyz(mlat, mlon))
    box = a.box or [float(mlat.min()), float(mlat.max()), float(mlon.min()), float(mlon.max())]
else:
    box = a.box or [20, 50, -129, -65]
S, N, W, E = box


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
    C_ = (
        np.sin(r(M)) * (1.914602 - T * (0.004817 + 0.000014 * T))
        + np.sin(r(2 * M)) * (0.019993 - 0.000101 * T)
        + np.sin(r(3 * M)) * 0.000289
    )
    om = 125.04 - 1934.136 * T
    lam = L0 + C_ - 0.00569 - 0.00478 * np.sin(r(om))
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


def group_bt(sw, sel):
    """Tc and per-channel PreQC of the selected pixels of one group after the quality rules (nan = fill)."""
    tc = sw['tc'][sel].copy()
    q = sw['q'][sel]
    nc = tc.shape[1]
    qc = np.repeat(q[:, None], nc, axis=1)
    tc[~np.isfinite(tc) | (tc < 50) | (tc > 350)] = np.nan
    chf = np.isin(q, list(CHFAIL))  # channel-failure codes: the failed channel is fill already, the others are good
    if chf.any():
        qc[chf] = np.where(np.isfinite(tc[chf]), 0, q[chf][:, None])
    tc[(q < 0) & ~chf] = np.nan  # any other error code: the whole group is unusable
    for f in fillq:
        tc[q == f] = np.nan
    return tc, qc


written = []
for fn in a.files:
    d = netCDF4.Dataset(fn)
    sw = {g: read_swath(d[g]) for g in C['groups']}
    d.close()
    b = sw[BASE]
    ns, nf = b['lat'].shape
    ok = (
        np.isfinite(b['lat'])
        & np.isfinite(b['lon'])
        & np.isfinite(b['t'])[:, None]
        & (b['lat'] > S)
        & (b['lat'] < N)
        & (b['lon'] > W)
        & (b['lon'] < E)
    )
    if mtree is not None and ok.any():
        dd, _ = mtree.query(xyz(b['lat'][ok], b['lon'][ok]))
        inmesh = 2 * RE * np.arcsin(np.clip(dd / 2, 0, 1)) <= a.meshdist
        ok[ok] = inmesh
    n = int(ok.sum())
    base = fn.split('/')[-1]
    if n < a.minloc:
        print(f'{base[:60]}: {ns} scans, {n} in domain < {a.minloc}, skipped')
        continue
    si, pi = np.nonzero(ok)
    lat = b['lat'][ok]
    lon = b['lon'][ok]
    t = b['t'][si]
    P = xyz(lat, lon)
    bt = np.full((n, NCH), np.nan, dtype='f4')
    qc = np.full((n, NCH), -99, dtype='i4')
    extra = {}
    msg = []
    for g, chs in C['groups'].items():
        k = np.array(chs) - 1
        s = sw[g]
        if g == BASE:
            tcg, qg = group_bt(s, ok)
            bt[:, k] = tcg
            qc[:, k] = qg
            continue
        ok2 = (
            np.isfinite(s['lat'])
            & np.isfinite(s['lon'])
            & (s['lat'] > S - 1)
            & (s['lat'] < N + 1)
            & (s['lon'] > W - 1)
            & (s['lon'] < E + 1)
        )
        dist = np.full(n, np.nan)
        inc2 = np.full(n, np.nan)
        if ok2.any():
            dd, ii = cKDTree(xyz(s['lat'][ok2], s['lon'][ok2])).query(P)
            dist = 2 * RE * np.arcsin(np.clip(dd / 2, 0, 1))
            near = dist <= a.maxdist
            tcg, qg = group_bt(s, ok2)
            bt[np.ix_(near, k)] = tcg[ii[near]]
            qc[np.ix_(near, k)] = qg[ii[near]]
            inc2[near] = s['inc'][ok2][ii[near]]
        extra[g] = (inc2, dist)
        msg.append(f'{g} {100.0 * np.mean(dist <= a.maxdist):.1f}% (med {np.nanmedian(dist):.1f} km)')
    bt[~np.isfinite(bt)] = FILL
    zen = b['inc'][ok]
    alt = b['scalt'][si]
    view = np.rad2deg(np.arcsin(RE / (RE + alt) * np.sin(np.deg2rad(zen))))
    saz = bearing(lat, lon, b['sclat'][si], b['sclon'][si])
    sozen, soaz = solar(t, lat, lon)
    mid = EP + dt.timedelta(seconds=int(0.5 * (t.min() + t.max())))
    stamp = mid.strftime('%Y%m%d%H%M')
    outdir = a.outdir or os.path.dirname(os.path.abspath(fn))
    os.makedirs(outdir, exist_ok=True)
    out = os.path.join(outdir, f'{a.sensor}_conus_{stamp}.nc4')
    with h5py.File(out, 'w') as o:
        o.attrs['_ioda_layout'] = np.bytes_('ObsGroup')
        o.attrs['_ioda_layout_version'] = np.int32(0)
        o.attrs['platform'] = np.bytes_(C['platform'])
        o.attrs['sensor'] = np.bytes_(C['inst'])
        o.attrs['source_files'] = np.bytes_(base)
        o.attrs['converter'] = np.bytes_('gpm1c_2ioda.py')
        o.attrs['crtm_sensor_id'] = np.bytes_(C['sid'])
        o.attrs['channel_description'] = np.bytes_(C['desc'])
        o.attrs['box_S_N_W_E'] = np.array(box, dtype='f4')
        o.attrs['crossing_midtime'] = np.bytes_(mid.strftime('%Y-%m-%dT%H:%M:%SZ'))
        if a.mesh:
            o.attrs['domain_mesh'] = np.bytes_(f'{os.path.abspath(a.mesh)} (nearest cell centre <= {a.meshdist} km)')
        o.attrs['channel_merging'] = np.bytes_(
            f'Locations are the 1C {BASE} footprints; the other groups ({", ".join(g + "->ch" + "/".join(map(str, c)) for g, c in C["groups"].items())}) are '
            f'taken from the nearest footprint of that group (great-circle KD-tree), fill where it is > {a.maxdist} km away. All channels share the '
            f'{BASE} geometry; '
            f'each attached group\'s incidence angle and match distance (km) are in MetaData/sensorZenithAngle<G>, MetaData/matchDistance<G>.'
        )
        o.attrs['geometry_note'] = np.bytes_(
            'sensorAzimuthAngle = bearing from footprint to sub-satellite point; sensorViewAngle from incidence angle and SCaltitude (spherical earth); '
            'solar angles from the NOAA solar-position equations; PreQC = 1C Quality of the source group (0 good, >0 warning, <0 error, -99 no data). '
            f'Tc set to fill for Quality < 0 (except channel-failure codes -125/-127: only the failed channel) and for Quality in {fillq}.'
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
        for g, (inc2, dist) in extra.items():
            put(f'MetaData/sensorZenithAngle{g}', f4(inc2), 'degree', FILL)
            put(f'MetaData/matchDistance{g}', f4(dist), 'km', FILL)
        put('ObsValue/brightnessTemperature', bt, 'K', FILL, dims=('Location', 'Channel'))
        put('ObsError/brightnessTemperature', np.full((n, NCH), a.obserr, dtype='f4'), 'K', FILL, dims=('Location', 'Channel'))
        put('PreQC/brightnessTemperature', qc, None, None, dims=('Location', 'Channel'))
    valid = ' '.join(f'{c}:{100 * np.mean(bt[:, c - 1] > 0):.0f}' for g in C['groups'] for c in C['groups'][g])
    print(
        f'{base[:64]}: {ns} scans x {nf} fovs, {n} in domain, matched {", ".join(msg)}, valid% {valid}, '
        f'{dt.datetime.utcfromtimestamp(int(t.min())):%H:%M}..{dt.datetime.utcfromtimestamp(int(t.max())):%H:%M} mid {stamp[-4:]} -> {out}'
    )
    written.append((stamp, out, n))
if a.passdir:
    os.makedirs(a.passdir, exist_ok=True)
    for stamp, out, n in written:
        pf = os.path.join(a.passdir, f'{C["family"]}passes_{stamp[:8]}.txt')
        tag = f'{C["tag"]}_{stamp[8:]}'
        line = f'{stamp}|{tag}|{C["sid"]}|{os.path.abspath(out)}|{simch}|{assimch}|{C["err"]}|{a.maxomb}'
        old = [ln.rstrip('\n') for ln in open(pf)] if os.path.exists(pf) else []
        keep = [ln for ln in old if ln and ln.split('|')[1] != tag]
        keep.append(line)
        keep.sort()
        open(pf, 'w').write('\n'.join(keep) + '\n')
        print(f'passlist {pf}: {line}')
