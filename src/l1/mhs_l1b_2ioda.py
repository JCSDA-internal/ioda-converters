#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
"""EUMETSAT EPS native MHS L1B (EO:EUM:DAT:METOP:MHSL1, MHSx_xxx_1B_M0x_*.nat) -> IODA, one file per domain overpass.
Hand-written EPS record parser (no satpy/eugene reader for MHS native): big-endian fixed-size records with a 20-byte GRH
(class 1 MPHR ascii, 5 GIADR, 8 MDR); MDR-1B (subclass 2, version 3, 4316 bytes) per scan line of 90 FOVs, offsets from the
PFS EPS.MIS.SPE.97229 v7E worksheet (record offsets, GRH included): UTC_SL_TIME day/ms 22/24, SCENE_RADIANCES int4 sf7
mW/m2/sr/cm-1 [90 fov][5 ch] at 83, FOV_DATA_QUALITY u4[90] at 1883 (bits 1-5 = channel bad, bit 0 = all missing),
SPACECRAFT_ALTITUDE u4 sf1 km at 2594, ANGULAR_RELATION int2 sf2 [90][solzen, satzen, solazi, satazi] at 2598,
EARTH_LOCATION int4 sf4 [90][lat, lon] at 3318, SURFACE_PROPERTIES u1[90] (0 water 1 mixed 2 land, 255 undefined) at 4038.
SURFACE_PROPERTIES is undefined (255) for ~30% of CONUS FOVs and TERRAIN_ELEVATION (int2 at 4128) is a constant 3901 m over land in the
June 2026 products, so landFraction carries -999 where undefined and no heightOfSurface is written (CRTM takes the surface from the model).
Radiance -> BT: inverse Planck at the GIADR-RADIANCE CENTRAL_WAVENUMBER_Hn (sf6 cm-1, record offset 418+12(n-1)), then the band
correction T = (T* - intercept)/slope (offsets +4, +8, sf6). Layout of the output mirrors eum_mw_l1b_2ioda.py.
Passes: FOVs inside --box are split at time gaps > --gap minutes; a pass with >= --minloc locations is written to
OUTDIR/mhs_<b|c>_conus_YYYYMMDDHHMM.nc4 (HHMM = mid-time of the in-box FOVs) and listed in PASSDIR/mhspasses_YYYYMMDD.txt
(pipe format of mwpasses_*.txt: stamp|mhs_<b|c>_HHMM|mhs_metop-<b|c>|file|1-5|3-5|MHSERR|3.0; lines with the same tag are replaced).
Usage: mhs_l1b_2ioda.py OUTDIR [--box S N W E] [--gap MIN] [--minloc N] [--passdir DIR] [--obserr K] FILE.nat [FILE.nat ...]"""
import sys, os, argparse, datetime as dt, numpy as np, h5py

ap = argparse.ArgumentParser()
ap.add_argument('outdir')
ap.add_argument('files', nargs='+')
ap.add_argument('--box', nargs=4, type=float, default=[20, 50, -129, -65])
ap.add_argument('--gap', type=float, default=10.0)
ap.add_argument('--minloc', type=int, default=500)
ap.add_argument('--passdir')
ap.add_argument('--obserr', type=float, default=2.0)
a = ap.parse_args()
S, N, W, E = a.box
UNIX2000 = int((dt.datetime(2000, 1, 1) - dt.datetime(1970, 1, 1)).total_seconds())
C1, C2 = 1.191042972e-5, 1.4387769  # mW/m2/sr/cm-4, cm K
SAT = {'M01': 'b', 'M02': 'a', 'M03': 'c'}
FREQ = np.array([89.0, 157.0, 183.311, 183.311, 190.311], 'f4')
NCH, NFOV, RS = 5, 90, 4316


# column slice of the (nrec, RS) byte block -> big-endian array
def be(buf, o, n, t):
    return np.frombuffer(np.ascontiguousarray(buf[:, o: o + n]).tobytes(), dtype=t).reshape(buf.shape[0], -1)


cols = {}  # per satellite: dict of lists
for fn in a.files:
    b = open(fn, 'rb').read()
    off = 0
    mphr = giadr2 = None
    offs = []
    ndummy = 0
    while off < len(b):
        cls, sub = b[off], b[off + 2]
        size = int.from_bytes(b[off + 4: off + 8], 'big')
        if size < 20:
            sys.exit(f'{fn}: corrupt GRH at {off}')
        if cls == 1:
            mphr = dict(ln.split('=', 1) for ln in b[off + 20: off + size].decode('ascii', 'replace').split('\n') if '=' in ln)
            mphr = {k.strip(): v.strip() for k, v in mphr.items()}
        elif cls == 5 and sub == 2:
            giadr2 = b[off: off + size]
        elif cls == 8:
            if sub == 2 and size == RS:
                offs.append(off)
            else:
                ndummy += 1  # dummy MDR (subclass 1, 21 bytes) marking a missing scan line
        off += size
    if not offs:
        print(f'{fn.split("/")[-1][:70]}: no MDR-1B')
        continue
    nmdr = len(offs)
    sat = SAT.get(mphr['SPACECRAFT_ID'], mphr['SPACECRAFT_ID'].lower())
    M = np.frombuffer(b''.join(b[o: o + RS] for o in offs), dtype='u1').reshape(nmdr, RS)
    g = np.frombuffer(giadr2[418:478], dtype='>i4').reshape(5, 3) / 1e6
    wn, bc_a, bc_b = g[:, 0], g[:, 1], g[:, 2]  # cm-1, K, K/K
    t = UNIX2000 + be(M, 22, 2, '>u2')[:, 0].astype('i8') * 86400 + np.round(be(M, 24, 4, '>u4')[:, 0] / 1000.0).astype('i8')
    R = be(M, 83, 1800, '>i4').reshape(nmdr, NFOV, NCH) / 1e7
    q = be(M, 1883, 360, '>u4')
    alt = be(M, 2594, 4, '>u4')[:, 0] / 10.0
    ang = be(M, 2598, 720, '>i2').reshape(nmdr, NFOV, 4) / 100.0
    el = be(M, 3318, 720, '>i4').reshape(nmdr, NFOV, 2) / 1e4
    surf = M[:, 4038:4128].astype('f8')
    with np.errstate(divide='ignore', invalid='ignore'):
        T = C2 * wn / np.log1p(C1 * wn**3 / np.where(R > 0, R, np.nan))
        T = (T - bc_a) / np.where(np.abs(bc_b) > 0.5, bc_b, 1.0)  # (nmdr, 90, 5)
    badch = ((q[:, :, None] >> np.arange(1, NCH + 1)) & 1).astype('i4') | (q[:, :, None] & 1).astype('i4')
    T[badch > 0] = np.nan
    lat, lon = el[:, :, 0], el[:, :, 1]
    ok = (lat >= S) & (lat <= N) & (lon >= W) & (lon <= E) & (np.abs(lat) <= 90) & (np.abs(lon) <= 180)
    c = cols.setdefault(
        sat, {k: [] for k in ('lat', 'lon', 'time', 'solzen', 'satzen', 'solaz', 'sataz', 'scanpos', 'alt', 'bt', 'qc', 'lsm', 'src')}
    )
    Tm = np.repeat(t[:, None], NFOV, axis=1)
    pos = np.repeat(np.arange(1, NFOV + 1)[None, :], nmdr, axis=0)
    altm = np.repeat(alt[:, None], NFOV, axis=1)
    for k, arr in (
        ('lat', lat),
        ('lon', lon),
        ('time', Tm),
        ('solzen', ang[:, :, 0]),
        ('satzen', ang[:, :, 1]),
        ('solaz', ang[:, :, 2]),
        ('sataz', ang[:, :, 3]),
        ('scanpos', pos),
        ('alt', altm),
        ('lsm', surf),
    ):
        c[k].append(arr[ok].astype('f8'))
    c['bt'].append(T[ok].astype('f4'))
    c['qc'].append(badch[ok])
    c['src'].append(fn.split('/')[-1])
    print(
        f"{fn.split('/')[-1][:70]}: Metop-{sat.upper()} {nmdr} scans ({ndummy} dummy), in box {int(ok.sum())}, wn {np.round(wn, 4).tolist()} bandcorr a "
        f"{bc_a.tolist()} b {bc_b.tolist()}, "
        f"{dt.datetime.utcfromtimestamp(int(t.min())):%Y-%m-%dT%H:%M}..{dt.datetime.utcfromtimestamp(int(t.max())):%H:%M}"
    )
os.makedirs(a.outdir, exist_ok=True)
passlines = {}
for sat, c in sorted(cols.items()):
    X = {k: np.concatenate(v) for k, v in c.items() if k != 'src'}
    if X['lat'].size == 0:
        print(f'Metop-{sat.upper()}: no FOV in box')
        continue
    o = np.lexsort((X['scanpos'], X['time']))
    X = {k: v[o] for k, v in X.items()}
    keep = np.ones(X['lat'].size, bool)
    keep[1:] = ~((np.diff(X['time']) == 0) & (np.diff(X['scanpos']) == 0))
    X = {k: v[keep] for k, v in X.items()}  # products abut; drop repeated scans
    brk = np.where(np.diff(X['time']) > a.gap * 60)[0] + 1
    segs = np.split(np.arange(X['lat'].size), brk)
    print(f'Metop-{sat.upper()}: {X["lat"].size} in-box FOVs, {len(segs)} segments')
    for idx in segs:
        n = idx.size
        t0, t1 = int(X['time'][idx].min()), int(X['time'][idx].max())
        mid = dt.datetime.utcfromtimestamp((t0 + t1) // 2)
        if n < a.minloc:
            print(f'  skip segment {mid:%Y-%m-%d %H:%M} n={n} < {a.minloc}')
            continue
        out = f'{a.outdir}/mhs_{sat}_conus_{mid:%Y%m%d%H%M}.nc4'
        P = {k: v[idx] for k, v in X.items()}
        bt = P['bt']
        bt[~np.isfinite(bt) | (bt < 50) | (bt > 400)] = -999.0
        with h5py.File(out, 'w') as h:
            h.attrs['_ioda_layout'] = np.bytes_('ObsGroup')
            h.attrs['_ioda_layout_version'] = np.int32(0)
            h.attrs['platform'] = np.bytes_(f'Metop-{sat.upper()}')
            h.attrs['sensor'] = np.bytes_('MHS')
            h.attrs['source_files'] = np.bytes_(';'.join(c['src']))
            h.attrs['converter'] = np.bytes_('mhs_l1b_2ioda.py')
            h.attrs['box'] = np.array(a.box, 'f4')
            h.create_dataset('Location', data=np.arange(n, dtype='i8'))
            h['Location'].make_scale('Location')
            h.create_dataset('Channel', data=np.arange(1, NCH + 1, dtype='i4'))
            h['Channel'].make_scale('Channel')
            for g_ in ('MetaData', 'ObsValue', 'ObsError', 'PreQC'):
                h.create_group(g_)

            def put(name, data, units=None, fill=None, dims=('Location',)):
                kw = dict(compression='gzip', compression_opts=4, shuffle=True, chunks=True) if data.ndim else {}
                ds = h.create_dataset(name, data=data, **kw)
                if units:
                    ds.attrs['units'] = np.bytes_(units)
                if fill is not None:
                    ds.attrs['_FillValue'] = np.array(fill, dtype=data.dtype)
                for i, dn in enumerate(dims):
                    ds.dims[i].attach_scale(h[dn])

            def f4(k):
                return P[k].astype('f4')

            put('MetaData/latitude', f4('lat'), 'degrees_north', -999.0)
            put('MetaData/longitude', f4('lon'), 'degrees_east', -999.0)
            put('MetaData/dateTime', P['time'].astype('i8'), 'seconds since 1970-01-01T00:00:00Z')
            put('MetaData/sensorChannelNumber', np.arange(1, NCH + 1, dtype='i4'), dims=('Channel',))
            put('MetaData/sensorCentralFrequency', FREQ, 'GHz', dims=('Channel',))
            put('MetaData/sensorZenithAngle', f4('satzen'), 'degree', -999.0)
            put('MetaData/sensorAzimuthAngle', (P['sataz'] % 360).astype('f4'), 'degree', -999.0)
            Re = 6371.0
            put(
                'MetaData/sensorViewAngle',
                np.rad2deg(np.arcsin(Re / (Re + P['alt']) * np.sin(np.deg2rad(P['satzen'])))).astype('f4'),
                'degree',
                -999.0,
            )
            put('MetaData/solarZenithAngle', f4('solzen'), 'degree', -999.0)
            put('MetaData/solarAzimuthAngle', (P['solaz'] % 360).astype('f4'), 'degree', -999.0)
            put('MetaData/sensorScanPosition', P['scanpos'].astype('i4'))
            lf = np.select([P['lsm'] == 0, P['lsm'] == 1, P['lsm'] == 2], [0.0, 0.5, 1.0], -999.0).astype('f4')
            put('MetaData/landFraction', lf, '1', -999.0)
            put('ObsValue/brightnessTemperature', bt, 'K', -999.0, dims=('Location', 'Channel'))
            put('ObsError/brightnessTemperature', np.full((n, NCH), a.obserr, dtype='f4'), 'K', -999.0, dims=('Location', 'Channel'))
            put('PreQC/brightnessTemperature', P['qc'].astype('i4'), None, None, dims=('Location', 'Channel'))
        print(
            f'  {out}: {n} locations, {dt.datetime.utcfromtimestamp(t0):%H:%M:%S}..{dt.datetime.utcfromtimestamp(t1):%H:%M:%S}, BT valid '
            f'{np.mean(bt > 0) * 100:.1f}%, lat {P["lat"].min():.1f}..{P["lat"].max():.1f} lon {P["lon"].min():.1f}..{P["lon"].max():.1f}'
        )
        passlines.setdefault(mid.strftime('%Y%m%d'), []).append(
            f'{mid:%Y%m%d%H%M}|mhs_{sat}_{mid:%H%M}|mhs_metop-{sat}|{os.path.abspath(out)}|1-5|3-5|MHSERR|3.0'
        )
if a.passdir:
    os.makedirs(a.passdir, exist_ok=True)
    for d, lines in sorted(passlines.items()):
        pf = f'{a.passdir}/mhspasses_{d}.txt'
        tags = {ln.split('|')[1] for ln in lines}
        old = [ln.rstrip('\n') for ln in open(pf) if ln.strip() and ln.split('|')[1] not in tags] if os.path.exists(pf) else []
        allp = sorted(old + lines, key=lambda ln: (ln.split('|')[2], ln.split('|')[0]))
        open(pf, 'w').write('\n'.join(allp) + '\n')
        print(f'{pf}: {len(lines)} new, {len(allp)} total')
