#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
"""EUMETSAT EPS native IASI L1C (EO:EUM:DAT:METOP:IASIL1C-ALL, IASI_xxx_1C_M0x_*.nat, format v11) -> IODA, one file per domain overpass.
Hand-written EPS record parser like mhs_l1b_2ioda.py: big-endian fixed-size records with a 20-byte GRH (class 1 MPHR ascii, class 5 GIADR,
class 8 MDR; MDR-1C = instrument group 8, subclass 2, 2,728,908 bytes per scan line of 30 fields of view x 4 pixels). Byte offsets inside the
MDR (GRH included) from the CODA definition IASI_MDR_L1C_v5 (stcorp codadef, matches PFS EUM-EPS-SYS-SPE-990003):
GEPSDatIasi short-cds [30] at 9122 (u2 day since 2000-01-01, u4 ms), GEPS_SP int4 [30] at 9380, GQisFlagQual u1 [30][4][3] at 255260,
GQisFlagQualDetailed u2 [30][4] at 255620, GGeoSondLoc int4 sf6 [30][4][lon,lat] at 255893, GGeoSondAnglesMETOP int4 sf6 [30][4][zen,az] at 256853,
GGeoSondAnglesSUN [30][4][zen,az] at 263813, IDefSpectDWn1b vsf (i1 sf, i4) at 276777, IDefNsfirst1b int4 at 276782, GS1cSpect int2 [30][4][8700]
at 276790, GEUMAvhrr1BCldFrac u1 [30][4] at 2728548, GEUMAvhrr1BLandFrac u1 [30][4] at 2728668.
Spectra: GS1cSpect index i is absolute sample IDefNsfirst1b+i (= 2581+i, wavenumber (sample-1) x 0.25 cm-1) = L1C channel i+1
(8461 channels, 645 + 0.25 (k-1) cm-1), radiance = int2 x 10^-sf(band) in
W/(m2 sr m-1) with the per-band scale factors of the GIADR-ScaleFactors record (84 bytes: nb i2 at 20, first i2[10] at 22, last i2[10] at 42,
sf i2[10] at 62; bands in absolute sample numbers: 2581-5920 sf 7, 5921-9008 sf 8, 9009-9540 sf 9, 9541-10720 sf 8, 10721-11041 sf 9);
x 1e5 -> mW/(m2 sr cm-1), then the inverse Planck function. CRTM iasi_metop-b/-c channel k = L1C channel k.
Channel subset (--channels, default: the curated CrIS set of data/obs/da/crispasses_*.txt mapped to the nearest IASI channel, 0.5 K in the
CO2 band and 0.8 K in the water-vapour band) is written with explicit errors into the pass list, no placeholder.
Output mirrors cris_sdr_2ioda.py (ObsGroup layout; MetaData/latitude,longitude,dateTime,sensorChannelNumber,...; ObsValue|ObsError|PreQC).
Passes: FOVs inside --box split at time gaps > --gap minutes; a pass with >= --minloc locations is written to OUTDIR/iasi_<b|c>_conus_YYYYMMDDHHMM.nc4
(HHMM = mid-time of the in-box FOVs) and listed in PASSDIR/iasipasses_YYYYMMDD.txt as
YYYYMMDDHHMM|iasi_<b|c>_HHMM|iasi_metop-<b|c>|file|<channels>|<channels>|<ch:err,...>|2.0 (lines with the same tag replaced).
Usage: iasi_l1c_2ioda.py OUTDIR [--box S N W E] [--gap MIN] [--minloc N] [--passdir DIR] [--channels LIST|FILE] [--check] FILE.nat [FILE.nat ...]"""
import sys, os, argparse, datetime as dt, numpy as np, h5py

ap = argparse.ArgumentParser()
ap.add_argument('outdir')
ap.add_argument('files', nargs='+')
ap.add_argument('--box', nargs=4, type=float, default=[20, 50, -129, -65])
ap.add_argument('--gap', type=float, default=10.0)
ap.add_argument('--minloc', type=int, default=500)
ap.add_argument('--passdir')
ap.add_argument('--channels', default='')
ap.add_argument('--errors', default='co2:0.5,h2o:0.8', help='errors per band (co2 = below 1000 cm-1, h2o = above) or ch:err list')
ap.add_argument(
    '--check', action='store_true', help='print mean BT of a few channels per pass (window 900 cm-1, CO2 661 cm-1, H2O 1400 cm-1)'
)
a = ap.parse_args()
S, N, W, E = a.box
UNIX2000 = int((dt.datetime(2000, 1, 1) - dt.datetime(1970, 1, 1)).total_seconds())
C1, C2 = 1.191042972e-5, 1.4387769  # mW/(m2 sr cm-4), cm K
SAT = {'M01': 'b', 'M02': 'a', 'M03': 'c'}
WMO = {'b': 3, 'c': 5, 'a': 4}
RS = 2728908
NSPEC = 8700
NCH = 8461
RE_KM, H_ORBIT_KM = 6371.0, 817.0
OFF = dict(
    time=9122,
    sp=9380,
    flag=255260,
    flagd=255620,
    loc=255893,
    angm=256853,
    angs=263813,
    dwn=276777,
    nsfirst=276782,
    spec=276790,
    cld=2728548,
    land=2728668,
)


def wn(k):
    return 645.0 + 0.25 * (np.asarray(k, dtype='f8') - 1)


def planck_bt(rad, w):  # rad mW/(m2 sr cm-1), w cm-1
    with np.errstate(divide='ignore', invalid='ignore'):
        return np.where(rad > 0, C2 * w / np.log1p(C1 * w**3 / np.where(rad > 0, rad, 1)), np.nan)


def ranges(ch):
    out = []
    s = p = ch[0]
    for c in list(ch[1:]) + [None]:
        if c is not None and c == p + 1:
            p = c
            continue
        out.append(f'{s}-{p}' if p > s else f'{s}')
        s = p = c
    return ','.join(out)


def parse_chans(spec):
    if spec and os.path.isfile(spec):
        spec = open(spec).read().replace('\n', ',')
    out = []
    for x in spec.split(','):
        x = x.strip()
        if not x:
            continue
        if '-' in x:
            lo, hi = x.split('-')
            out += list(range(int(lo), int(hi) + 1))
        else:
            out.append(int(x))
    return sorted(set(out))


# channel subset
if a.channels:
    chans = np.array(parse_chans(a.channels), dtype='i4')
    csrc = a.channels[:120]
else:  # nearest IASI channel to every curated CrIS channel (CrIS FSR: LW 650 + 0.625 (k-1), MW 1210 + 0.625 (k-714), SW 2155 + 0.625 (k-1579))
    import glob

    cl = sorted(glob.glob(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'data', 'obs', 'da', 'crispasses_*.txt')))
    if not cl:
        sys.exit('no crispasses_*.txt to derive the channel subset from; give --channels')
    cc = parse_chans(open(cl[-1]).readline().split('|')[5])
    cw = [650 + 0.625 * (k - 1) if k <= 713 else (1210 + 0.625 * (k - 714) if k <= 1578 else 2155 + 0.625 * (k - 1579)) for k in cc]
    chans = np.array(sorted({int(round((w - 645) / 0.25)) + 1 for w in cw}), dtype='i4')
    csrc = f'CrIS curated set of {os.path.basename(cl[-1])} ({len(cc)} ch) mapped to the nearest IASI channel'
chans = chans[(chans >= 1) & (chans <= NCH)]
wsub = wn(chans)
if ':' in a.errors and not a.errors.startswith(('co2', 'h2o')):
    errs = {int(k): float(v) for k, v in (x.split(':') for x in a.errors.split(','))}
    err = np.array([errs.get(int(c), 1.0) for c in chans], 'f4')
else:
    eb = dict(x.split(':') for x in a.errors.split(','))
    err = np.where(wsub < 1000, float(eb.get('co2', 0.5)), float(eb.get('h2o', 0.8))).astype('f4')
print(f'{len(chans)} channels ({csrc}): {wsub.min():.2f}-{wsub.max():.2f} cm-1, errors {sorted(set(err.tolist()))} K')


def be(buf, o, n, t):
    return np.frombuffer(bytes(buf[o: o + n]), dtype=t)


cols = {}
sat = None
for fn in a.files:
    m = np.memmap(fn, dtype='u1', mode='r')
    off = 0
    mphr = None
    sf = None
    mdrs = []
    while off < len(m):
        cls, grp, sub = int(m[off]), int(m[off + 1]), int(m[off + 2])
        size = int(be(m, off + 4, 4, '>u4')[0])
        if size < 20:
            sys.exit(f'{fn}: corrupt GRH at {off}')
        if cls == 1:
            mphr = dict(ln.split('=', 1) for ln in bytes(m[off + 20: off + size]).decode('ascii', 'replace').split('\n') if '=' in ln)
            mphr = {k.strip(): v.strip() for k, v in mphr.items()}
        elif cls == 5 and size == 84:
            nb = int(be(m, off + 20, 2, '>i2')[0])
            f1 = be(m, off + 22, 20, '>i2')[:nb]
            l1 = be(m, off + 42, 20, '>i2')[:nb]
            s1 = be(m, off + 62, 20, '>i2')[:nb]
            sf = list(zip(f1, l1, s1))
        elif cls == 8 and grp == 8 and sub == 2 and size == RS:
            mdrs.append(off)
        off += size
    if mphr is None or sf is None or not mdrs:
        print(f'{fn}: MPHR {mphr is not None}, scale factors {sf is not None}, {len(mdrs)} MDRs: skipped')
        continue
    s = SAT.get(mphr.get('SPACECRAFT_ID', ''), '?')
    sat = sat or s
    if s != sat:
        print(f'{fn}: satellite {s} differs from {sat}, skipped')
        continue
    fmt = mphr.get('FORMAT_MAJOR_VERSION', '?').strip()
    if fmt != '11':
        print(f'{fn}: format version {fmt} (offsets verified for 11), continuing')
    # per-band scale as a per-channel factor 10^-sf -> W/(m2 sr m-1); x 1e5 -> mW/(m2 sr cm-1)
    o0 = mdrs[0]
    dwn = int(be(m, o0 + OFF['dwn'] + 1, 4, '>i4')[0]) * 10.0 ** (
        -int(np.frombuffer(bytes(m[o0 + OFF['dwn']: o0 + OFF['dwn'] + 1]), 'i1')[0])
    )
    nsf = int(be(m, o0 + OFF['nsfirst'], 4, '>i4')[0])
    # sample numbering is absolute: sample n has wavenumber (n-1) x dwn (2581 -> 645.0 cm-1); channel k = sample nsf+k-1 = GS1cSpect index k-1
    if abs(dwn - 25.0) > 1e-3 or abs((nsf - 1) * dwn / 100.0 - 645.0) > 1e-3:
        print(f'{fn}: WARNING sample width {dwn} m-1, first sample {nsf} -> {(nsf - 1) * dwn / 100:.2f} cm-1 (expected 25, 2581 -> 645.00)')
    samp = chans + nsf - 1
    scale = np.zeros(len(chans))
    for f1, l1, s1 in sf:
        scale[(samp >= f1) & (samp <= l1)] = 10.0 ** (-int(s1)) * 1e5
    if (scale == 0).any():
        sys.exit(f'{fn}: channels outside the scale-factor bands {sf}: {chans[scale == 0][:10]}')
    spec_idx = chans - 1  # index into GS1cSpect
    c = cols.setdefault(
        sat, {k: [] for k in ('lat', 'lon', 'time', 'satzen', 'sataz', 'solzen', 'solaz', 'sp', 'pix', 'bt', 'qc', 'cld', 'land', 'src')}
    )
    nin = 0
    for o in mdrs:
        loc = be(m, o + OFF['loc'], 960, '>i4').reshape(30, 4, 2) * 1e-6
        lon, lat = loc[..., 0], loc[..., 1]
        ok = (lat >= S) & (lat <= N) & (lon >= W) & (lon <= E)
        if not ok.any():
            continue
        t = be(m, o + OFF['time'], 180, 'u1').reshape(30, 6)
        day = np.frombuffer(t[:, :2].tobytes(), '>u2').astype('f8')
        ms = np.frombuffer(t[:, 2:6].tobytes(), '>u4').astype('f8')
        T = np.repeat((UNIX2000 + day * 86400 + ms / 1000.0)[:, None], 4, axis=1)
        am = be(m, o + OFF['angm'], 960, '>i4').reshape(30, 4, 2) * 1e-6
        asun = be(m, o + OFF['angs'], 960, '>i4').reshape(30, 4, 2) * 1e-6
        fq = be(m, o + OFF['flag'], 360, 'u1').reshape(30, 4, 3)
        fqd = be(m, o + OFF['flagd'], 240, '>u2').reshape(30, 4)
        sp = np.repeat(be(m, o + OFF['sp'], 120, '>i4')[:, None], 4, axis=1)
        pix = np.broadcast_to(np.arange(1, 5)[None, :], (30, 4))
        cld = be(m, o + OFF['cld'], 120, 'u1').reshape(30, 4)
        land = be(m, o + OFF['land'], 120, 'u1').reshape(30, 4)
        spec = (
            be(m, o + OFF['spec'], 2 * 30 * 4 * NSPEC, '>i2').reshape(30, 4, NSPEC)[:, :, spec_idx].astype('f8') * scale
        )  # mW/(m2 sr cm-1)
        bt = planck_bt(spec[ok], wsub).astype('f4')
        qc = np.zeros((int(ok.sum()), len(chans)), 'i4')
        bad = ~np.isfinite(bt) | (bt < 150) | (bt > 400)
        qc[bad] |= 1 << 16
        bt[bad] = -999.0
        qflag = ((fq[ok, 0] != 0) | (fq[ok, 1] != 0) | (fq[ok, 2] != 0)).astype('i4') | (fqd[ok].astype('i4') << 1)
        qc |= qflag[:, None]
        for k, v in (
            ('lat', lat),
            ('lon', lon),
            ('time', T),
            ('satzen', am[..., 0]),
            ('sataz', am[..., 1]),
            ('solzen', asun[..., 0]),
            ('solaz', asun[..., 1]),
            ('sp', sp),
            ('pix', pix),
            ('cld', cld),
            ('land', land),
        ):
            c[k].append(np.asarray(v, 'f8')[ok])
        c['bt'].append(bt)
        c['qc'].append(qc)
        nin += int(ok.sum())
    c['src'].append(os.path.basename(fn))
    print(f'{os.path.basename(fn)}: {len(mdrs)} scan lines, {nin} FOVs in box')
os.makedirs(a.outdir, exist_ok=True)
for sat, c in cols.items():
    if not c['lat']:
        continue
    X = {k: np.concatenate(v) for k, v in c.items() if k != 'src'}
    order = np.argsort(X['time'], kind='stable')
    X = {k: v[order] for k, v in X.items()}
    t = X['time']
    cut = np.where(np.diff(t) > a.gap * 60)[0] + 1
    segs = np.split(np.arange(len(t)), cut)
    plist = os.path.join(a.passdir, f'iasipasses_{{}}.txt') if a.passdir else None
    sid = f'iasi_metop-{sat}'
    for seg in segs:
        n = len(seg)
        if n < a.minloc:
            print(f'pass with {n} locations < {a.minloc}, skipped')
            continue
        P = {k: v[seg] for k, v in X.items()}
        tmid = 0.5 * (P['time'].min() + P['time'].max())
        stamp = dt.datetime.utcfromtimestamp(tmid).strftime('%Y%m%d%H%M')
        tag = f'iasi_{sat}_{stamp[8:]}'
        out = os.path.join(a.outdir, f'iasi_{sat}_conus_{stamp}.nc4')
        with h5py.File(out, 'w') as o:
            o.attrs['_ioda_layout'] = np.bytes_('ObsGroup')
            o.attrs['_ioda_layout_version'] = np.int32(0)
            o.attrs['platform'] = np.bytes_(f'Metop-{sat.upper()}')
            o.attrs['sensor'] = np.bytes_('IASI')
            o.attrs['crtm_sensor_id'] = np.bytes_(sid)
            o.attrs['wmo_satellite_id'] = np.int32(WMO[sat])
            o.attrs['source_files'] = np.bytes_(';'.join(c['src']))
            o.attrs['converter'] = np.bytes_('iasi_l1c_2ioda.py')
            o.attrs['channel_subset'] = np.bytes_(csrc)
            o.attrs['channel_mapping'] = np.bytes_(
                'CRTM channel k = L1C sample k, 645 + 0.25 (k-1) cm-1; Gaussian-apodized L1C spectra as delivered'
            )
            o.attrs['box'] = np.array(a.box, dtype='f4')
            o.create_dataset('Location', data=np.arange(n, dtype='i8'))
            o['Location'].make_scale('Location')
            o.create_dataset('Channel', data=chans)
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

            def f4(k):
                return P[k].astype('f4')

            put('MetaData/latitude', f4('lat'), 'degrees_north', -999.0)
            put('MetaData/longitude', f4('lon'), 'degrees_east', -999.0)
            put('MetaData/dateTime', np.round(P['time']).astype('i8'), 'seconds since 1970-01-01T00:00:00Z')
            put('MetaData/sensorChannelNumber', chans, dims=('Channel',))
            put('MetaData/sensorCentralWavenumber', wsub.astype('f4'), 'cm-1', dims=('Channel',))
            put('MetaData/sensorCentralFrequency', (wsub * 29.9792458).astype('f4'), 'GHz', dims=('Channel',))
            put('MetaData/sensorZenithAngle', f4('satzen'), 'degree', -999.0)
            put('MetaData/sensorAzimuthAngle', (P['sataz'] % 360).astype('f4'), 'degree', -999.0)
            put(
                'MetaData/sensorViewAngle',
                np.rad2deg(np.arcsin(RE_KM / (RE_KM + H_ORBIT_KM) * np.sin(np.deg2rad(P['satzen'])))).astype('f4'),
                'degree',
                -999.0,
            )
            put('MetaData/solarZenithAngle', f4('solzen'), 'degree', -999.0)
            put('MetaData/solarAzimuthAngle', (P['solaz'] % 360).astype('f4'), 'degree', -999.0)
            put('MetaData/sensorScanPosition', P['sp'].astype('i4'))
            put('MetaData/fieldOfViewNumber', P['pix'].astype('i4'))
            put('MetaData/cloudAmount', np.where(P['cld'] <= 100, P['cld'] / 100.0, -999.0).astype('f4'), '1', -999.0)
            put('MetaData/landFraction', np.where(P['land'] <= 100, P['land'] / 100.0, -999.0).astype('f4'), '1', -999.0)
            put('ObsValue/brightnessTemperature', P['bt'], 'K', np.float32(-999.0), dims=('Location', 'Channel'))
            put('ObsError/brightnessTemperature', np.tile(err, (n, 1)), 'K', np.float32(-999.0), dims=('Location', 'Channel'))
            put('PreQC/brightnessTemperature', P['qc'], None, None, dims=('Location', 'Channel'))
        if plist:
            pl = plist.format(stamp[:8])
            errs = ",".join(f"{int(k)}:{e:g}" for k, e in zip(chans, err))
            line = f'{stamp}|{tag}|{sid}|{os.path.abspath(out)}|{ranges(chans.tolist())}|{ranges(chans.tolist())}|{errs}|2.0'
            old = [ln.rstrip('\n') for ln in open(pl)] if os.path.exists(pl) else []
            new = sorted(
                set(ln for ln in old if ln and ln.split('|')[1] != tag) | {line}, key=lambda ln: (ln.split('|')[2], ln.split('|')[0])
            )
            open(pl, 'w').write('\n'.join(new) + '\n')
        bt = P['bt']
        tmin, tmax = dt.datetime.utcfromtimestamp(P['time'].min()), dt.datetime.utcfromtimestamp(P['time'].max())
        print(
            f'pass {tag}: {tmin:%H:%M:%S}..{tmax:%H:%M:%S} -> {out}: {n} locations, {len(chans)} channels, BT valid {np.mean(bt > 0)*100:.1f}%, QC!=0 '
            f'{np.mean(P["qc"] != 0)*100:.1f}%, '
            f'cloud fraction mean {np.mean(P["cld"][P["cld"] <= 100])/100:.2f}, solzen<90 {np.mean(P["solzen"] < 90)*100:.0f}%'
            + (f'; pass list {pl}' if plist else '')
        )
        if a.check:
            for w0 in (661.0, 900.0, 1400.0):
                k = int(round((w0 - 645) / 0.25)) + 1
                j = np.where(chans == k)[0]
                if len(j):
                    v = bt[:, j[0]]
                    print(
                        f'  check ch{k} ({w0:.0f} cm-1): mean {np.nanmean(v[v > 0]):.2f} K, min {np.nanmin(v[v > 0]):.1f}, max {np.nanmax(v[v > 0]):.1f}'
                    )
                else:
                    print(f'  check ch{k} ({w0:.0f} cm-1): not in the subset')
