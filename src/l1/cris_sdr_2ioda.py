#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
"""NOAA-20/21 CrIS full-spectral SDR granules (JPSS IDPS HDF5: SCRIF_* spectra + GCRSO_* geolocation, 32 s / 4 scans each) -> IODA v2,
same layout as lib/eum_mw_l1b_2ioda.py (ObsGroup; MetaData/latitude,longitude,dateTime,sensorChannelNumber,...; ObsValue|ObsError|PreQC/brightnessTemperature).

SDR layout (All_Data/CrIS-FS-SDR_All): ES_Real{LW,MW,SW} (scan=4, FOR=30, FOV=9, channel) in mW/(m2 sr cm-1), unapodized (sinc ILS),
  LW 717 = 713 + 2 guard channels per edge (648.75..1096.25), MW 869 = 865 + 4 (1208.75..1751.25), SW 637 = 633 + 4 (2153.75..2551.25), all at 0.625 cm-1.
  QF3_CRISSDR / QF4_CRISSDR (scan, FOR, FOV, band). GEO (All_Data/CrIS-SDR-GEO_All): Latitude, Longitude,
  SatelliteZenith|AzimuthAngle, SolarZenith|AzimuthAngle,
  Height (scan, FOR, FOV); FORTime (scan, FOR) in IET microseconds (TAI since 1958-01-01; UTC = IET - 37 s leap seconds, checked against Beginning_Time).
CRTM cris-fsr_n20|n21 SpcCoeff: 2211 channels = LW 713 (650-1095) + MW 865 (1210-1750) + SW 633 (2155-2550); CRTM channel k maps to the SDR band
  array index (k - k0) + 2 (guard offset), k0 = 1 | 714 | 1579. The SpcCoeff wavenumbers are checked against this grid when the file is present.
Radiances are Hamming-apodized in the spectral domain (0.23, 0.54, 0.23 over neighbouring channels; the guard channels supply the edge neighbours;
  CRTM's CrIS coefficients are for Hamming-apodized spectra, as in the operational BUFR product) and converted to BT with the Planck function at the
  channel wavenumber. All nine FOVs of each FOR are separate locations (sensorScanPosition = FOR 1..30, fieldOfViewNumber = FOV 1..9).
Default channel subset: the 431-channel list of ufo/test/testinput/instrumentTests/cris/cris-fsr_n20_gfs_HofX.yaml (GSI CrIS-FSR set, = cris-fsr431 SpcCoeff).
Granules of one satellite closer than --gap seconds form a pass; one file per pass with >= --minloc in-box locations:
  <root>/cris_<sat>/<date>/cris_<sat>_conus_YYYYMMDDHHMM.nc4 (HHMM = 0.5*(tmin+tmax) of the in-box locations), and a line
  YYYYMMDDHHMM|cris_<sat>_HHMM|cris-fsr_<sat>|file|<channels>|<channels>|CRISERR|2.0 in <passdir>/crispasses_<date>.txt (same tag replaced).
Usage: cris_sdr_2ioda.py --date YYYYMMDD --sat n20|n21 [--root data/obs] [--passdir data/obs/da] [--box 20 50 -129 -65] [--gap 600] [--minloc 500]
       [--obserr 0.6] [--channels LIST|@FILE] [--no-apodize] [--only HHMM] [--dump-full DIR]"""
import os, re, sys, argparse, datetime as dt, numpy as np, h5py

# --- CRTM cris-fsr channel grid: three bands, 0.625 cm-1, channel numbers 1..2211 (LW 1-713, MW 714-1578, SW 1579-2211)
BANDS = (('LW', 1, 713, 650.0), ('MW', 714, 865, 1210.0), ('SW', 1579, 633, 2155.0))  # name, first CRTM channel, count, first wavenumber
GUARD = 2
NCHAN = 2211
DWN = 0.625
SPEC_FILL = -999.0


def wavenumber(k):
    k = np.asarray(k)
    wn = np.full(k.shape, np.nan)
    for _, k0, n, w0 in BANDS:
        m = (k >= k0) & (k < k0 + n)
        wn[m] = w0 + (k[m] - k0) * DWN
    return wn


# --- default subset: ufo/test/testinput/instrumentTests/cris/cris-fsr_n20_gfs_HofX.yaml (channels: &all_channels), 431 channels
UFO_CRIS_FSR_N20 = """19, 24, 26, 27, 28, 31, 32, 33, 37, 39, 42, 44, 47, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68,
69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131,
132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187,
188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 208, 211, 216, 224, 234, 236, 238, 239, 242, 246, 248, 255, 264, 266, 268,
275, 279, 283, 285, 291, 295, 301, 305, 311, 332, 342, 389, 400, 402, 404, 406, 410, 427, 439, 440, 441, 445, 449, 455, 458, 461, 464, 467,
470, 473, 475, 482, 486, 487, 490, 493, 496, 499, 501, 503, 505, 511, 513, 514, 518, 519, 520, 522, 529, 534, 563, 568, 575, 592, 594, 596,
598, 600, 602, 604, 611, 614, 616, 618, 620, 622, 626, 631, 638, 646, 648, 652, 659, 673, 675, 678, 684, 688, 694, 700, 707, 710, 713, 714,
718, 720, 722, 725, 728, 735, 742, 748, 753, 762, 780, 784, 798, 849, 860, 862, 866, 874, 882, 890, 898, 906, 907, 908, 914, 937, 972, 973,
978, 980, 981, 988, 995, 998, 1000, 1003, 1008, 1009, 1010, 1014, 1017, 1018, 1020, 1022, 1024, 1026, 1029, 1030, 1032, 1034, 1037, 1038, 1041,
1042, 1044, 1046, 1049, 1050, 1053, 1054, 1058, 1060, 1062, 1064, 1066, 1069, 1076, 1077, 1080, 1086, 1091, 1095, 1101, 1109, 1112, 1121, 1128,
1133, 1163, 1172, 1187, 1189, 1205, 1211, 1219, 1231, 1245, 1271, 1289, 1300, 1313, 1316, 1325, 1329, 1346, 1347, 1473, 1474, 1491, 1499, 1553,
1570, 1596, 1602, 1619, 1624, 1635, 1939, 1940, 1941, 1942, 1943, 1944, 1945, 1946, 1947, 1948, 1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956,
1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,
1981, 1982, 1983, 1984, 1985, 1986, 1987, 2119, 2140, 2143, 2147, 2153, 2158, 2161, 2168, 2171, 2175, 2182"""


def parse_channels(s):
    if s.startswith('@'):
        s = open(s[1:]).read()
    out = []
    for tok in re.split(r'[,\s]+', s.strip()):
        if not tok:
            continue
        if '-' in tok:
            a, b = tok.split('-')
            out += list(range(int(a), int(b) + 1))
        else:
            out.append(int(tok))
    return sorted(set(out))


def ranges(ch):  # 1,2,3,5 -> "1-3,5"
    out = []
    s = p = ch[0]
    for c in list(ch[1:]) + [None]:
        if c is not None and c == p + 1:
            p = c
            continue
        out.append(f'{s}-{p}' if p > s else f'{s}')
        s = p = c
    return ','.join(out)


C1, C2 = 1.191042972e-5, 1.438776877  # 2hc^2 in mW m-2 sr-1 cm^4; hc/k in cm K


def planck_bt(rad, wn):
    with np.errstate(divide='ignore', invalid='ignore'):
        bt = C2 * wn / np.log1p(C1 * wn**3 / rad)
    bt = np.where(rad > 0, bt, np.nan)
    return bt


IET_LEAP = 37.0
UNIX1958 = int((dt.datetime(1958, 1, 1) - dt.datetime(1970, 1, 1)).total_seconds())


def iet_to_unix(iet):
    return iet.astype('f8') * 1e-6 - IET_LEAP + UNIX1958


SAT = {'n20': ('j01', 'NOAA-20', 225), 'n21': ('j02', 'NOAA-21', 226)}
H_ORBIT_KM, RE_KM = 824.0, 6371.0

ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
ap.add_argument('--date', required=True)
ap.add_argument('--sat', required=True, choices=sorted(SAT))
ap.add_argument('--root', default=os.path.join(os.environ.get('AFDEMO', os.path.expanduser('~/afdemo')), 'data/obs'))
ap.add_argument('--passdir', default=None, help='default <root>/da')
ap.add_argument('--box', nargs=4, type=float, default=[20.0, 50.0, -129.0, -65.0], metavar=('LAT1', 'LAT2', 'LON1', 'LON2'))
ap.add_argument('--gap', type=float, default=600.0)
ap.add_argument('--minloc', type=int, default=500)
ap.add_argument('--obserr', type=float, default=0.6)
ap.add_argument('--channels', default=None, help='comma/range list or @FILE; default: ufo cris-fsr_n20_gfs_HofX.yaml 431-channel set')
ap.add_argument('--no-apodize', action='store_true')
ap.add_argument('--only', default=None, help='convert only the pass whose mid-time is HHMM (validation)')
ap.add_argument(
    '--dump-full',
    default=None,
    help='DIR: also write DIR/cris_<sat>_<stamp>_full.npz with the apodized BT of all 2211 channels (validation)',
)
ap.add_argument(
    '--spccoeff',
    default=None,
    help='CRTM SpcCoeff to check the wavenumber grid against (default data/crtm/ufo_coeffs/cris-fsr_<sat>.SpcCoeff.nc)',
)
a = ap.parse_args()
jtag, platform, wmo = SAT[a.sat]
sid = f'cris-fsr_{a.sat}'
chans = np.array(parse_channels(a.channels) if a.channels else parse_channels(UFO_CRIS_FSR_N20), dtype='i4')
assert chans.min() >= 1 and chans.max() <= NCHAN, 'channel outside 1..2211'
wn_sub = wavenumber(chans)
print(f'{len(chans)} channels, {wn_sub.min():.3f}..{wn_sub.max():.3f} cm-1; apodize={not a.no_apodize}')
spc = a.spccoeff or os.path.join(os.path.dirname(a.root.rstrip('/')), 'crtm/ufo_coeffs', f'{sid}.SpcCoeff.nc')
if os.path.exists(spc):
    import netCDF4

    d = netCDF4.Dataset(spc)
    wn_c = d['Wavenumber'][:]
    ch_c = d['Sensor_Channel'][:]
    dev = np.abs(wn_c - wavenumber(ch_c)).max()
    print(f'SpcCoeff {os.path.basename(spc)}: {ch_c.size} channels, max |wn - grid| = {dev:.2e} cm-1')
    assert dev < 1e-3

indir = os.path.join(a.root, f'cris_{a.sat}', a.date)
passdir = a.passdir or os.path.join(a.root, 'da')
key = re.compile(r'_(d\d{8}_t\d{7}_e\d{7}_b\d{5})_')
files = {}
for fn in os.listdir(indir):
    m = key.search(fn)
    if not m or not fn.endswith('.h5'):
        continue
    files.setdefault(m.group(1), {})['S' if fn.startswith('SCRIF_') else 'G' if fn.startswith('GCRSO_') else 'x'] = os.path.join(indir, fn)


def t0(k):  # granule start (unix s) from the file-name key dYYYYMMDD_tHHMMSSs
    m = re.match(r'd(\d{4})(\d\d)(\d\d)_t(\d\d)(\d\d)(\d\d)(\d)', k)
    y, mo, d_, h, mi, s, ds = map(int, m.groups())
    return (dt.datetime(y, mo, d_, h, mi, s) - dt.datetime(1970, 1, 1)).total_seconds() + ds / 10.0


grans = sorted((t0(k), k) for k, v in files.items() if 'S' in v and 'G' in v)
missing = [k for k, v in files.items() if not ('S' in v and 'G' in v)]
if missing:
    print(f'{len(missing)} granules without an SDR/GEO partner, skipped')
passes = []
for t, k in grans:
    if passes and t - passes[-1][-1][0] <= a.gap:
        passes[-1].append((t, k))
    else:
        passes.append([(t, k)])
print(f'{len(grans)} granule pairs in {indir} -> {len(passes)} passes (gap > {a.gap:.0f} s)')

lat1, lat2, lon1, lon2 = a.box
band_idx = []  # per band: (name, band index, SDR array indices of the subset channels in that band, positions in chans)
for b, (name, k0, n, w0) in enumerate(BANDS):
    pos = np.where((chans >= k0) & (chans < k0 + n))[0]
    band_idx.append((name, b, chans[pos] - k0 + GUARD, pos))


def apod(r, idx):  # r (..., nsdr); Hamming in the spectral domain at the requested SDR indices
    if a.no_apodize:
        return r[..., idx]
    return 0.23 * r[..., idx - 1] + 0.54 * r[..., idx] + 0.23 * r[..., idx + 1]


os.makedirs(passdir, exist_ok=True)
plist = os.path.join(passdir, f'crispasses_{a.date}.txt')
written = []
for pnum, pg in enumerate(passes):
    cols = {k: [] for k in ('lat', 'lon', 'time', 'satzen', 'sataz', 'solzen', 'solaz', 'for', 'fov', 'elev', 'bt', 'qc', 'full')}
    srcs = []
    for t, k in pg:
        g = h5py.File(files[k]['G'], 'r')
        G = g['All_Data/CrIS-SDR-GEO_All']
        lat = G['Latitude'][...]
        lon = G['Longitude'][...]
        ok = (lat >= lat1) & (lat <= lat2) & (lon >= lon1) & (lon <= lon2) & (lat > -900) & (lon > -900)
        if not ok.any():
            continue
        ns, nfor, nfov = lat.shape
        ft = iet_to_unix(G['FORTime'][...])
        T = np.repeat(ft[:, :, None], nfov, axis=2)
        FOR = np.broadcast_to(np.arange(1, nfor + 1)[None, :, None], lat.shape)
        FOV = np.broadcast_to(np.arange(1, nfov + 1)[None, None, :], lat.shape)
        for name, arr in (
            ('lat', lat),
            ('lon', lon),
            ('time', T),
            ('satzen', G['SatelliteZenithAngle'][...]),
            ('sataz', G['SatelliteAzimuthAngle'][...]),
            ('solzen', G['SolarZenithAngle'][...]),
            ('solaz', G['SolarAzimuthAngle'][...]),
            ('for', FOR),
            ('fov', FOV),
            ('elev', G['Height'][...]),
        ):
            cols[name].append(np.asarray(arr, dtype='f8')[ok])
        s = h5py.File(files[k]['S'], 'r')
        S = s['All_Data/CrIS-FS-SDR_All']
        if not srcs:  # time-base check on the first granule of the pass: file attribute Beginning_Time vs IET - leap seconds
            bt_attr = s['Data_Products/CrIS-FS-SDR/CrIS-FS-SDR_Gran_0'].attrs['Beginning_Time'][0][0].decode()
            tt = dt.datetime.utcfromtimestamp(ft[0, 0])
            if abs((tt - tt.replace(hour=int(bt_attr[:2]), minute=int(bt_attr[2:4]), second=int(bt_attr[4:6]))).total_seconds()) > 2:
                print(f'WARNING: IET time base off: FORTime[0,0] {tt} vs Beginning_Time {bt_attr}')
        qf3 = S['QF3_CRISSDR'][...]
        qf4 = S['QF4_CRISSDR'][...]
        bt = np.full((int(ok.sum()), len(chans)), np.nan, dtype='f4')
        qc = np.zeros((int(ok.sum()), len(chans)), dtype='i4')
        full = None
        if a.dump_full:
            full = np.full((int(ok.sum()), NCHAN), np.nan, dtype='f4')
        for name, b, idx, pos in band_idx:
            r = S['ES_Real' + name][...]
            r = r[ok]  # (nok, nsdr)
            bt[:, pos] = planck_bt(apod(r, idx), wavenumber(chans[pos]))
            qc[:, pos] = (qf3[..., b][ok].astype('i4') | ((qf4[..., b][ok].astype('i4') & ~1) << 8))[
                :, None
            ]  # QF4 bit 0 = day/night indicator, not a quality bit
            if full is not None:
                k0, n = BANDS[b][1], BANDS[b][2]
                allidx = np.arange(n) + GUARD
                full[:, k0 - 1: k0 - 1 + n] = planck_bt(apod(r, allidx), wavenumber(np.arange(k0, k0 + n)))
        cols['bt'].append(bt)
        cols['qc'].append(qc)
        cols['full'].append(full)
        srcs.append(os.path.basename(files[k]['S']))
    n = sum(x.size for x in cols['lat'])
    if n == 0:
        print(f'pass {pnum}: {len(pg)} granules from {dt.datetime.utcfromtimestamp(pg[0][0]):%H:%M}, none in box, skipped')
        continue
    tmid = 0.5 * (min(x.min() for x in cols['time']) + max(x.max() for x in cols['time']))
    stamp = dt.datetime.utcfromtimestamp(tmid).strftime('%Y%m%d%H%M')
    tag = f'cris_{a.sat}_{stamp[8:]}'
    if a.only and stamp[8:] != a.only:
        continue
    if n < a.minloc:
        print(f'pass {pnum}: {len(pg)} granules, mid {stamp}, {n} in-box locations < {a.minloc}, skipped')
        continue
    X = {k: np.concatenate(v) for k, v in cols.items() if k != 'full'}
    bt = X['bt']
    bad = ~np.isfinite(bt) | (bt < 150) | (bt > 400)
    X['qc'][bad] |= 1 << 16
    bt[bad] = SPEC_FILL
    out = os.path.join(indir, f'cris_{a.sat}_conus_{stamp}.nc4')
    with h5py.File(out, 'w') as o:
        o.attrs['_ioda_layout'] = np.bytes_('ObsGroup')
        o.attrs['_ioda_layout_version'] = np.int32(0)
        o.attrs['platform'] = np.bytes_(platform)
        o.attrs['sensor'] = np.bytes_('CrIS-FSR')
        o.attrs['crtm_sensor_id'] = np.bytes_(sid)
        o.attrs['wmo_satellite_id'] = np.int32(wmo)
        o.attrs['source_files'] = np.bytes_(';'.join(srcs))
        o.attrs['converter'] = np.bytes_('cris_sdr_2ioda.py')
        o.attrs['apodization'] = np.bytes_('none' if a.no_apodize else 'Hamming (spectral 0.23/0.54/0.23)')
        o.attrs['channel_subset'] = np.bytes_('ufo cris-fsr_n20_gfs_HofX.yaml (431)' if a.channels is None else a.channels[:200])
        o.attrs['channel_mapping'] = np.bytes_(
            'CRTM channel k -> SDR band index (k-k0)+2 guard channels; k0 = 1 (LW 650), 714 (MW 1210), 1579 (SW 2155); 0.625 cm-1'
        )
        o.attrs['time_base'] = np.bytes_('FORTime IET (TAI 1958) - 37 s')
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
            return X[k].astype('f4')

        put('MetaData/latitude', f4('lat'), 'degrees_north', -999.0)
        put('MetaData/longitude', f4('lon'), 'degrees_east', -999.0)
        put('MetaData/dateTime', np.round(X['time']).astype('i8'), 'seconds since 1970-01-01T00:00:00Z')
        put('MetaData/sensorChannelNumber', chans, dims=('Channel',))
        put('MetaData/sensorCentralWavenumber', wn_sub.astype('f4'), 'cm-1', dims=('Channel',))
        put('MetaData/sensorCentralFrequency', (wn_sub * 29.9792458).astype('f4'), 'GHz', dims=('Channel',))
        put('MetaData/sensorZenithAngle', f4('satzen'), 'degree', -999.0)
        put('MetaData/sensorAzimuthAngle', (X['sataz'] % 360).astype('f4'), 'degree', -999.0)
        put(
            'MetaData/sensorViewAngle',
            np.rad2deg(np.arcsin(RE_KM / (RE_KM + H_ORBIT_KM) * np.sin(np.deg2rad(X['satzen'])))).astype('f4'),
            'degree',
            -999.0,
        )
        put('MetaData/solarZenithAngle', f4('solzen'), 'degree', -999.0)
        put('MetaData/solarAzimuthAngle', (X['solaz'] % 360).astype('f4'), 'degree', -999.0)
        put('MetaData/sensorScanPosition', X['for'].astype('i4'))
        put('MetaData/fieldOfViewNumber', X['fov'].astype('i4'))
        put('MetaData/heightOfSurface', f4('elev'), 'm')
        put('ObsValue/brightnessTemperature', bt, 'K', SPEC_FILL, dims=('Location', 'Channel'))
        put('ObsError/brightnessTemperature', np.full((n, len(chans)), a.obserr, dtype='f4'), 'K', SPEC_FILL, dims=('Location', 'Channel'))
        put('PreQC/brightnessTemperature', X['qc'], None, None, dims=('Location', 'Channel'))
    if a.dump_full:
        os.makedirs(a.dump_full, exist_ok=True)
        np.savez(
            os.path.join(a.dump_full, f'cris_{a.sat}_{stamp}_full.npz'),
            bt=np.concatenate(cols['full']),
            lat=X['lat'],
            lon=X['lon'],
            time=X['time'],
            chans=chans,
            wn=wavenumber(np.arange(1, NCHAN + 1)),
        )
    line = f'{stamp}|{tag}|{sid}|{os.path.abspath(out)}|{ranges(chans.tolist())}|{ranges(chans.tolist())}|CRISERR|2.0'
    old = [ln.rstrip('\n') for ln in open(plist)] if os.path.exists(plist) else []
    new = sorted(set(ln for ln in old if ln and ln.split('|')[1] != tag) | {line}, key=lambda ln: (ln.split('|')[2], ln.split('|')[0]))
    open(plist, 'w').write('\n'.join(new) + '\n')
    tmin, tmax = dt.datetime.utcfromtimestamp(X['time'].min()), dt.datetime.utcfromtimestamp(X['time'].max())
    print(
        f'pass {pnum}: {len(srcs)} granules in box of {len(pg)}, {tmin:%H:%M:%S}..{tmax:%H:%M:%S} -> {out}: {n} locations, {len(chans)} channels, BT '
        f'valid {np.mean(bt > 0)*100:.1f}%, '
        f'QC!=0 {np.mean(X["qc"] != 0)*100:.1f}%, solzen<90 {np.mean(X["solzen"] < 90)*100:.0f}%; pass list {plist}'
    )
    written.append(out)
print(f'{len(written)} pass files written for {sid} {a.date}')
