#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
"""EUMETSAT microwave sounder L1B netCDF (AWS MWR, Metop-SG MWS) -> IODA. Both products share the schema
data/navigation/<p>_lat|lon|*_angle, data/calibration/<p>_toa_brightness_temperature (K*1000), data/processing_information/<p>_brightnesstemp_flag.
AWS geolocation is per feed-horn group (n_geo_groups); GEOGROUP selects it (default 2 = 165-183 GHz band, groups: 0 50 GHz, 1 89 GHz, 2 165-183, 3 325 GHz).
Usage: eum_mw_l1b_2ioda.py OUT.nc4 [--center LAT LON --radius KM] [--geogroup N] FILE.nc [FILE.nc ...]"""
import sys, argparse, datetime as dt, numpy as np, netCDF4, h5py

ap = argparse.ArgumentParser()
ap.add_argument('out')
ap.add_argument('files', nargs='+')
ap.add_argument('--center', nargs=2, type=float)
ap.add_argument('--radius', type=float, default=400.0)
ap.add_argument('--geogroup', type=int, default=2)
ap.add_argument('--obserr', type=float, default=1.0)
ap.add_argument(
    '--remap',
    action='store_true',
    help='AWS: geometry of --geogroup for the file; channels of the other feedhorn groups nearest-neighbour matched onto its '
    'footprints (needed so every file carries the 89/165 GHz scattering-index pair)',
)
ap.add_argument('--remap-km', type=float, default=25.0)
a = ap.parse_args()
AWSGRP = (
    [0] * 8 + [1] + [2] * 7 + [3] * 3
)  # AWS MWR channel -> feedhorn/geolocation group: 50 GHz (1-8), 89 (9), 165-183 (10-16), 325 GHz (17-19)
EPOCH2020 = dt.datetime(2020, 1, 1)
UNIX2020 = int((EPOCH2020 - dt.datetime(1970, 1, 1)).total_seconds())
cols = {k: [] for k in ('lat', 'lon', 'time', 'satzen', 'sataz', 'solzen', 'solaz', 'scanpos', 'bt', 'qc', 'lsm', 'elev')}
platform = instrument = None
nchan = None
freqs = None
for fn in a.files:
    d = netCDF4.Dataset(fn)
    p = 'aws' if 'AWS' in fn.upper() or 'aws_lat' in d['data/navigation'].variables else 'mws'
    platform = platform or ('AWS1' if p == 'aws' else 'Metop-SG A1')
    instrument = instrument or p.upper()
    nav = d['data/navigation']
    cal = d['data/calibration']
    pi = d['data/processing_information']
    lat = nav[p + '_lat'][:]
    lon = nav[p + '_lon'][:]
    szen = nav[p + '_satellite_zenith_angle'][:]
    saz = nav[p + '_satellite_azimuth_angle'][:]
    sozen = nav[p + '_solar_zenith_angle'][:]
    soaz = nav[p + '_solar_azimuth_angle'][:]
    lsm = nav[p + '_surface_type'][:]
    elev = nav[p + '_terrain_elevation'][:]
    if (
        lat.ndim == 3
    ):  # AWS: (scan, fov, geo_group); every group has its own footprints and viewing geometry
        # (2026-09-23: using group 2 for all channels misplaced 50 GHz by up to 370 km and 10 deg in zenith)
        g = a.geogroup
        if a.remap:
            from scipy.spatial import cKDTree

            bt_ = np.ma.filled(cal[p + '_toa_brightness_temperature'][:].astype('f8'), np.nan)
            fl_ = np.ma.filled(pi[p + '_brightnesstemp_flag'][:], 0)

            def xyz(la_, lo_):
                la_, lo_ = np.deg2rad(np.ma.filled(la_.astype('f8'), np.nan)), np.deg2rad(np.ma.filled(lo_.astype('f8'), np.nan))
                return np.c_[(np.cos(la_) * np.cos(lo_)).ravel(), (np.cos(la_) * np.sin(lo_)).ravel(), np.sin(la_).ravel()] * 6371.0

            tgt = xyz(lat[:, :, g], lon[:, :, g])
            good = np.all(np.isfinite(tgt), axis=1)
            remapped = []
            for gg in sorted(set(AWSGRP) - {g}):
                src = xyz(lat[:, :, gg], lon[:, :, gg])
                sg = np.all(np.isfinite(src), axis=1)
                dd = np.full(len(tgt), np.inf)
                kk = np.zeros(len(tgt), int)
                dd[good], kk[good] = cKDTree(src[sg]).query(tgt[good])
                idx = np.where(sg)[0][kk]
                far = dd > a.remap_km
                for c in [i for i, x in enumerate(AWSGRP) if x == gg]:
                    v = bt_[:, :, c].ravel()[idx]
                    v[far] = np.nan
                    bt_[:, :, c] = v.reshape(bt_.shape[:2])
                    f_ = fl_[:, :, c].ravel()[idx]
                    fl_[:, :, c] = f_.reshape(fl_.shape[:2])
                    remapped.append(c + 1)
                print(
                    f'  group {gg} -> {g}: median match distance {np.median(dd[good]):.1f} km, {100 * far[good].mean():.1f}% beyond {a.remap_km:.0f} km'
                )
        lat, lon, szen, saz, sozen, soaz, lsm, elev = (x[:, :, g] for x in (lat, lon, szen, saz, sozen, soaz, lsm, elev))
    elif lsm.ndim == 3:
        lsm = lsm[:, :, 0]
        elev = elev[:, :, 0]  # MWS: (scan, fov, n_resol)
    bt = cal[p + '_toa_brightness_temperature'][:]
    flag = pi[p + '_brightnesstemp_flag'][:]
    if p == 'aws' and a.remap:
        bt = np.ma.masked_invalid(bt_)
        flag = fl_
    tname = 'time_startscan_utc_earthview' if 'time_startscan_utc_earthview' in nav.variables else p + '_scantime_utc'
    t = nav[tname][:] + UNIX2020
    ns, nf, nc = bt.shape
    nchan = nc
    freqs = d['status/instrument/channel_central_freq'][:] if freqs is None else freqs
    T = np.repeat(t[:, None], nf, axis=1)
    pos = np.repeat(np.arange(1, nf + 1)[None, :], ns, axis=0)
    ok = np.isfinite(np.ma.filled(lat.astype('f8'), np.nan)) & np.isfinite(np.ma.filled(lon.astype('f8'), np.nan))
    if a.center:
        la0, lo0 = np.deg2rad(a.center)
        la = np.deg2rad(np.ma.filled(lat, 0.0))
        lo = np.deg2rad(np.ma.filled(lon, 0.0))
        dist = 6371.0 * np.arccos(np.clip(np.sin(la0) * np.sin(la) + np.cos(la0) * np.cos(la) * np.cos(lo - lo0), -1, 1))
        ok &= dist <= a.radius
    for k, arr in (
        ('lat', lat),
        ('lon', lon),
        ('time', T),
        ('satzen', szen),
        ('sataz', saz),
        ('solzen', sozen),
        ('solaz', soaz),
        ('scanpos', pos),
        ('lsm', lsm),
        ('elev', elev),
    ):
        cols[k].append(np.ma.filled(np.ma.asarray(arr).astype('f8'), np.nan)[ok])
    cols['bt'].append(np.ma.filled(np.ma.asarray(bt).astype('f4'), np.nan)[ok])
    cols['qc'].append(np.ma.filled(np.ma.asarray(flag), 0).astype('i4')[ok])
    print(f"{fn.split('/')[-1][:70]}: {ns} scans x {nf} fovs, kept {int(ok.sum())}")
X = {k: np.concatenate(v) for k, v in cols.items()}
n = X['lat'].size
if n == 0:
    sys.exit(f'{a.out}: no footprints inside the selection (group {a.geogroup}, centre {a.center}, radius {a.radius} km); nothing written')
bt = X['bt']
bt[~np.isfinite(bt) | (bt < 50) | (bt > 400)] = -999.0
with h5py.File(a.out, 'w') as o:
    o.attrs['_ioda_layout'] = np.bytes_('ObsGroup')
    o.attrs['_ioda_layout_version'] = np.int32(0)
    o.attrs['platform'] = np.bytes_(platform)
    o.attrs['sensor'] = np.bytes_(instrument)
    o.attrs['source_files'] = np.bytes_(';'.join(f.split('/')[-1] for f in a.files))
    o.attrs['converter'] = np.bytes_('eum_mw_l1b_2ioda.py')
    o.attrs['geogroup'] = np.int32(a.geogroup)
    o.create_dataset('Location', data=np.arange(n, dtype='i8'))
    o['Location'].make_scale('Location')
    o.create_dataset('Channel', data=np.arange(1, nchan + 1, dtype='i4'))
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
    put('MetaData/dateTime', X['time'].astype('i8'), 'seconds since 1970-01-01T00:00:00Z')
    put('MetaData/sensorChannelNumber', np.arange(1, nchan + 1, dtype='i4'), dims=('Channel',))
    put('MetaData/sensorCentralFrequency', freqs.astype('f4'), 'GHz', dims=('Channel',))
    put('MetaData/sensorZenithAngle', f4('satzen'), 'degree', -999.0)
    put('MetaData/sensorAzimuthAngle', (X['sataz'] % 360).astype('f4'), 'degree', -999.0)
    Re, h = 6371.0, (597.0 if instrument == 'AWS' else 832.0)
    put('MetaData/sensorViewAngle', np.rad2deg(np.arcsin(Re / (Re + h) * np.sin(np.deg2rad(X['satzen'])))).astype('f4'), 'degree', -999.0)
    put('MetaData/solarZenithAngle', f4('solzen'), 'degree', -999.0)
    put('MetaData/solarAzimuthAngle', (X['solaz'] % 360).astype('f4'), 'degree', -999.0)
    put('MetaData/sensorScanPosition', X['scanpos'].astype('i4'))
    put('MetaData/landFraction', (X['lsm'] / (10000.0 if X['lsm'].max() > 1.5 else 1.0)).astype('f4'), '1')
    put('MetaData/heightOfSurface', f4('elev'), 'm')
    put('ObsValue/brightnessTemperature', bt, 'K', -999.0, dims=('Location', 'Channel'))
    put('ObsError/brightnessTemperature', np.full((n, nchan), a.obserr, dtype='f4'), 'K', -999.0, dims=('Location', 'Channel'))
    put('PreQC/brightnessTemperature', X['qc'], None, None, dims=('Location', 'Channel'))
print(
    f"{a.out}: {n} locations, {nchan} channels; BT valid {np.mean(bt > 0)*100:.1f}%; time {dt.datetime.utcfromtimestamp(int(X['time'].min()))} .. "
    f"{dt.datetime.utcfromtimestamp(int(X['time'].max()))}; freqs GHz {np.round(freqs,1).tolist()}"
)
