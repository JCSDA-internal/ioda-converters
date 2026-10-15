#!/usr/bin/env python3

#
# (C) Copyright 2026 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
#
# Geostationary imager full-disk IR scan -> IODA brightness temperatures (all-sky: no cloud
# screening), through satpy, for the geostationary ring of the JCSDA Global MPAS case (its
# cloud analysis and the clear-sky H(x) it compares with). Unlike ami_l1b_to_ioda.py and
# fci_l1c_to_ioda.py, which keep the native grid for thinning in UFO, every scan is written
# twice: at the --stride sampling, and thinned to one location per --thin-km equal-area box
# (thin_global.py).
#
# Layout as the ABI files of goes2ioda: MetaData/latitude, longitude, dateTime,
# sensorZenithAngle, sensorAzimuthAngle, sensorViewAngle, solarZenithAngle, solarAzimuthAngle,
# sensorChannelNumber (CRTM numbering), sensorScanPosition; ObsValue and ObsError
# brightnessTemperature (Location, Channel), fill -999.
#
# Sensors (--sensor; satpy reader; IR channels -> CRTM channel numbers):
#   abi     abi_l1b            C07..C16                                     -> 7..16   (abi_g19, abi_g18)
#   ahi     ahi_hsd            B07..B16                                     -> 7..16   (ahi_himawari9)
#   ami     ami_l1b            SW038 WV063 WV069 WV073 IR087 ... IR133      -> 7..16   (ami_gk2)
#   fci     fci_l1c_nc         ir_38 wv_63 wv_73 ir_87 ir_97 ir_105 ir_123 ir_133 -> 9..16 (fci_mtg-i1)
#   seviri  seviri_l1b_native  IR_039 WV_062 WV_073 IR_087 IR_097 IR_108 IR_120 IR_134 -> 4..11 (seviri_m09/m10/m11)
#
# Sampling: --stride N keeps every Nth pixel and line of the IR grid (N=5: ~10 km for the 2 km
# imagers, N=3 for SEVIRI's 3 km) -> OUT_<res>km.nc4; --thin-km D (one sample per D-km box,
# nearest the box centre) on that set -> OUT_<D>km.nc4. Pixels with a satellite zenith angle
# above --maxzen, or with any converted channel missing or outside 150-350 K, are dropped. The
# time of each line is interpolated between the scan start and end (north to south for ABI,
# AHI and AMI; south to north for FCI and SEVIRI). --csm ACMF.nc (ABI only) adds the clear-sky
# mask (BCM, 0 clear / 1 cloudy) on the same 2 km grid as MetaData/cloudClearMask.
#
# Usage: geo_satpy_2ioda.py --sensor S --out PREFIX [--stride 5] [--thin-km 60] [--maxzen 80]
#                           [--csm ACMF.nc] [--only C13] FILE [FILE ...]
#

import argparse
import datetime as dt
import os
import sys
import warnings

import h5py
import numpy as np

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from thin_global import thin_index

warnings.filterwarnings('ignore')

# satpy reader, IR channel names, CRTM channel numbers, edge where the scan starts
SENSORS = {
    'abi': ('abi_l1b', [f'C{c:02d}' for c in range(7, 17)], list(range(7, 17)), 'N'),
    'ahi': ('ahi_hsd', [f'B{c:02d}' for c in range(7, 17)], list(range(7, 17)), 'N'),
    'ami': ('ami_l1b', ['SW038', 'WV063', 'WV069', 'WV073', 'IR087', 'IR096', 'IR105', 'IR112', 'IR123', 'IR133'],
            list(range(7, 17)), 'N'),
    'fci': ('fci_l1c_nc', ['ir_38', 'wv_63', 'wv_73', 'ir_87', 'ir_97', 'ir_105', 'ir_123', 'ir_133'], list(range(9, 17)), 'S'),
    'seviri': ('seviri_l1b_native', ['IR_039', 'WV_062', 'WV_073', 'IR_087', 'IR_097', 'IR_108', 'IR_120', 'IR_134'],
               list(range(4, 12)), 'S'),
}


def get_args():
    ap = argparse.ArgumentParser(description='Geostationary imager full-disk IR scan to IODA, at the --stride sampling and thinned')
    ap.add_argument('files', nargs='+')
    ap.add_argument('--sensor', required=True, choices=sorted(SENSORS))
    ap.add_argument('--out', required=True, help='output prefix: PREFIX_<res>km.nc4 and PREFIX_<thin-km>km.nc4')
    ap.add_argument('--stride', type=int, default=5)
    ap.add_argument('--thin-km', type=float, default=60.0)
    ap.add_argument('--maxzen', type=float, default=80.0)
    ap.add_argument('--csm')
    ap.add_argument('--obserr', type=float, default=2.0)
    ap.add_argument('--tref')
    ap.add_argument('--only', help='comma list of satpy channel names to convert (e.g. C13); default all IR channels')
    return ap.parse_args()


def main():
    a = get_args()
    reader, names, chans, scan = SENSORS[a.sensor]
    if a.only:
        keep = [i for i, n in enumerate(names) if n in a.only.split(',')]
        names = [names[i] for i in keep]
        chans = [chans[i] for i in keep]
    from satpy import Scene
    from pyorbital.orbital import get_observer_look
    from pyorbital.astronomy import get_alt_az, sun_zenith_angle
    t_start = dt.datetime.now()
    sc = Scene(reader=reader, filenames=a.files)
    sc.load(names, calibration='brightness_temperature')
    ref = sc[names[-3] if len(names) >= 3 else names[0]]   # an IR window channel defines the grid (all IR channels share it)
    area = ref.attrs['area']
    ny, nx = area.shape
    st = a.stride
    rows = np.arange(st // 2, ny, st)
    cols = np.arange(st // 2, nx, st)
    lon2, lat2 = area.get_lonlats(data_slice=(slice(st // 2, ny, st), slice(st // 2, nx, st)))
    lon2 = np.asarray(lon2, 'f8')
    lat2 = np.asarray(lat2, 'f8')
    bt = np.full((lat2.size, len(names)), np.nan, 'f4')
    for k, n in enumerate(names):
        d = sc[n]
        if d.attrs['area'].shape != area.shape:
            raise SystemExit(f'{n}: grid {d.attrs["area"].shape} differs from {area.shape}')
        bt[:, k] = np.asarray(d.data[st // 2::st, st // 2::st].compute(), 'f4').ravel()
    R, C = np.meshgrid(rows, cols, indexing='ij')
    R = R.ravel()
    C = C.ravel()
    lat = lat2.ravel()
    lon = lon2.ravel()
    ok = np.isfinite(lat) & np.isfinite(lon) & (np.abs(lat) <= 90) & np.all(np.isfinite(bt) & (bt > 150) & (bt < 350), axis=1)

    # Time per line. Which edge row 0 is depends on the reader (satpy's FCI and SEVIRI grids start
    # at the south edge), so it is taken from the latitudes.
    t0 = ref.attrs['start_time']
    t1 = ref.attrs['end_time']
    frac = R / max(ny - 1, 1)
    latf = np.where(np.isfinite(lat2) & (np.abs(lat2) <= 90), lat2, np.nan)
    nb = max(1, latf.shape[0] // 10)
    row0_north = np.nanmedian(latf[:nb]) > np.nanmedian(latf[-nb:])
    if (scan == 'S') == row0_north:
        frac = 1.0 - frac      # time runs from the edge where the scan starts
    tsec = (t0 - dt.datetime(1970, 1, 1)).total_seconds() + frac * (t1 - t0).total_seconds()

    # Satellite position and viewing geometry
    op = ref.attrs.get('orbital_parameters', {})
    slon = op.get('satellite_actual_longitude', op.get('satellite_nominal_longitude', op.get('projection_longitude')))
    slat = op.get('satellite_actual_latitude', op.get('satellite_nominal_latitude', 0.0))
    salt = op.get('satellite_actual_altitude', op.get('satellite_nominal_altitude', 35786023.0))
    if not np.isfinite(slat):
        slat = 0.0
    idx = np.nonzero(ok)[0]
    tmid = t0 + (t1 - t0) / 2
    saz, sel = get_observer_look(np.full(idx.size, slon), np.full(idx.size, slat), np.full(idx.size, salt / 1000.0), tmid,
                                 lon[idx], lat[idx], np.zeros(idx.size))
    szen = 90.0 - sel
    good = szen <= a.maxzen
    idx = idx[good]
    saz = saz[good]
    szen = szen[good]
    sunzen = sun_zenith_angle(tmid, lon[idx], lat[idx])
    _, sunaz = get_alt_az(tmid, lon[idx], lat[idx])
    sunaz = np.rad2deg(sunaz) % 360
    # view angle (nadir angle at the satellite) from the zenith angle: sin(view) = R/(R+h) sin(zen)
    view = np.rad2deg(np.arcsin(6371.0 / (6371.0 + salt / 1000.0) * np.sin(np.deg2rad(szen))))
    csm = None
    if a.csm:
        import netCDF4
        m = netCDF4.Dataset(a.csm)['BCM'][:]
        if m.shape == (ny, nx):
            csm = np.asarray(np.ma.filled(m.astype('f4'), np.nan))[R[idx], C[idx]]
        else:
            print(f'csm grid {m.shape} != {(ny, nx)}, skipped')
    plat = ref.attrs.get('platform_name', a.sensor)
    inst = ref.attrs.get('sensor', a.sensor)

    def write(fn, sel_):
        n = sel_.size
        with h5py.File(fn, 'w') as o:
            o.attrs['platformCommonName'] = str(plat)
            o.attrs['sensor'] = str(inst)
            o.attrs['converter'] = 'geo_satpy_2ioda.py'
            o.attrs['datetimeReference'] = t0.strftime('%Y-%m-%dT%H:%M:%SZ')
            o.attrs['scanStart'] = t0.isoformat()
            o.attrs['scanEnd'] = t1.isoformat()
            o.create_dataset('Location', data=np.arange(n, dtype='i4'))
            o['Location'].make_scale('Location')
            o.create_dataset('Channel', data=np.array(chans, 'i4'))
            o['Channel'].make_scale('Channel')

            def put(name, data, dt_='f4', units=None, ch=False, fill=-999.0):
                g, v = name.split('/')
                ds = o.require_group(g).create_dataset(v, data=np.asarray(data).astype(dt_), fillvalue=np.array(fill).astype(dt_),
                                                       compression='gzip', compression_opts=1)
                if units:
                    ds.attrs['units'] = units
                if ch:
                    ds.dims[0].attach_scale(o['Channel'])
                else:
                    ds.dims[0].attach_scale(o['Location'])
                    if ds.ndim == 2:
                        ds.dims[1].attach_scale(o['Channel'])
            ii = idx[sel_]
            put('MetaData/latitude', lat[ii], units='degrees_north')
            put('MetaData/longitude', lon[ii], units='degrees_east')
            put('MetaData/dateTime', np.round(tsec[ii]), 'i8', 'seconds since 1970-01-01T00:00:00Z', fill=-9223372036854775806)
            put('MetaData/sensorZenithAngle', szen[sel_], units='degrees')
            put('MetaData/sensorAzimuthAngle', saz[sel_], units='degrees')
            put('MetaData/sensorViewAngle', view[sel_], units='degrees')
            put('MetaData/solarZenithAngle', sunzen[sel_], units='degrees')
            put('MetaData/solarAzimuthAngle', sunaz[sel_], units='degrees')
            put('MetaData/sensorChannelNumber', chans, 'i4', ch=True, fill=-999)
            put('MetaData/sensorScanPosition', C[ii] + 1, 'i4', fill=-999)   # pixel column (1-based); CRTM's H(x) requires it
            if csm is not None:
                put('MetaData/cloudClearMask', np.nan_to_num(csm[sel_], nan=-999.0))
                o['MetaData/cloudClearMask'].attrs['long_name'] = 'ABI ACM binary cloud mask (BCM): 0 clear, 1 cloudy'
            b = bt[ii]
            put('ObsValue/brightnessTemperature', b, units='K')
            put('ObsError/brightnessTemperature', np.full_like(b, a.obserr), units='K')
        return n

    res = {'seviri': 3}.get(a.sensor, 2) * st
    n1 = write(f'{a.out}_{res}km.nc4', np.arange(idx.size))
    tref = (dt.datetime.fromisoformat(a.tref) - dt.datetime(1970, 1, 1)).total_seconds() if a.tref else None
    k = thin_index(lat[idx], lon[idx], a.thin_km, tsec[idx], tref)
    n2 = write(f'{a.out}_{a.thin_km:g}km.nc4', k)
    kw = -3 if len(chans) >= 3 else 0
    print(f"{a.sensor} {plat}: scan {t0:%Y-%m-%d %H:%M:%S}-{t1:%H:%M:%S}, grid {ny}x{nx}, sat lon {slon:.2f}; "
          f"{n1} locations at ~{res} km (zenith <= {a.maxzen:g}), {n2} at {a.thin_km:g} km; {len(chans)} channels {chans}; "
          f"BT ch {chans[kw]} {np.nanmin(bt[idx, kw]):.1f}..{np.nanmax(bt[idx, kw]):.1f} K"
          + (f"; cloudy fraction (CSM) {np.nanmean(csm == 1):.2f}" if csm is not None else '')
          + f"; {(dt.datetime.now() - t_start).total_seconds():.0f} s")


if __name__ == '__main__':
    main()
