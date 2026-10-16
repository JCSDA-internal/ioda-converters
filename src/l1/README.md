# Level-1 radiance converters of the JCSDA Global MPAS case

Converters from the satellites' Level-1 files to IODA for the all-sky radiances the JCSDA Global
MPAS case assimilates, as that system used them (its `scripts/lib`). They read the raw products
directly, so each file carries the channel set and the geometry the all-sky observers expect.

| Script | Input | Sensors |
|---|---|---|
| (`../hdf5/atms_netcdf_hdf5_2ioda.py`) | JPSS ATMS SDR + GEO (HDF5), one granule at a time | ATMS on NOAA-20, NOAA-21, Suomi NPP |
| `cris_sdr_2ioda.py` | JPSS CrIS full-spectral-resolution SDR + GEO (HDF5); 431 channels (the GSI set) by default, Hamming-apodized | CrIS on NOAA-20, NOAA-21 |
| `iasi_l1c_2ioda.py` | EUMETSAT EPS native IASI L1C; channels from a list (`iasi_channels_gsi616_regional231.txt`: the GSI 616 set plus the 231 all-sky channels, 770) | IASI on Metop-B, Metop-C |
| `mhs_l1b_2ioda.py` | EUMETSAT EPS native MHS L1B | MHS on Metop-B, Metop-C |
| `eum_mw_l1b_2ioda.py` | EUMETSAT microwave L1B (netCDF); for AWS one feed-horn geometry group per file, with `--remap` matching the other groups' channels onto its footprints | AWS MWR, Metop-SG-A1 MWS |
| `gmi_l1c_2ioda.py` | GPM Level 1C (inter-calibrated), GMI | GMI |
| `gpm1c_2ioda.py` | GPM Level 1C (inter-calibrated) | AMSR2, SSMIS F16, F17, F18 |
| `ioda_merge.py` | IODA files | joins files along Location, in the order given |
| `ioda_thin_global.py` | IODA file | cuts to a time window and thins to one location per D-km equal-area box, nearest the box centre and then a reference time (`../satpy/thin_global.py`) |

The converters were written for a regional system: they keep the locations inside `--box` and write
one file per satellite pass (`--gap`, `--minloc`, `--passdir`). For global use, pass the whole
globe and `--minloc 1`, then merge the pass files, cut them to the window and thin them; EWOK's
convert step `l1_2ioda` does this for Skylab's ingest (obs/ingest/l1_*_ingest.yaml):

    iasi_l1c_2ioda.py OUTDIR --box -91 91 -181 181 --gap 1000 --minloc 1 --channels iasi_channels_gsi616_regional231.txt IASI_xxx_1C_M01_*.nat
    ioda_merge.py merged.nc4 OUTDIR/iasi_b_conus_*.nc4
    ioda_thin_global.py merged.nc4 native.nc4 --km 0 --t0 2026-09-06T23:30 --t1 2026-09-07T00:30
    ioda_thin_global.py native.nc4 iasi_metop-b.nc4 --km 120 --t0 2026-09-06T23:30 --t1 2026-09-07T00:30 --tref 2026-09-07T00:00

The code is that of the Global system, reformatted for pycodestyle (black, then by hand where the
style check asked for it). Run on the raw files of 2026-09-07 00Z as above, every script gives the
Global system's files for that hour exactly.
