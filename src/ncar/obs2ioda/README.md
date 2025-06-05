# obs2ioda
## Installation
obs2ioda utilizes CMake as its primary build system. Follow the steps below to build the project:

### Prerequisites
Please make sure the following libraries are installed:
- NetCDF
- NCEP BUFR library. (Instructions for installing the NCEP BUFR library are provided in a subsequent section)

If you have an environment preconfigured for `mpas-jedi`, simply source that environment prior to building `obs2ioda`.

### Build Instructions
1. First, clone the repository into your preferred directory (`<OBS2IODA_ROOT_DIR>`):
   ```bash
   git clone https://github.com/NCAR/obs2ioda.git <OBS2IODA_ROOT_DIR>
   ```
1. Create a new directory `build` and navigate into it:
   ```bash
   mkdir build && cd build
   ```
1. Locate the NCEP BUFR library by executing the following command in the `NCEP BUFR` library's build directory:
   ```bash
   find . -name *libbufr*
   ```
1. Run `CMake` from the `build` directory to configure the project. Be sure to set the build type (`Release`, `RelWithDebInfo`, or `Debug`) and provide the required path to the `NCEP BUFR` library. To build the `GOES-ABI` converter, enable it explicitly with `-DBUILD_GOES_ABI_CONVERTER=ON`.
    ```bash
    cmake <OBS2IODA_ROOT_DIR> \
    -DNCEP_BUFR_LIB=<NCEP_BUFR_LIB_PATH> \
    -DCMAKE_BUILD_TYPE=<BUILD_TYPE> \
    -DBUILD_GOES_ABI_CONVERTER=ON
    ```
1. Finally, build `obs2ioda` using `CMake`'s build tool. In this case, we use `GNU Make`, but other build tools supported by `CMake` can be used:
   ```bash
   make
   ```
The `obs2ioda-v3` executable will reside in the `bin` directory within the build directory.

### Running the Obs2Ioda Test Suite
#### Running the Unit Test Suite
1. **Run the unit test suite** using `ctest`. To see detailed output and the list of tests being executed, add the `--verbose` flag:
   ```bash
   ctest --verbose
   ```
   *(The `--verbose` flag is optional.)*

#### Running the Validation Test Suite
**Steps 1–2 are optional** if you already have a Python environment with `pytest`, `netcdf4`, and `requests` installed.

1. **Create and activate a virtual environment** in the `obs2ioda` root directory:

   ```bash
   python3 -m venv .obs2ioda
   source .obs2ioda/bin/activate
   ```

1. **Install required dependencies**:

   ```bash
   pip install pytest netcdf4 requests
   ```

1. **Run the test suite** from the `obs2ioda` build directory. To display detailed output and see which tests are being run, use the `--verbose` flag:

   ```bash
   pytest --verbose
   ```

1. **Run a specific test suite** by using the `-m` flag followed by the suite name. For example, to run only the GOES-ABI tests:

   ```bash
   pytest -m goes_abi --verbose
   ```

   Or to run the NCEP PREPBUFR tests:

   ```bash
   pytest -m ncep_prepbufr_bufr --verbose
   ```

---

### Currently Available Test Suites

* `goes_abi`
* `ncep_prepbufr_bufr`


---
## Installing NCEP BUFR Library
To install the NCEP BUFR library, follow these steps:

1. Clone the NCEP BUFR repository into a directory of your choice (`<NCEP_BUFR_ROOT_DIR>`):
   ```bash
   git clone https://github.com/NOAA-EMC/NCEPLIBS-bufr.git <NCEP_BUFR_ROOT_DIR>
   ```
1. Create a new directory `build` and navigate into it:
   ```bash
   mkdir build && cd build
   ```
1. Run CMake to configure the build (Ensure NetCDF is installed):
   ```bash
   cmake <NCEP_BUFR_ROOT_DIR>
   ```
1. Build the library with the command:
   ```bash
   make
   ```
1. To locate the NCEP BUFR library, run:
   ```bash
   find . -name *libbufr*
   ```
Remember to note down the library path (`<NCEP_BUFR_LIB_PATH>`) required for the build process of `obs2ioda-v3`.


## Converting PREPBUFR and BUFR files
```
Usage: obs2ioda-v3 [-i input_dir] [-o output_dir] [bufr_filename(s)_to_convert] [-split]
```
If [-i input_dir] [-o output_dir] are not specified in the command line, the default is the current working directory.  
If [bufr_filename(s)_to_convert] is not specified in the command line, the code looks for file name, **prepbufr.bufr** (also **satwnd.bufr**, **gnssro.bufr**, **amsua.bufr**, **airs.bufr**, **mhs.bufr**, **iasi.bufr**, **cris.bufr**), in the input/working directory. If the file exists, do the conversion, otherwise skip it.  
If specify ``-split``, the converted file will contain hourly data.

> obs2ioda-v3 -i input_dir -o output_dir prepbufr.gdas.YYYYMMDD.tHHz.nr

Example output files (date in the output filename is extracted from the input bufr files):  
aircraft_obs_YYYYMMDDHH.h5  
ascat_obs_YYYYMMDDHH.h5  
profiler_obs_YYYYMMDDHH.h5  
satwind_obs_YYYYMMDDHH.h5  
sfc_obs_YYYYMMDDHH.h5  
sondes_obs_YYYYMMDDHH.h5

> obs2ioda-v3 -i input_dir -o output_dir gdas.satwnd.tHHz.YYYYMMDD.bufr

Example output files (date in the output filename is extracted from the input bufr files):  
satwnd_obs_YYYYMMDDHH.h5  (GOES-16/GOES-17, AVHRR (METOP/NOAA), VIIRS (NPP/NOAA), LEOGEO AMVs)

> obs2ioda-v3 -i input_dir -o output_dir gdas.1bamua.tHHz.YYYYMMDD.bufr

Example output files:  
amsua_metop-a_obs_YYYYMMDDHH.h5  
amsua_metop-b_obs_YYYYMMDDHH.h5  
amsua_n15_obs_YYYYMMDDHH.h5  
amsua_n18_obs_YYYYMMDDHH.h5  
amsua_n19_obs_YYYYMMDDHH.h5

> obs2ioda-v3 -i input_dir -o output_dir gdas.airsev.tHHz.YYYYMMDD.bufr

Example output files:  
amsua_aqua_obs_YYYYMMDDHH.h5

> obs2ioda-v3 -i input_dir -o output_dir gdas.1bmhs.tHHz.YYYYMMDD.bufr

Example output files:  
mhs_metop-a_obs_YYYYMMDDHH.h5  
mhs_metop-b_obs_YYYYMMDDHH.h5  
mhs_n18_obs_YYYYMMDDHH.h5  
mhs_n19_obs_YYYYMMDDHH.h5

> obs2ioda-v3 -i input_dir -o output_dir gdas.mtiasi.tHHz.YYYYMMDD.bufr

**the following CRTM SpcCoeff files in little_endian must be present in the working directory for IASI radiance to brightness temperature conversion**  
iasi_metop-a.SpcCoeff.bin -> iasi616_metop-a.SpcCoeff.bin  
iasi_metop-b.SpcCoeff.bin -> iasi616_metop-b.SpcCoeff.bin  
iasi_metop-c.SpcCoeff.bin -> iasi616_metop-c.SpcCoeff.bin

Example output files:  
iasi_metop-a_obs_YYYYMMDDHH.h5  
iasi_metop-b_obs_YYYYMMDDHH.h5  
iasi_metop-c_obs_YYYYMMDDHH.h5

> obs2ioda-v3 -i input_dir -o output_dir gdas.crisf4.tHHz.YYYYMMDD.bufr

**the following CRTM SpcCoeff files in little_endian must be present in the working directory for CrIS radiance to brightness temperature conversion**  
_for **cris** bufr file_  
cris_npp.SpcCoeff.bin -> cris399_npp.SpcCoeff.bin  
cris_n20.SpcCoeff.bin -> cris399_n20.SpcCoeff.bin  
_for **crisf4** bufr file_  
cris_npp.SpcCoeff.bin -> cris-fsr431_npp.SpcCoeff.bin  
cris_n20.SpcCoeff.bin -> cris-fsr431_n20.SpcCoeff.bin

Example output files:  
cris_npp_obs_YYYYMMDDHH.h5  
cris_n20_obs_YYYYMMDDHH.h5

> obs2ioda-v3 -i input_dir -o output_dir gdas.gpsro.tHHz.YYYYMMDD.bufr

Example output files:  
gnssro_obs_YYYYMMDDHH.h5

## Converting Himawari Standard Data (HSD) FLDK files
```
Usage: obs2ioda-v3 -i input_dir -ahi -t YYYYMMDDHHNN [-s 1]
```

Input files are a list of Himawari Standard Data, e.g. HS_H08_20200815_0000_B14_FLDK_R20_S0210.DAT in the input_dir.  
Minute must be specified in the time (-t) option.  
Number of pixels to skip must be specified in the (-s) option. The default value of 1 means no pixels will be skipped.

## Notes
* The output prefix (before _obs) is defined in define_mod.f90
* The mapping of numeric report types to the named types is coded in define_mod.f90
  through subroutines set_obtype_conv, set_name_satellite, set_name_sensor.
* For gdas.satwnd.tHHz.YYYYMMDD.bufr, only GOES-16/GOES-17, AVHRR (METOP/NOAA), VIIRS (NPP/NOAA), LEOGEO AMVs are converted when available. Other AMVs are available through PREPBUFR files.

## The current version is coded to match current GSI-processed diags as close as possible.
* The ob errors of conventional observations are either extracted from the input prepbufr or from an external error table (if obs_errtable exists in the working directory).
* The ob errors of AMSU-A/MHS radiances are coded in define_mod.f90. This should be changed in the future to read in from an external error table.
* The ob errors of satwnd-decoded AMVs are from an external error table (obs_errtable).
* Subroutine filter_obs_conv applies some additional QC for conventional observations as in GSI's read_prepbufr.f90 for the global model and can be de-activated through ``-noqc`` command-line option.
  100 is added to the @PreQC value when the ob is flagged as not-use by filter_obs_conv.  
  100 is chosen to make the original prepbufr quality marker easily readable.
* Subroutine filter_obs_satwnd applies QC for GOES-16/GOES-17 AMVs as in GSI's read_satwnd.f90.  
  @PreQC value is set to 15 for rejected obs.

---

## Converting GOES-ABI Files

```bash
Usage: goes_abi_converter
```

Runtime options are configured through the Fortran namelist file `namelist.goes_abi_converter`.
Input filenames (excluding the path) should be listed in a plain text file.

---

### Example: `namelist.goes_abi_converter`

```fortran
&data_nml
  nc_list_file = 'flist.txt'
  data_dir     = '/data/goes',         ! Path to the GRB netCDF files
  data_id      = 'OR_ABI-L1b-RadF-M6', ! File prefix
  sat_id       = 'G16',
  n_subsample  = 1
/
```

---

### Example: `flist.txt`

```
OR_ABI-L1b-RadF-M6C07_G16_s20212370000202_e20212370009522_c20212370009562.nc
OR_ABI-L1b-RadF-M6C08_G16_s20212370000202_e20212370009510_c20212370009564.nc
OR_ABI-L1b-RadF-M6C09_G16_s20212370000202_e20212370009516_c20212370009566.nc
OR_ABI-L1b-RadF-M6C10_G16_s20212370000202_e20212370009522_c20212370009559.nc
OR_ABI-L1b-RadF-M6C11_G16_s20212370000202_e20212370009510_c20212370009563.nc
OR_ABI-L1b-RadF-M6C12_G16_s20212370000202_e20212370009516_c20212370009569.nc
OR_ABI-L1b-RadF-M6C13_G16_s20212370000202_e20212370009521_c20212370009573.nc
OR_ABI-L1b-RadF-M6C14_G16_s20212370000202_e20212370009510_c20212370009579.nc
OR_ABI-L1b-RadF-M6C15_G16_s20212370000202_e20212370009516_c20212370009582.nc
OR_ABI-L1b-RadF-M6C16_G16_s20212370000202_e20212370009522_c20212370009576.nc
```

---

### Output

For each scan time listed in `flist.txt`, one IODA-v3 NetCDF file is generated. Example output:

```
OR_ABI-L1b-RadF-M6_G16_2021-08-25T00:00:20.2Z.nc4
```

---

### Notes

* **Currently, only bands 7–16 are processed.**
