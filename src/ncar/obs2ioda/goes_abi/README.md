# goes_abi_converter.x
```
Compile: cd goes_abi/src; make
```
## Convert GOES ReBroadcast netCDF files to ioda-v1 format
```
Usage: goes_abi_converter.x
```
Run-time options are specified through ``namelist.goes_abi_converter``.  
List of GoesReBroadcast files (exclude path) to be processed are specified in a plain text file.

Example namelist.goes_abi_converter:
```
&data_nml
  nc_list_file = 'flist.txt'
  data_dir = '/data/goes',         ! path of the GRB nc files
  data_id = 'OR_ABI-L1b-RadF-M6'   ! prefix of the downloaded GRB nc files
  sat_id = 'G16'
  n_subsample = 1
/
```
Example content of flist.txt:
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
Example output:
```
OR_ABI-L1b-RadF-M6_G16_2021-08-25T00:00:20.2Z.nc4
```
When flist.txt contains multiple scan times, there will be multiple output files.

## Note: Currently only bands 7-16 are processed.
