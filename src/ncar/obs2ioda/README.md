# obs2ioda

### To convert NCEP PREPBUFR/BUFR files and Himawari Standard Data FLDK files to ioda-v1 or ioda-v2 format.  

### To convert GOES-ABI netCDF to ioda-v1 format.  

File types that obs2ioda can handle:  
* prepbufr.gdas.YYYYMMDD.tHHz.nr  
* gdas.satwnd.tHHz.YYYYMMDD.bufr  
* gdas.gpsro.tHHz.YYYYMMDD.bufr  
* gdas.1bamua.tHHz.YYYYMMDD.bufr  
* gdas.1bmhs.tHHz.YYYYMMDD.bufr  
* gdas.airsev.tHHz.YYYYMMDD.bufr  
* gdas.cris.tHHz.YYYYMMDD.bufr  
* gdas.crisf4.tHHz.YYYYMMDD.bufr  
* gdas.mtiasi.tHHz.YYYYMMDD.bufr  
* HS_H08_YYYYMMDD_HH00_BNN_FLDK_R20_S0210.DAT  
* OR_ABI-L1b-Rad nc files  

## See [obs2ioda-v1/README.md](https://github.com/jamiebresch/obs2ioda/blob/main/obs2ioda-v1/README.md) for compilation and usage.

## See [obs2ioda-v2/README.md](https://github.com/jamiebresch/obs2ioda/blob/main/obs2ioda-v2/README.md) for compilation and usage.

## See [goes_abi/README.md](https://github.com/jamiebresch/obs2ioda/blob/main/goes_abi/README.md) for compilation and usage.

## Observation data sources:
* https://nomads.ncep.noaa.gov/pub/data/nccf/com/gfs/prod/
* https://rda.ucar.edu/datasets/ds337.0/
* https://rda.ucar.edu/datasets/ds735.0/
* https://registry.opendata.aws/noaa-goes/
* https://registry.opendata.aws/noaa-himawari/
