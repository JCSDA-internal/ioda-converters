#!/bin/ksh
ncks -F -O -d nlocs,1,100 aod_geoval_2018041500_f.nc4  aod_geoval_2018041500_m.nc4
ncks -F -O -d nlocs,1,10 aod_geoval_2018041500_f.nc4  aod_geoval_2018041500_s.nc4
ncks -F -O -d nlocs,1,100 aod_obs_2018041500_f.nc4  aod_obs_2018041500_m.nc4
ncks -F -O -d nlocs,1,10 aod_obs_2018041500_f.nc4  aod_obs_2018041500_s.nc4
