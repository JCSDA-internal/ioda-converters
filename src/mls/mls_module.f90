module mls_module
!
!
!  Contains the data read from the MLS files 
!
!
  integer, parameter :: maxprof = 7000  !maximum number of profiles (2 days of mls profiles)
  integer, parameter :: maxlev = 100    !maximum number of levels    

  real,    parameter :: mls_pmax = 100.0 !maximum (low altitude) cutoff for using MLS data. 
                                         !Value of 100.0 keeps the data above the tropopause

  real,    parameter :: mls_pmin = 0.002 !minimum (high altitude) cutoff for using MLS data 
                                         !ar_sweep should reject obs that are too high of altitude,
                                         !but to be safe, I put this here
  real,    parameter :: saber_pmin = 0.0002 !minimum (high altitude) cutoff for using SABER data 
                                            ! added 9/8/15 JPM

  real    mls_val(maxprof,maxlev)  ! T(K), O3(ppmv), or H2O(ppmv)       
  real    mls_err(maxprof,maxlev)  ! error stdev 
  real    mls_p(maxlev)     ! pressure (hPa)
  real    mls_lat(maxprof)  ! latitude (deg)
  real    mls_lon(maxprof)  ! longitude (deg)
  integer mls_dt(maxprof)   ! time offset in seconds from anaysis time
  integer nmls              ! number of mls measurements
  integer nlev              ! number of mls pressure levels

end module mls_module
