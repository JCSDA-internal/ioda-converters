!
!  (C) Copyright 2017-2019 UCAR
!
!  This software is licensed under the terms of the Apache Licence Version 2.0
!  which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!

module ufo_vars_mod

implicit none
private
public :: ufo_vars_getindex

INTEGER, PARAMETER, PUBLIC :: n_aerosols_gocart_default=14,&
     &n_aerosols_gocart_esrl=15,n_aerosols_other=1

integer, parameter, public :: MAXVARLEN=60
character(len=MAXVARLEN), public, parameter :: var_tv   = "virtual_temperature"
character(len=MAXVARLEN), public, parameter :: var_ts   = "air_temperature"
character(len=MAXVARLEN), public, parameter :: var_t    = "temperature"
character(len=MAXVARLEN), public, parameter :: var_mixr = "humidity_mixing_ratio"
character(len=MAXVARLEN), public, parameter :: var_q    = "specific_humidity"
character(len=MAXVARLEN), public, parameter :: var_u    = "eastward_wind"
character(len=MAXVARLEN), public, parameter :: var_v    = "northward_wind"
character(len=MAXVARLEN), public, parameter :: var_prs  = "air_pressure"
character(len=MAXVARLEN), public, parameter :: var_prsi = "air_pressure_levels"
character(len=MAXVARLEN), public, parameter :: var_delp = "air_pressure_thickness"
character(len=MAXVARLEN), public, parameter :: var_ps   = "surface_pressure"
character(len=MAXVARLEN), public, parameter :: var_z    = "geopotential_height"
character(len=MAXVARLEN), public, parameter :: var_zi   = "geopotential_height_levels"
character(len=MAXVARLEN), public, parameter :: var_sfc_z= "surface_geopotential_height"
character(len=MAXVARLEN), public, parameter :: var_oz   = "mole_fraction_of_ozone_in_air"
character(len=MAXVARLEN), public, parameter :: var_co2  = "mole_fraction_of_carbon_dioxide_in_air"
character(len=MAXVARLEN), public, parameter :: var_clw  = "mass_content_of_cloud_liquid_water_in_atmosphere_layer"
character(len=MAXVARLEN), public, parameter :: var_cli  = "mass_content_of_cloud_ice_in_atmosphere_layer"
character(len=MAXVARLEN), public, parameter :: var_clr  = "mass_content_of_rain_in_atmosphere_layer"
character(len=MAXVARLEN), public, parameter :: var_cls  = "mass_content_of_snow_in_atmosphere_layer"
character(len=MAXVARLEN), public, parameter :: var_clg  = "mass_content_of_graupel_in_atmosphere_layer"
character(len=MAXVARLEN), public, parameter :: var_clh  = "mass_content_of_hail_in_atmosphere_layer"
character(len=MAXVARLEN), public, parameter :: var_clwefr = "effective_radius_of_cloud_liquid_water_particle"
character(len=MAXVARLEN), public, parameter :: var_cliefr = "effective_radius_of_cloud_ice_particle"
character(len=MAXVARLEN), public, parameter :: var_clrefr = "effective_radius_of_rain_particle"
character(len=MAXVARLEN), public, parameter :: var_clsefr = "effective_radius_of_snow_particle"
character(len=MAXVARLEN), public, parameter :: var_clgefr = "effective_radius_of_graupel_particle"
character(len=MAXVARLEN), public, parameter :: var_clhefr = "effective_radius_of_hail_particle"
character(len=MAXVARLEN), public, parameter :: var_cldfrac= "cloud_area_fraction_in_atmosphere_layer"
character(len=MAXVARLEN), public, parameter :: var_sfc_wfrac = "water_area_fraction"
character(len=MAXVARLEN), public, parameter :: var_sfc_lfrac = "land_area_fraction"
character(len=MAXVARLEN), public, parameter :: var_sfc_ifrac = "ice_area_fraction"
character(len=MAXVARLEN), public, parameter :: var_sfc_sfrac = "surface_snow_area_fraction"
character(len=MAXVARLEN), public, parameter :: var_sfc_wtmp  = "surface_temperature_where_sea"
character(len=MAXVARLEN), public, parameter :: var_sfc_ltmp  = "surface_temperature_where_land"
character(len=MAXVARLEN), public, parameter :: var_sfc_itmp  = "surface_temperature_where_ice"
character(len=MAXVARLEN), public, parameter :: var_sfc_stmp  = "surface_temperature_where_snow"
character(len=MAXVARLEN), public, parameter :: var_sfc_sdepth  = "surface_snow_thickness"
character(len=MAXVARLEN), public, parameter :: var_sfc_vegfrac = "vegetation_area_fraction"
character(len=MAXVARLEN), public, parameter :: var_sfc_wspeed  = "surface_wind_speed"
character(len=MAXVARLEN), public, parameter :: var_sfc_wdir    = "surface_wind_from_direction"
character(len=MAXVARLEN), public, parameter :: var_sfc_lai     = "leaf_area_index"
character(len=MAXVARLEN), public, parameter :: var_sfc_soilm   = "volume_fraction_of_condensed_water_in_soil"
character(len=MAXVARLEN), public, parameter :: var_sfc_soilt   = "soil_temperature"
character(len=MAXVARLEN), public, parameter :: var_sfc_landtyp = "land_type_index"
character(len=MAXVARLEN), public, parameter :: var_sfc_vegtyp  = "vegetation_type_index"
character(len=MAXVARLEN), public, parameter :: var_sfc_soiltyp = "soil_type"
character(len=MAXVARLEN), public, parameter :: var_geomz       = "height"
character(len=MAXVARLEN), public, parameter :: var_sfc_geomz   = "surface_altitude"
character(len=MAXVARLEN), public, parameter :: var_sfc_rough   = "surface_roughness_length"
character(len=MAXVARLEN), public, parameter :: var_sfc_t       = "surface_temperature"
character(len=MAXVARLEN), public, parameter :: var_sfc_fact10  = "wind_reduction_factor_at_10m"
character(len=MAXVARLEN), public, parameter :: var_sfc_emiss   = "surface_emissivity"
character(len=MAXVARLEN), public, parameter :: var_sfc_sss     = "sea_surface_salinity"
character(len=MAXVARLEN), public, parameter :: var_opt_depth   = "optical_thickness_of_atmosphere_layer"
character(len=MAXVARLEN), public, parameter :: var_radiance    = "toa_outgoing_radiance_per_unit_wavenumber"
character(len=MAXVARLEN), public, parameter :: var_tb          = "brightness_temperature"
character(len=MAXVARLEN), public, parameter :: var_tb_clr      = "brightness_temperature_assuming_clear_sky"
character(len=MAXVARLEN), public, parameter :: var_lvl_transmit= "transmittances_of_atmosphere_layer"
character(len=MAXVARLEN), public, parameter :: var_lvl_weightfunc= "weightingfunction_of_atmosphere_layer"
character(len=MAXVARLEN), public, parameter :: var_pmaxlev_weightfunc= "pressure_level_at_peak_of_weightingfunction"
character(len=MAXVARLEN), public, parameter :: var_tsavg5      = "average_surface_temperature_within_field_of_view"


character(len=MAXVARLEN), public, parameter :: var_refl        = "equivalent_reflectivity_factor"
character(len=MAXVARLEN), public, parameter :: var_w           = "upward_air_velocity"

!@mzp strings have to be same MAXVARLEN length for array constructor
CHARACTER(len=MAXVARLEN), public, parameter :: var_rh          = "relative_humidity"

CHARACTER(len=MAXVARLEN), DIMENSION(n_aerosols_gocart_default), PUBLIC, PARAMETER  :: &
     &var_aerosols_gocart_default = [&
     &"sulf                                                    ",&
     &"bc1                                                     ",&
     &"bc2                                                     ",&
     &"oc1                                                     ",&
     &"oc2                                                     ",&
     &"dust1                                                   ",&
     &"dust2                                                   ",&
     &"dust3                                                   ",&
     &"dust4                                                   ",&
     &"dust5                                                   ",&
     &"seas1                                                   ",&
     &"seas2                                                   ",&
     &"seas3                                                   ",&
     &"seas4                                                   "]
!@mzp var_aerosols_gocart_esrl =[&
!    &var_aerosols_gocart_default,&
!    &"p25                                                     "]
! won't compile
CHARACTER(len=MAXVARLEN), DIMENSION(n_aerosols_gocart_esrl), PUBLIC, PARAMETER :: &
     &var_aerosols_gocart_esrl = [&
     &"sulf                                                    ",&
     &"bc1                                                     ",&
     &"bc2                                                     ",&
     &"oc1                                                     ",&
     &"oc2                                                     ",&
     &"dust1                                                   ",&
     &"dust2                                                   ",&
     &"dust3                                                   ",&
     &"dust4                                                   ",&
     &"dust5                                                   ",&
     &"seas1                                                   ",&
     &"seas2                                                   ",&
     &"seas3                                                   ",&
     &"seas4                                                   ",&
     &"p25                                                     "]

CHARACTER(len=MAXVARLEN), DIMENSION(n_aerosols_other), PUBLIC, PARAMETER :: &
     &var_aerosols_other = [&
     &"other                                                   "]

character(len=MAXVARLEN), public :: var_seaicefrac      = "sea_ice_category_area_fraction"
character(len=MAXVARLEN), public :: var_seaicethick     = "sea_ice_category_thickness"
character(len=MAXVARLEN), public :: var_seaicesnowthick = "sea_ice_category_snow_thickness"
character(len=MAXVARLEN), public :: var_abs_topo        = "sea_surface_height_above_geoid"
character(len=MAXVARLEN), public :: var_ocn_pot_temp    = "sea_water_potential_temperature"
character(len=MAXVARLEN), public :: var_ocn_con_temp    = "sea_water_conservative_temperature"
character(len=MAXVARLEN), public :: var_ocn_abs_salt    = "sea_water_absolute_salinity"
character(len=MAXVARLEN), public :: var_ocn_pra_salt    = "sea_water_practical_salinity"
character(len=MAXVARLEN), public :: var_ocn_salt        = "sea_water_salinity"
character(len=MAXVARLEN), public :: var_ocn_lay_thick   = "sea_water_cell_thickness"
character(len=MAXVARLEN), public :: var_ocn_sst         = "sea_surface_temperature"
character(len=MAXVARLEN), public :: var_sea_td          = "sea_surface_foundation_temperature"
character(len=MAXVARLEN), public :: var_latent_vap      = "latent_heat_vaporization"
character(len=MAXVARLEN), public :: var_sw_rad          = "net_downwelling_shortwave_radiation"
character(len=MAXVARLEN), public :: var_latent_heat     = "upward_latent_heat_flux_in_air"
character(len=MAXVARLEN), public :: var_sens_heat       = "upward_sensible_heat_flux_in_air"
character(len=MAXVARLEN), public :: var_lw_rad          = "net_downwelling_longwave_radiation"
character(len=MAXVARLEN), public :: var_sea_fric_vel    = "friction_velocity_over_water"

character(len=MAXVARLEN), public, parameter :: var_du001 = "mass_fraction_of_dust001_in_air"
character(len=MAXVARLEN), public, parameter :: var_du002 = "mass_fraction_of_dust002_in_air"
character(len=MAXVARLEN), public, parameter :: var_du003 = "mass_fraction_of_dust003_in_air"
character(len=MAXVARLEN), public, parameter :: var_du004 = "mass_fraction_of_dust004_in_air"
character(len=MAXVARLEN), public, parameter :: var_du005 = "mass_fraction_of_dust005_in_air"
character(len=MAXVARLEN), public, parameter :: var_ss001 = "mass_fraction_of_sea_salt001_in_air"
character(len=MAXVARLEN), public, parameter :: var_ss002 = "mass_fraction_of_sea_salt002_in_air"
character(len=MAXVARLEN), public, parameter :: var_ss003 = "mass_fraction_of_sea_salt003_in_air"
character(len=MAXVARLEN), public, parameter :: var_ss004 = "mass_fraction_of_sea_salt004_in_air"
character(len=MAXVARLEN), public, parameter :: var_ss005 = "mass_fraction_of_sea_salt005_in_air"
character(len=MAXVARLEN), public, parameter :: var_bcphobic = "mass_fraction_of_hydrophobic_black_carbon_in_air"
character(len=MAXVARLEN), public, parameter :: var_bcphilic = "mass_fraction_of_hydrophilic_black_carbon_in_air"
character(len=MAXVARLEN), public, parameter :: var_ocphobic = "mass_fraction_of_hydrophobic_organic_carbon_in_air"
character(len=MAXVARLEN), public, parameter :: var_ocphilic = "mass_fraction_of_hydrophilic_organic_carbon_in_air"
character(len=MAXVARLEN), public, parameter :: var_sulfate = "mass_fraction_of_sulfate_aerosols_in_air"
character(len=MAXVARLEN), public, parameter :: var_no3an1 = "mass_fraction_of_nitrate001_in_air"
character(len=MAXVARLEN), public, parameter :: var_no3an2 = "mass_fraction_of_nitrate002_in_air"
character(len=MAXVARLEN), public, parameter :: var_no3an3 = "mass_fraction_of_nitrate003_in_air"


! ------------------------------------------------------------------------------
contains

! ------------------------------------------------------------------------------

integer function ufo_vars_getindex(vars, varname)
implicit none
character(len=*), intent(in) :: vars(:)
character(len=*), intent(in) :: varname

integer :: ivar

ufo_vars_getindex = -1

do ivar = 1, size(vars)
  if (trim(vars(ivar)) == trim(varname)) then
    ufo_vars_getindex = ivar
    exit
  endif
enddo

end function ufo_vars_getindex

! ------------------------------------------------------------------------------

end module ufo_vars_mod
