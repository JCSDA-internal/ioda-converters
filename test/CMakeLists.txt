# (C) Copyright 2019-2022 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# IODA-CONVERTER tests
################################################################################

list( APPEND test_input
  testinput/tropics_2022021600.nc
  testinput/sample.amsr2.gcom-w1.20220216.h5
  testinput/gmi_gpm_20220216.h5
  testinput/icethk_coperl4.nc
  testinput/nsidc_l4_icec.nc
  testinput/amsr2_icec_l2p.nc
  testinput/gds2_sst_l2p.nc
  testinput/gds2_sst_l3u.nc
  testinput/giirs_fy4a-test.nc
  testinput/gmao_argo.nc
  testinput/godae_prof.bin
  testinput/godae_ship.bin
  testinput/godae_trak.bin
  testinput/rads_adt.nc
  testinput/smap_sss_rss.nc
  testinput/hgodas_adt.nc
  testinput/hgodas_insitu.nc
  testinput/hgodas_sst.nc
  testinput/marineglider_AOML.nc
  testinput/ndbc_hfradar_in.nc
  testinput/gsidiag_conv_t_sfc_test.nc
  testinput/gsidiag_conv_uv_testinput.nc
  testinput/gsidiag_amsua_aqua_radiance_test.nc
  testinput/argoclim_test.nc
  testinput/cryosat2_L2_test.nc
  testinput/emc_ice.nc
  testinput/viirs_jpss1_oc_l2.nc
  testinput/modis_aqua_oc_l2.nc
  testinput/viirs_jpss1_oc_l3.nc
  testinput/sondes_obs_2018041500_m.nc4
  testinput/gnssro_obs_2018041500_s.nc4
  testinput/wrfdadiags_goes-16-abi_2018041500.nc
  testinput/b001xx007.20200310.bufr
  testinput/icartt_DC8_20230627_RA.nc
  testinput/viirs_aod.nc
  testinput/tropomi_no2.nc
  testinput/mopitt_co.he5
  testinput/tropess_cris_co.nc
  testinput/modis_aod.hdf
  testinput/2020100106_metars_small.csv
  testinput/singleob.yaml
  testinput/ghcn_20200228.csv
  testinput/SMAP_L2_SM_P_NRT_24342_A_20190822T224858_N16023_002.h5
  testinput/AL945_2020277203000.EDP
  testinput/smos_l2nrt_ssm.nc
  testinput/ascat_ssm.nc
  testinput/SM_REPR_MIR_OSUDP2_20100601T000131_20100601T001849_662_120_1.nc
  testinput/airnow_sites.dat
  testinput/airnow_2020081306.dat
  testinput/aeronet_aod.dat
  testinput/dt_global_twosat_phy_l4_20190101_vDT2021.nc
  testinput/global_vavh_l3_rt_s3a_20210930T18.nc
  testinput/SWOT_L2_SSHA.nc
  testinput/aeronet_cad.dat
  testinput/aeronet_tab.dat
  testinput/ascii_input_IMSscf.20230501.oro_C48.nc
  testinput/nc_input_IMSscf.20230501.oro_C48.nc
  testinput/bfrPrf_C2E6.2021.214.12.00.G16_0001.0001_bufr
  testinput/sample.SNDR.SNPP.ATMS.2022021600.nc4
  testinput/sample.SNDR.J1.ATMS.2022021600.nc4
  testinput/atms_BGremap_coeffs_ch1ch2.nc
  testinput/glider.yaml
  testinput/godae_bgc_argo.nc
  testinput/owp_snow_obs.csv
  testinput/sonde_wmo_double.bufr
  testinput/avhrr_radiance.nc
  testinput/SMAP_L2_SM_P_E_36365_D_20211122T013945_R18240_001.h5
  testinput/satwinds_ssec2021080103.txt
  testinput/pace_oc_l2.nc
  testinput/pace_radiance_L1B.nc
  testinput/modis_aqua_Rrs_OC_L2.nc
  testinput/mls_o3_l2.he5
  testinput/omi_o3_l2.he5
  testinput/ompsnm_o3_l2.he5
  testinput/sst_ostia.nc
  testinput/2021081612_RAOB_small.txt
  testinput/tropomi_co.nc
  testinput/TEMPO_no2_L2_V01_20231101T210834Z_S011G08.nc
  testinput/TEMPO_no2_L2_V01_20231101T211511Z_S011G09.nc
  testinput/TEMPO_no2_L2_V01_20231101T210157Z_S011G07.nc  
  testinput/gdas.t18z.abias_air
  testinput/MRMS_MergedReflectivityQC_01.50_20230712-152241.grib2
  testinput/OMPS-NPP_NMTO3-L2_v2.1_2020m0903t162415_small.h5
  testinput/OMPS-NPP_NMTO3-L2_v2.1_2020m0903t180544_small.h5
  testinput/viirs_j1_l1b_albedo_geoloc_202108050600.nc
  testinput/viirs_j1_l1b_albedo_obsval_202108050600.nc
  testinput/madis_2021010100.nc
)

list( APPEND test_output
  testoutput/20220216T00Z_PT3H_tms_tropics-01.nc4
  testoutput/amsr2_gcom-w1_20220216.nc4
  testoutput/gmi_gpm_20220216.nc4
  testoutput/icethk_coperl4.nc
  testoutput/nsidc_l4_icec.nc
  testoutput/amsr2_icec_l2p.nc
  testoutput/20220216T00Z_PT1H_amsr2_sample.nc4
  testoutput/gds2_sst_l2p.nc
  testoutput/gds2_sst_l3u.nc
  testoutput/giirs_fy4a_obs_2017030208.nc4
  testoutput/giirs_ssec_ioda.nc4
  testoutput/gmao_argo.nc
  testoutput/godae_prof.nc
  testoutput/godae_ship.nc
  testoutput/godae_trak.nc
  testoutput/rads_adt.nc
  testoutput/smap_sss_rss.nc
  testoutput/hgodas_adt.nc
  testoutput/hgodas_insitu.nc
  testoutput/hgodas_sst.nc
  testoutput/test_glider.nc
  testoutput/ndbc_hfradar_out.nc
  testoutput/sfc_tv_obs_2018041500.nc4
  testoutput/satwind_obs_2018041500.nc4
  testoutput/amsua_aqua_obs_2018041500.nc4
  testoutput/argoclim.nc
  testoutput/cryosat2_L2.nc
  testoutput/emc_ice_ioda2.nc
  testoutput/viirs_jpss1_oc_l2.nc
  testoutput/modis_aqua_oc_l2.nc
  testoutput/viirs_jpss1_oc_l3.nc
  testoutput/aod_viirs_obs_2018041500_s.nc4
  testoutput/abi_g16_obs_2018041500.nc4
  testoutput/ioda.NC001007.2020031012.nc
  testoutput/ioda_icartt_DC8_20230627_RA.nc
  testoutput/viirs_aod.nc
  testoutput/viirs_bias.nc
  testoutput/tropomi_no2_total.nc
  testoutput/tropomi_no2_tropo.nc
  testoutput/mopitt_co.nc
  testoutput/tropess_cris_co.nc
  testoutput/modis_aod.nc
  testoutput/2020100106_metars_small.nc
  testoutput/singleob.nc
  testoutput/ghcn_snod_20200228.nc
  testoutput/ionosonde_edp_20201003T203000Z.nc4
  testoutput/smap_ssm.nc
  testoutput/smap_nrt_ssm.nc
  testoutput/smos_ssm.nc
  testoutput/ascat_ssm.nc
  testoutput/smos_sss_l2.nc
  testoutput/airnow_2020081306.nc
  testoutput/aeronet_aod.nc
  testoutput/ioda_dt_global_twosat_phy_l4_20190101_vDT2021.nc
  testoutput/ioda_global_vavh_l3_rt_s3a_20210930T18.nc
  testoutput/SWOT_L2_ADT.nc
  testoutput/aeronet_aaod.nc
  testoutput/imsfv3_scf.nc
  testoutput/gnssro_cosmic2_2021080212.nc4
  testoutput/2022021600_atms_sdr.nc4
  testoutput/2022021600_atms_BGRemap_sdr.nc4
  testoutput/godae_bgc_argo.nc
  testoutput/owp_snow_obs_dup_thin_err_fn.nc
  testoutput/wmo_raob_double.nc4
  testoutput/wmo_raob_quadruple.nc4
  testoutput/avhrr_radiance.nc
  testoutput/satwinds_ssec2021080103.nc
  testoutput/pace_oc_l2.nc
  testoutput/pace_radiance_L1B.nc
  testoutput/modis_water_leaving_radiance_L2.nc
  testoutput/mls_o3_l2.nc
  testoutput/omi_o3_l2.nc
  testoutput/ompsnm_o3_l2.nc
  testoutput/sst_ostia.nc
  testoutput/2021081612_sonde_small.nc
  testoutput/tropomi_co_total.nc
  testoutput/tempo_no2_total.nc
  testoutput/tempo_no2_troposphere.nc
  testoutput/tempo_no2_stratosphere.nc
  testoutput/gdas.t18z.abias_air_ascent.csv
  testoutput/mrms_reflectivity_Z1500m.nc
  testoutput/omps_o3_nm.nc
  testoutput/viirs_j1_l1b_albedo_2021080506.nc
  testoutput/imsfv3grid_scf.nc
  testoutput/madis_snod_2021010100.nc
)

if( iodaconv_gnssro_ENABLED )
  list( APPEND test_input
    testinput/gnssro_3prof_2022090100.bufr
    testinput/gnssro_refractivityRetrieval_metop_romsaf_2401.0011_metopc-G32-202108020112.nc
    testinput/gnssaro_N49T_20210120_00Z.nc
  )

  list( APPEND test_output
    testoutput/gnssro_obs_3prof_2022090100.nc4
    testoutput/gnssro_obs_3prof_generic_2022090100.nc4
    testoutput/gnssro_obs_awsopendata_2021080200.nc4
    testoutput/gnssaro_obs_2021012000.nc
  )
endif()

if( iodaconv_bufr_query_ENABLED )
  list( APPEND test_input
    testinput/bufr_tables
    testinput/gdas.t12z.1bamua.tm00.bufr_d
    testinput/gdas.t12z.esamua.tm00.bufr_d
    testinput/gdas.t12z.1bmhs.tm00.bufr_d
    testinput/gdas.t00z.atms.tm00.bufr_d
    testinput/gdas.t12z.esmhs.tm00.bufr_d
    testinput/gdas.t12z.mtiasi.tm00.bufr_d
    testinput/gdas.t12z.adpupa.tm00.bufr_d
    testinput/gdas.t18z.1bmhs.tm00.bufr_d
    testinput/gdas.t00z.1bhrs4.tm00.bufr_d
    testinput/gdas.t00z.sevcsr.tm00.bufr_d
    testinput/gdas.t18z.satwnd_avhrr.tm00.bufr_d
    testinput/bufr_read_2_dim_blocks.bufr
    testinput/bufr_read_wmo_radiosonde.bufr
    testinput/bufr_satwnd_new_format.bufr
    testinput/bufr_satwnd_old_format.bufr
    testinput/bufr_simple_groupby.bufr
    testinput/bufr_empty_fields.bufr_d
    testinput/bufr_sfcshp.bufr
    testinput/ADPUPA.prepbufr
    testinput/gdas.t12z.adpupa_nc002103.tm00.bufr_d
    testinput/rtma_ru.t00z.msonet.tm00.bufr_d
    testinput/rtma_ru.t0000z.adpsfc_nc000101.tm00.bufr_d
    testinput/bufr_mhs.yaml
    testinput/bufr_hrs.yaml
    testinput/bufr_query_filtering.yaml
    testinput/bufr_filtering.yaml
    testinput/bufr_splitting.yaml
    testinput/bufr_filter_split.yaml
    testinput/bufr_ncep_prepbufr_adpsfc.yaml
    testinput/bufr_ncep_adpsfc.yaml
    testinput/gdas.t12z.adpsfc.prepbufr
    testinput/gdas.t00z.sfcshp.prepbufr
    testinput/gdas.t06z.adpsfc.tm00.bufr_d
    testinput/gdas.t12z.adpsfc.tm00.bufr_d
    testinput/gdas.t12z.aircar.tm00.bufr_d
    testinput/gdas.t12z.aircft.tm00.bufr_d
    testinput/gdas.t12z.acft_profiles.prepbufr
    testinput/bufr_ncep_snow_adpsfc.yaml
    testinput/bufr_read_2_dim_blocks.yaml
    testinput/bufr_ncep_1bamua_ta.yaml
    testinput/bufr_ncep_1bamua_n15.yaml
    testinput/bufr_ncep_1bmhs.yaml
    testinput/bufr_ncep_esamua.yaml
    testinput/bufr_ncep_esmhs.yaml
    testinput/bufr_ncep_highRes_sonde.yaml
    testinput/bufr_ncep_satwind_avhrr.yaml
    testinput/bufr_ncep_aircar.yaml
    testinput/bufr_ncep_aircft_AMDAR103.yaml
    testinput/bufr_ncep_aircft_noAMDAR103.yaml
    testinput/bufr_ncep_prepbufr_aircft.yaml
    testinput/bufr_read_wmo_radiosonde.yaml
    testinput/bufr_satwnd_old_format.yaml
    testinput/bufr_satwnd_new_format.yaml
    testinput/bufr_ncep_sevcsr.yaml
    testinput/bufr_ncep_mtiasi.yaml
    testinput/bufr_ncep_atms.yaml
    testinput/bufr_ncep_atms_remap.yaml
    testinput/bufr_ncep_atms_ta_remap.yaml
    testinput/bufr_simple_groupby.yaml
    testinput/bufr_empty_fields.yaml
    testinput/bufr_sfcshp.yaml
    testinput/bufr_ncep_adpupa.yaml
    testinput/bufr_ncep_prepbufr_adpupa.yaml
    testinput/prepbufr_adpsfc_api.py
    testinput/prepbufr_sfcshp_api.py
    testinput/bufr_ncep_prepbufr_adpupa.py
    testinput/aircar_BUFR2ioda.yaml
    testinput/gdas.aircar.t00z.20210801.bufr
    testinput/airep_wmoBUFR2ioda.yaml
    testinput/airep_wmo_multi.bufr
    testinput/bufr_wmo_amdar_multi.yaml
    testinput/amdar_wmo_multi.bufr
    testinput/gnssro_wmoBUFR2ioda.yaml
    testinput/gnssro_2020-306-2358C2E6.bufr
    testinput/buoy_wmoBUFR2ioda.yaml
    testinput/buoy_wmo_multi.bufr
    testinput/rass_wmoBUFR2ioda.yaml
    testinput/rass_wmo_multi.bufr
    testinput/ship_wmoBUFR2ioda.yaml
    testinput/ship_wmo_multi.bufr
    testinput/synop_wmoBUFR2ioda.yaml
    testinput/synop_wmo_multi.bufr
    testinput/satwind_EUMet_wmoBUFR2ioda.yaml
    testinput/satwind_EUMet_wmo.bufr
    testinput/satwind_Himawari_wmoBUFR2ioda.yaml
    testinput/satwind_Himawari_wmo.bufr
    testinput/satwind_Insat_wmoBUFR2ioda.yaml
    testinput/satwind_Insat_wmo.bufr
    testinput/vadwinds_wmoBUFR2ioda.yaml
    testinput/vadwinds_wmo_multi.bufr
    testinput/gdas.t12z.aircft.tm00.bufr_d
    testinput/bufr_specific_subsets_by_query.yaml
    testinput/bufr_specifying_subsets.yaml
    testinput/rap.t06z.lgycld.tm00.bufr_d
    testinput/rap.t06z.adpsfc.prepbufr.tm00
    testinput/bufr_ncep_lgycld_rrfs.yaml
    testinput/bufr_ncep_prepbufr_adpsfc_cloud_rrfs.yaml
    testinput/atms_beamwidth.txt
    testinput/bufr_query_python_test.py
    testinput/bufr_query_python_to_ioda_test.py
    testinput/bufr_query_fieldname_validation.py
    testinput/bufr_ncep_rtma_mesonet.yaml
    testinput/bufr_ncep_long_strs.yaml
    testinput/gdas.t06z.snocvr.tm00.bufr_d
    testinput/bufr_ncep_rtma_adpsfc.yaml
    testinput/rtma_ru.t0000z.aircft.tm00_nc004006.bufr_d
    testinput/rtma_ru.t0000z.aircft.tm00_nc004103.bufr_d
    testinput/bufr_ncep_rtma_aircft.yaml
    testinput/bufr_ncep_rtma_aircft_NC004103.yaml
    testinput/bufr_ncep_rtma_aircar.yaml
    testinput/rtma_ru.t0000z.aircar_NC004004.tm00.bufr_d
    testinput/bufr_ncep_rtma_prepbufr_adpupa.yaml
    testinput/rtma_ADPUPA.prepbufr
    testinput/bufr_ncep_rtma_adpupa.yaml
    testinput/rtma.t00z.adpupa.tm00.bufr_d
  )

  list( APPEND test_output
    testoutput/gdas.t18z.1bmhs.tm00.nc
    testoutput/gdas.t00z.1bhrs4.tm00.nc
    testoutput/gdas.t18z.1bmhs.tm00.filtering.nc
    testoutput/gdas.t18z.1bmhs.tm00.15.seven.split.nc
    testoutput/gdas.t18z.1bmhs.tm00.15.7.filter_split.nc
    testoutput/gdas.t12z.amsua_n15.tm00.nc
    testoutput/gdas.t12z.amsua_metop-c_ta.tm00.nc
    testoutput/gdas.t00z.atms_n20.tm00.nc
    testoutput/gdas.t12z.esamua_n18.tm00.nc
    testoutput/gdas.t12z.1bmhs.metop-b.tm00.nc
    testoutput/gdas.t12z.esmhs.noaa-19.tm00.nc
    testoutput/gdas.t12z.adpsfc.prepbufr.nc
    testoutput/prepbufr_adpsfc_api.nc
    testoutput/prepbufr_sfcshp_api.nc
    testoutput/gdas.t12z.adpupa_bufr.tm00.nc
    testoutput/gdas.t12z.adpsfc.tm00.nc
    testoutput/gdas.t06z.adpsfc_snow.tm00.nc
    testoutput/gdas.t12z.adpupa_bufr_nc002103.tm00.nc
    testoutput/gdas.t12z.aircar.tm00.nc
    testoutput/gdas.t12z.aircft_AMDAR103.tm00.nc
    testoutput/gdas.t12z.aircft_noAMDAR103.tm00.nc
    testoutput/gdas.t12z.acft_profiles.prepbufr.nc
    testoutput/gdas.t00z.sevcsr.tm00.nc
    testoutput/bufr_read_2_dim_blocks.nc
    testoutput/bufr_read_wmo_radiosonde.nc
    testoutput/NC005031.nc
    testoutput/NC005066.nc
    testoutput/gdas.t18z.satwnd_avhrr.tm00.nc
    testoutput/gdas.t12z.mtiasi_metop-c.tm00.nc
    testoutput/bufr_simple_groupby.nc
    testoutput/bufr_empty_fields.nc
    testoutput/bufr_sfcshp.nc
    testoutput/gdas.aircar.t00z.20210801.nc
    testoutput/airep_multi.nc
    testoutput/bufr_wmo_amdar_multi.nc
    testoutput/amdar_wmo_multi2.nc
    testoutput/gnssro_2020-306-2358C2E6.nc
    testoutput/buoy_wmo_multi.nc
    testoutput/buoy_wmo_multi2.nc
    testoutput/rass_wmo_multi.nc
    testoutput/ship_wmo_multi.nc
    testoutput/ship_wmo_multi2.nc
    testoutput/synop_wmo_multi.nc
    testoutput/synop_wmo_multi2.nc
    testoutput/satwind_EUMet.nc
    testoutput/satwind_Himawari.nc
    testoutput/satwind_Insat.nc
    testoutput/vadwinds_wmo_multi.nc
    testoutput/bufr_specifying_subsets.nc
    testoutput/rrfs.t06z.lgycld.bufr.nc
    testoutput/rrfs.t06z.adpsfc.prepbufr.nc
    testoutput/bufr_query_filtering.nc
    testoutput/gdas.t12z.adpupa.prepbufr.nc
    testoutput/prepbufr_adpupa_api.nc
    testoutput/bufr_query_python_to_ioda_test.nc
    testoutput/rtma_ru.t00z.msonet.tm00.nc
    testoutput/bufr_ncep_long_strs.nc
    testoutput/rtma_ru.t0000z.adpsfc_nc000101.tm00.nc
    testoutput/rtma_ru.t0000z.aircft.tm00_nc004006.nc
    testoutput/rtma_ru.t0000z.aircft.tm00_nc004103.nc
    testoutput/rtma_ru.t0000z.aircar_NC004004.tm00.nc
    testoutput/gdas.t00z.rtma_adpupa.prepbufr.nc
    testoutput/rtma.t00z.adpupa.tm00.nc
  )
endif()

if (iodaconv_gsi_varbc_ENABLED )
  list( APPEND test_input
    testinput/satbias_converter_amsua.yaml
    testinput/satbias_converter_cris.yaml
    testinput/satbias_converter_gmi_gpm.yaml
    testinput/satbias_converter_ssmis.yaml
    testinput/acftbias_converter.yaml
    testinput/satbias_crtm_in
    testinput/satbias_crtm_pc
  )

  list( APPEND test_output
    testoutput/varbc/satbias_amsua_n18.nc4
    testoutput/varbc/satbias_cris_npp.nc4
    testoutput/varbc/satbias_gmi_gpm.nc4
    testoutput/varbc/satbias_ssmis_f16.nc4
    testoutput/varbc/viirs_bias.nc
    testoutput/varbc/acft_out.nc4
  )
endif()

if (iodaconv_obserror_ENABLED )
  list( APPEND test_input
    testinput/Rcov_iasi_metop-b_sea.bin
  )

  list( APPEND test_output
    testoutput/Rcov_iasi_metop-b_sea.nc4
  )
endif()

if( iodaconv_pbfortran_ENABLED )
  list( APPEND test_input
    testinput/gnssro_obs_3prof_2022090100.bufr
    testinput/prepbufr.bufr
    testinput/gdas.t18z.1bmhs.tm00.bufr_d
  )

  list( APPEND test_output
    testoutput/gnssro_obs_2018041500.nc4
    testoutput/radiosonde_obs_2020093018.nc4
    testoutput/mhs_metop-b_obs_2020101215.nc4
    testoutput/mhs_metop-b_obs_2020110112.nc4
    testoutput/mhs_n19_obs_2020110112.nc4
    testoutput/amsua_metop-b_obs_2020110112.nc4
    testoutput/amsua_n18_obs_2020110112.nc4
    testoutput/iasi_metop-b_obs_2022021912.nc4
  )
endif()

# create test directories and make links to the input files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testoutput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testoutput/varbc)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testrun)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testrun/varbc)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Data)
foreach(FILENAME ${test_input} ${test_output})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

# create the share directory and link files to test subdir
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/share)
file( GLOB_RECURSE SHARE_FILES ${CMAKE_CURRENT_SOURCE_DIR}/../share/* )
foreach ( f ${SHARE_FILES} )
    get_filename_component(filename ${f} NAME)
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/../share/${filename}
           ${CMAKE_CURRENT_BINARY_DIR}/share/${filename} )
endforeach()

#===============================================================================
# The following tests use odb compare or nccmp to check the difference between the output
#  file, and the reference copy of what the ouput file should look like.
# The first argument to the script, is what the output file type is (netcdf or odb)
# The second rgument to the script, wrapped in quotes, is the command to execute
#  the ioda converter.
# The third argument is the name of the output file to compare (one copy to be
#  created in testrun/ by the converter, and one copy already present in in
#  testoutput/)
#===============================================================================

# Tolerence (relative percent difference) for comparing floating point numbers.
set(IODA_CONV_COMP_TOL "5.0e-4")

# The ioda-engines CMake configuration has set the file target _ioda_python to
# the full path to the library file containing the python bindings created
# by pybind11. For example:
#
#   path_to_lib/python3.9/ioda/_ioda_python.cpython-39-darwin.so
#
# To be able to import the ioda python module, PYTHONPATH needs to be
# set to the directory that is one above where the ioda module is located.
# Using the above example:
#
#   PYTHONPATH=path_to_lib/python3.9
#
# This can be accompilished by using the TARGET_FILE_DIR cmake generator
# which works like the shell dirname command. In the ecbuild_add_test command
# PYTHONPATH can be set using the ENVIRONMENT keyword. Note that the desired
# path is one directory above what TARGET_FILE_DIR will return in the example below.
#
#   ecbuild_add_test(
#        ...
#        ENVIRONMENT "PYTHONPATH=$<TARGET_FILE_DIR:_ioda_python>/../"
#        ...
#        )
#  Dom's fix to get right path

list(APPEND python_path
  "${PYIODACONV_BUILD_LIBDIR}/../"
  "${PYIODACONV_INSTALL_LIBDIR}/../"
  "$<TARGET_FILE_DIR:_ioda_python>/../../"
  "$ENV{PYTHONPATH}")

string(REPLACE ";" ":" python_path_str "${python_path}")
set(IODACONV_PYTHONPATH "${python_path_str}")

#===============================================================================
# Marine converters
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_nsidc_l4cdr_icec
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/nsidc_l4cdr_ice2ioda.py
                          -i testinput/nsidc_l4_icec.nc
                          -o testrun/nsidc_l4_icec.nc
                          -d 2019010112"
                          nsidc_l4_icec.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_copernicus_l4icethk
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/copernicus_l4icethk2ioda.py
                          -i testinput/icethk_coperl4.nc
                          -o testrun/icethk_coperl4.nc -d 2019011500"
                          icethk_coperl4.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_amsr2_l2icec
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/amsr2_icec2ioda.py
                          -i testinput/amsr2_icec_l2p.nc
                          -o testrun/amsr2_icec_l2p.nc
                          -d 2021080112"
                          amsr2_icec_l2p.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gds2_sst_l2p
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/gds2_sst2ioda.py
                          -i testinput/gds2_sst_l2p.nc
                          -o testrun/gds2_sst_l2p.nc
                          -d 2018041512
                          -t 0.5"
                          gds2_sst_l2p.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gds2_sst_l3u
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/gds2_sst2ioda.py
                          -i testinput/gds2_sst_l3u.nc
                          -o testrun/gds2_sst_l3u.nc
                          -d 2018041512
                          -t 0.5"
                          gds2_sst_l3u.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_smap_sss
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/smap_sss2ioda.py
                          -i testinput/smap_sss_rss.nc
                          -o testrun/smap_sss_rss.nc
                          -d 2018041512"
                          smap_sss_rss.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_rads_adt
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/rads_adt2ioda.py
                          -i testinput/rads_adt.nc
                          -o testrun/rads_adt.nc
                          -d 2018041512"
                          rads_adt.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gmao_argo
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/gmao_obs2ioda.py
                          -i testinput/gmao_argo.nc
                          -o testrun/gmao_argo.nc"
                          gmao_argo.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_godae_prof
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/godae_profile2ioda.py
                          -i testinput/godae_prof.bin
                          -o testrun/godae_prof.nc
                          -d 1998092212"
                          godae_prof.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_godae_ship
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/godae_ship2ioda.py
                          -i testinput/godae_ship.bin
                          -o testrun/godae_ship.nc
                          -d 1998090112"
                          godae_ship.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_godae_trak
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/godae_trak2ioda.py
                          -i testinput/godae_trak.bin
                          -o testrun/godae_trak.nc
                          -d 2004070812"
                          godae_trak.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_hgodas_adt
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/hgodas_adt2ioda.py
                          -i testinput/hgodas_adt.nc
                          -o testrun/hgodas_adt.nc
                          -d 2018041512"
                          hgodas_adt.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_hgodas_insitu
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/hgodas_insitu2ioda.py
                          -i testinput/hgodas_insitu.nc
                          -o testrun/hgodas_insitu.nc
                          -d 2018041512"
                          hgodas_insitu.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_hgodas_sst
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/hgodas_sst2ioda.py
                          -i testinput/hgodas_sst.nc
                          -o testrun/hgodas_sst.nc
                          -d 2018041512"
                          hgodas_sst.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_glider
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/glider2ioda.py
                          -i testinput/marineglider_AOML.nc
                          -o testrun/test_glider.nc
                          -d 2016080412
                          -y testinput/glider.yaml"
                          test_glider.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_hfradar
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/ndbc_hfradar2ioda.py
                          -i testinput/ndbc_hfradar_in.nc
                          -o testrun/ndbc_hfradar_out.nc
                          -d 2020072012"
                          ndbc_hfradar_out.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_argoclim
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/argoClim2ioda.py
                          -i testinput/argoclim_test.nc
                          -o testrun/argoclim.nc
                          -d 2004010200"
                          argoclim.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_cryosat2
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/cryosat_ice2ioda.py
                          -i testinput/cryosat2_L2_test.nc
                          -o testrun/cryosat2_L2.nc
                          -d 2019092112"
                          cryosat2_L2.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_emc_ice
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/emc_ice2ioda.py
                          -i testinput/emc_ice.nc
                          -o testrun/emc_ice_ioda2.nc
                          -d 2015082812"
                          emc_ice_ioda2.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_smos_sss
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/smos_sss2ioda.py
                          -i testinput/SM_REPR_MIR_OSUDP2_20100601T000131_20100601T001849_662_120_1.nc
                          -o testrun/smos_sss_l2.nc
                          -d 2010060100"
                          smos_sss_l2.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_copernicus_l4adt
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/copernicus_l4adt2ioda.py
                          -i testinput/dt_global_twosat_phy_l4_20190101_vDT2021.nc
                          -o testrun/ioda_dt_global_twosat_phy_l4_20190101_vDT2021.nc -d 20190101"
                          ioda_dt_global_twosat_phy_l4_20190101_vDT2021.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_copernicus_l3swh
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/copernicus_l3swh2ioda.py
                          -i testinput/global_vavh_l3_rt_s3a_20210930T18.nc
                          -o testrun/ioda_global_vavh_l3_rt_s3a_20210930T18.nc"
                          ioda_global_vavh_l3_rt_s3a_20210930T18.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_swot_l2adt
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/swot_l2adt2ioda.py
                          -i testinput/SWOT_L2_SSHA.nc
                          -o testrun/SWOT_L2_ADT.nc"
                          SWOT_L2_ADT.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_sst_ostia
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/ostia_l4sst2ioda.py
                          -i testinput/sst_ostia.nc
                          -o testrun/sst_ostia.nc"
                          sst_ostia.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_avhrr_radiance
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/avhrr_radiance2ioda.py
                          -i testinput/avhrr_radiance.nc
                          -o testrun/avhrr_radiance.nc
                          -d 2015010100"
                          avhrr_radiance.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_pace_oc_l2
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/pace_oc2ioda.py
                          -i testinput/pace_oc_l2.nc
                          -o testrun/pace_oc_l2.nc
                          -d 2019032112
                          -t 0.5"
                          pace_oc_l2.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_pace_radiance_L1B
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/pace_radiance2ioda.py
                          -i testinput/pace_radiance_L1B.nc
                          -o testrun/pace_radiance_L1B.nc
                          -d 2019032112"
                          pace_radiance_L1B.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_modis_water_leaving_radiance_L2                                               
                  TYPE    SCRIPT                                                                               
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"                                              
                  COMMAND bash                                                                                 
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh                                             
                          netcdf                                                                               
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/oc_l2_radiance2ioda.py                 
                          -i testinput/modis_aqua_Rrs_OC_L2.nc                                                    
                          -o testrun/modis_water_leaving_radiance_L2.nc                                                      
                          -d 2021080112"
                          modis_water_leaving_radiance_L2.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_mls_o3_l2
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/mls_o3_nc2ioda.py
                          -i testinput/mls_o3_l2.he5
                          -o testrun/mls_o3_l2.nc
                          -y 2022
                          -m 06
                          -d 21
                          -z 12"
                          mls_o3_l2.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_omi_o3_l2
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/omi_o3_nc2ioda.py
                          -i testinput/omi_o3_l2.he5
                          -o testrun/omi_o3_l2.nc
                          -y 2020
                          -m 12
                          -d 15
                          -z 12"
                          omi_o3_l2.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ompsnm_o3_l2
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/ompsnm_o3_nc2ioda.py
                          -i testinput/ompsnm_o3_l2.he5
                          -o testrun/ompsnm_o3_l2.nc
                          -y 2020
                          -m 12
                          -d 15
                          -z 12"
                          ompsnm_o3_l2.nc ${IODA_CONV_COMP_TOL})

#===============================================================================
# Conventional data, Sat-winds atmospheric motion vectors NESDIS ftp server
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_satwinds
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/amv_ssec_ascii2ioda.py
                          -i testinput/satwinds_ssec2021080103.txt
                          -o testrun/satwinds_ssec2021080103.nc
                          -d 2021080103"
                          satwinds_ssec2021080103.nc ${IODA_CONV_COMP_TOL})

#===============================================================================
# Conventional data, surface Obs - METAR converter
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_metar
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/metar_csv2ioda.py
                          -i testinput/2020100106_metars_small.csv
                          -o testrun/2020100106_metars_small.nc
                          -d 2020100106"
                          2020100106_metars_small.nc ${IODA_CONV_COMP_TOL})

#===============================================================================
# Conventional data, radiosonde, text/alpha (legacy) converter
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_sondetac
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/sonde_tac2ioda.py
                          -i testinput/2021081612_RAOB_small.txt
                          -o testrun/2021081612_sonde_small.nc
                          -t share/raob_stations_small.json
                          --date 2021-08-16T12:00:00Z --netcdf"
                          2021081612_sonde_small.nc ${IODA_CONV_COMP_TOL})

#===============================================================================
# MRMS (multi-radar, multi-sensor) radar reflectivity CONUS converter
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_mrms
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/mrms_grib2ioda.py
                          -i testinput/MRMS_MergedReflectivityQC_01.50_20230712-152241.grib2
                          -o testrun/mrms_reflectivity_Z1500m.nc"
                          mrms_reflectivity_Z1500m.nc ${IODA_CONV_COMP_TOL_ZERO})

#===============================================================================
# MISC converters
#===============================================================================

ecbuild_add_test( TARGET  test_iodaconv_tropics
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                  "${CMAKE_BINARY_DIR}/bin/tropics_2ioda.py
                  -i testinput/tropics_2022021600.nc
                  -o testrun/20220216T00Z_PT3H_tms_tropics-01.nc4
                  -d 2022021600"
                  20220216T00Z_PT3H_tms_tropics-01.nc4 ${IODA_CONV_COMP_TOL})
                  
ecbuild_add_test( TARGET  test_iodaconv_amsr2_gcom-w1
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                  "${CMAKE_BINARY_DIR}/bin/amsr2_2ioda.py
                  -i testinput/sample.amsr2.gcom-w1.20220216.h5
                  -o testrun/20220216T00Z_PT1H_amsr2_sample.nc4
                  -d 2022021600"
                  20220216T00Z_PT1H_amsr2_sample.nc4 ${IODA_CONV_COMP_TOL})
                  
ecbuild_add_test( TARGET  test_iodaconv_gmi_gpm
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                  "${CMAKE_BINARY_DIR}/bin/gmi_2ioda.py
                  -i testinput/gmi_gpm_20220216.h5
                  -o testrun/gmi_gpm_20220216.nc4
                  -d 2022021600"
                  gmi_gpm_20220216.nc4 ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_viirs_jpss1_oc_l2
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/viirs_modis_l2_oc2ioda.py
                          -i testinput/viirs_jpss1_oc_l2.nc
                          -o testrun/viirs_jpss1_oc_l2.nc
                          -d 2018041512
                          -t 0.5"
                          viirs_jpss1_oc_l2.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_modis_aqua_oc_l2
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/viirs_modis_l2_oc2ioda.py
                          -i testinput/modis_aqua_oc_l2.nc
                          -o testrun/modis_aqua_oc_l2.nc
                          -d 2018041512
                          -t 0.5"
                          modis_aqua_oc_l2.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_viirs_jpss1_oc_l3
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  TYPE    SCRIPT
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/viirs_modis_l3_oc2ioda.py
                          -i testinput/viirs_jpss1_oc_l3.nc
                          -o testrun/viirs_jpss1_oc_l3.nc
                          -d 2018041512"
                          viirs_jpss1_oc_l3.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_godae_bgc_argo
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/godae_bgc_argo2ioda.py
                          -i testinput/godae_bgc_argo.nc
                          -o testrun/godae_bgc_argo.nc
                          -d 2018041512"
                          godae_bgc_argo.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ncep_bufr
                  TYPE    SCRIPT
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/ncep_classes.py
                          -p testinput/
                          -i b001xx007.20200310.bufr
                          -o testrun/ioda.NC001007.2020031012.nc
                          -d 2020031012
                          -m 5
                          -l NC001007.yaml
                          -ot NC001007"
                          ioda.NC001007.2020031012.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gen_singleob
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/gen_single_ob.py
                          -y testinput/singleob.yaml"
                          singleob.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_gen_aircraft_bias_csv_python
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          ascii
                          "${Python3_EXECUTABLE} ${PROJECT_SOURCE_DIR}/src/gsi_varbc/generate_aircraft_bias_csv.py ./testinput/gdas.t18z.abias_air ./testrun/gdas.t18z.abias_air_ascent.csv MetaData/stationIdentification string 0 BiasCoefficientValue/ascentPredictor float 3"
                          gdas.t18z.abias_air_ascent.csv ${IODA_CONV_COMP_TOL})

#===============================================================================
# SSEC GIIRS converters
#===============================================================================

# use the non-zero tolerance for this test since the script is doing a variable change
ecbuild_add_test( TARGET  test_${PROJECT_NAME}_giirs_ssec2ioda
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/giirs_ssec2ioda.py
                          -i testinput/giirs_fy4a-test.nc
                          -o testrun/giirs_ssec_ioda.nc4
                          -d 2017030208"
                          giirs_ssec_ioda.nc4 ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_giirs_lw
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/giirs_lw2ioda.py
                          -i testinput/giirs_fy4a-test.nc
                          -o testrun/giirs_fy4a_obs_2017030208.nc4
                          -d 2017030208"
                          giirs_fy4a_obs_2017030208.nc4 ${IODA_CONV_COMP_TOL})

#===============================================================================
# GSI ncdiag converters
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gsidiag_conv_uv
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
                          -i testinput/gsidiag_conv_uv_testinput.nc
                          -o testrun/
                          -t conv
                          -p satwind"
                          satwind_obs_2018041500.nc4 ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gsidiag_conv
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
                          -i testinput/gsidiag_conv_t_sfc_test.nc
                          -o testrun/
                          -t conv
                          -p sfc"
                          sfc_tv_obs_2018041500.nc4 ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gsidiag_rad
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  TYPE    SCRIPT
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
                          -i testinput/gsidiag_amsua_aqua_radiance_test.nc
                          -o testrun/
                          -t rad"
                          amsua_aqua_obs_2018041500.nc4 ${IODA_CONV_COMP_TOL})

#===============================================================================
# WRFDA ncdiag converter
#===============================================================================

# DH* 20230217 - comment out, has not been updated with naming conventions
# see https://github.com/JCSDA-internal/ioda-converters/pull/1161
#  FIX or REMOVE ME
#ecbuild_add_test( TARGET  test_${PROJECT_NAME}_wrfdadiag_rad
#                  TYPE    SCRIPT
#                  COMMAND bash
#                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                          netcdf
#                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/test_wrfdadiag.py
#                          -i testinput/wrfdadiags_goes-16-abi_2018041500.nc
#                          -o testrun/
#                          -t rad"
#                          abi_g16_obs_2018041500.nc4 ${IODA_CONV_COMP_TOL})
# *DH

#===============================================================================
# GNSSRO BUFR converter
#===============================================================================

if( iodaconv_gnssro_ENABLED )
  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gnssro_bufr_conv
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/gnssro_bufr2ioda
                            2022090100
                            testinput/gnssro_3prof_2022090100.bufr
                            testrun/gnssro_obs_3prof_2022090100.nc4"
                            gnssro_obs_3prof_2022090100.nc4  ${IODA_CONV_COMP_TOL}
                    DEPENDS gnssro_bufr2ioda)

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gnssro_AWSopendataNetcdf_conv
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/gnssro_AWSopendataNetcdf2ioda.py
                            -i testinput/gnssro_refractivityRetrieval_metop_romsaf_2401.0011_metopc-G32-202108020112.nc
                            -o testrun/gnssro_obs_awsopendata_2021080200.nc4
                            -d 2021080200"
                            gnssro_obs_awsopendata_2021080200.nc4 ${IODA_CONV_COMP_TOL})

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gnssaro_netcdf_conv
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/gnssaro_netcdf2ioda.py
                            -i testinput/gnssaro_N49T_20210120_00Z.nc
                            -o testrun/gnssaro_obs_2021012000.nc
                            -d 2021012000"
                            gnssaro_obs_2021012000.nc ${IODA_CONV_COMP_TOL})
endif()

#==============================================================================
# Atmospheric Composition converters
#==============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_viirs_aod
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/viirs_aod2ioda.py
                          -i testinput/viirs_aod.nc
                          -o testrun/viirs_aod.nc
                          -m nesdis
                          -k maskout
                          -n 0.0"
                          viirs_aod.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_satbias_viirs_aod
                  TYPE    SCRIPT
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/viirs_biaswriter.py
                          -o testrun/varbc/viirs_bias.nc"
                          varbc/viirs_bias.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_icartt
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/icartt_nc2ioda.py
                          -i testinput/icartt_DC8_20230627_RA.nc
                          -o testrun/ioda_icartt_DC8_20230627_RA.nc"
                          ioda_icartt_DC8_20230627_RA.nc ${IODA_CONV_COMP_TOL})

foreach (coltype total tropo)
        ecbuild_add_test( TARGET  test_${PROJECT_NAME}_tropomi_no2_${coltype}
                          TYPE    SCRIPT
                          ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                          COMMAND bash
                          ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                                  netcdf
                                  "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/tropomi_no2_co_nc2ioda.py
                                  -i testinput/tropomi_no2.nc
                                  -o testrun/tropomi_no2_${coltype}.nc
                                  -v no2
                                  -c ${coltype}"
                                  tropomi_no2_${coltype}.nc ${IODA_CONV_COMP_TOL})
endforeach()

foreach (coltype total troposphere stratosphere)
  foreach (spec no2) #hcho not tested and o3 not out yet with real data
    if (NOT (${spec} STREQUAL "hcho" AND (${coltype} STREQUAL "total" OR ${coltype} STREQUAL "stratosphere")))
       ecbuild_add_test( TARGET  test_${PROJECT_NAME}_tempo_${spec}_${coltype}
                          TYPE    SCRIPT
                          ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                          COMMAND bash
                          ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                                  netcdf
                                  "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/tempo_nc2ioda.py
				  -i testinput/TEMPO_${spec}_L2_V01_20231101T210157Z_S011G07.nc
				     testinput/TEMPO_${spec}_L2_V01_20231101T210834Z_S011G08.nc
				     testinput/TEMPO_${spec}_L2_V01_20231101T211511Z_S011G09.nc
				  -o testrun/tempo_${spec}_${coltype}.nc
                                  -v ${spec}
                                  -c ${coltype}"
                                  tempo_${spec}_${coltype}.nc ${IODA_CONV_COMP_TOL})
    endif()
  endforeach()
endforeach()

# disable this test
ecbuild_add_test( TARGET  test_${PROJECT_NAME}_mopitt_co
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/mopitt_co_nc2ioda.py
                          -i testinput/mopitt_co.he5
                          -o testrun/mopitt_co.nc
                          -r 2021092903 2021092921"
                          mopitt_co.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_tropess_cris_co
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/tropess_co_nc2ioda.py
                          -i testinput/tropess_cris_co.nc
                          -o testrun/tropess_cris_co.nc
                          --levels 0 4 8 12"
                          tropess_cris_co.nc "5e-2")

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_modis_aod
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/modis_aod2ioda.py
                          -i testinput/modis_aod.hdf
                          -t 2021080100
                          -p Terra
                          -o testrun/modis_aod.nc"
                          modis_aod.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_aeronet_aod
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/aeronet_aod2ioda.py
                          -i testinput/aeronet_aod.dat
                          -o testrun/aeronet_aod.nc"
                          aeronet_aod.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_aeronet_aaod
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/aeronet_aaod2ioda.py
                          -c testinput/aeronet_cad.dat
                          -t testinput/aeronet_tab.dat
                          -o testrun/aeronet_aaod.nc"
                          aeronet_aaod.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_airnow
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/airnow2ioda_nc.py
                          -i testinput/airnow_2020081306.dat
                          -s testinput/airnow_sites.dat
                          -o testrun/airnow_2020081306.nc"
                          airnow_2020081306.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_tropomi_co_total
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/tropomi_no2_co_nc2ioda.py
                          -i testinput/tropomi_co.nc
                          -o testrun/tropomi_co_total.nc
                          -v co
                          -q 0.5
                          -c total"
                          tropomi_co_total.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_omps_o3_nm
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/omps_o3_nm_h52ioda.py
                          -i testinput/OMPS-NPP_NMTO3-L2_v2.1_2020m0903t162415_small.h5
                             testinput/OMPS-NPP_NMTO3-L2_v2.1_2020m0903t180544_small.h5
                          -o testrun/omps_o3_nm.nc
                          -q 128"
                          omps_o3_nm.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_viirs_j1_l1b_albedo
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/viirs_l1bnc2ioda.py
                          -i testinput/viirs_j1_l1b_albedo_obsval_202108050600.nc
                          -g testinput/viirs_j1_l1b_albedo_geoloc_202108050600.nc
                          --secterm
                          -o testrun/viirs_j1_l1b_albedo_2021080506.nc"
                          viirs_j1_l1b_albedo_2021080506.nc ${IODA_CONV_COMP_TOL})


#==============================================================================
# Bufr Ingester tests
#==============================================================================

if(iodaconv_bufr_query_ENABLED)

  ecbuild_add_test( TARGET  test_iodaconv_bufr_bufrdescription
                    SOURCES bufr/TestBufrDescription.cpp
                    ARGS    testinput/bufr_mhs.yaml
                    LIBS    eckit oops iodaconv::ingester)

  ecbuild_add_test( TARGET  test_iodaconv_bufr_mhs2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_mhs.yaml"
                            gdas.t18z.1bmhs.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_hrs2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_hrs.yaml"
                            gdas.t00z.1bhrs4.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_query_filtering
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_query_filtering.yaml"
                            bufr_query_filtering.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_filtering
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_filtering.yaml"
                            gdas.t18z.1bmhs.tm00.filtering.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_splitting
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_splitting.yaml"
                            gdas.t18z.1bmhs.tm00.15.seven.split.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_filter_split
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_filter_split.yaml"
                            gdas.t18z.1bmhs.tm00.15.7.filter_split.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_1bamua2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_1bamua_ta.yaml"
                            gdas.t12z.amsua_metop-c_ta.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_1bamua2ioda_n15
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_1bamua_n15.yaml"
                            gdas.t12z.amsua_n15.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_esamua2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_esamua.yaml"
                            gdas.t12z.esamua_n18.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_1bmhs2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_1bmhs.yaml"
                            gdas.t12z.1bmhs.metop-b.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_esmhs2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_esmhs.yaml"
                            gdas.t12z.esmhs.noaa-19.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_adpsfc2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_adpsfc.yaml"
                            gdas.t12z.adpsfc.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_prepbufr_ncep_api_adpsfc2ioda
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${PROJECT_SOURCE_DIR}/src/ncepbufr/prepbufr_adpsfc_api.py
                            -d 202108010000
                            -i testinput/gdas.t12z.adpsfc.prepbufr
                            -o testrun/prepbufr_adpsfc_api.nc"
                            prepbufr_adpsfc_api.nc ${IODA_CONV_COMP_TOL_ZERO} )


  ecbuild_add_test( TARGET  test_iodaconv_prepbufr_ncep_api_sfcshp2ioda
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${PROJECT_SOURCE_DIR}/src/ncepbufr/prepbufr_sfcshp_api.py
                            -d 202108010000
                            -i testinput/gdas.t00z.sfcshp.prepbufr
                            -o testrun/prepbufr_sfcshp_api.nc"
                            prepbufr_sfcshp_api.nc ${IODA_CONV_COMP_TOL_ZERO} )


  ecbuild_add_test( TARGET  test_iodaconv_prepbufr_ncep_adpsfc2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_prepbufr_adpsfc.yaml"
                            gdas.t12z.adpsfc.prepbufr.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_snowadpsfc2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_snow_adpsfc.yaml"
                            gdas.t06z.adpsfc_snow.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_aircar2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_aircar.yaml"
                            gdas.t12z.aircar.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_aircft_noamdar2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_aircft_noAMDAR103.yaml"
                            gdas.t12z.aircft_noAMDAR103.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_aircft_amdar2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_aircft_AMDAR103.yaml"
                            gdas.t12z.aircft_AMDAR103.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_prepbufr_ncep_aircftprofiles2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_prepbufr_aircft.yaml"
                            gdas.t12z.acft_profiles.prepbufr.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )


  ecbuild_add_test( TARGET  test_iodaconv_bufr_read_2_dim_blocks
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_read_2_dim_blocks.yaml"
                            bufr_read_2_dim_blocks.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_read_wmo_radiosonde
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_read_wmo_radiosonde.yaml"
                            bufr_read_wmo_radiosonde.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_satwnd_old_format
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_satwnd_old_format.yaml"
                            NC005066.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_satwnd_new_format
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_satwnd_new_format.yaml"
                            NC005031.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_satwind_avhrr
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_satwind_avhrr.yaml"
                            gdas.t18z.satwnd_avhrr.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_mtiasi
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_mtiasi.yaml"
                            gdas.t12z.mtiasi_metop-c.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_atms
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_atms.yaml"
                            gdas.t00z.atms_n20.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_simple_groupby
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_simple_groupby.yaml"
                            bufr_simple_groupby.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_empty_fields
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_empty_fields.yaml"
                            bufr_empty_fields.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_sfcshp
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_sfcshp.yaml"
                            bufr_sfcshp.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_prepbufr_adpupa
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_prepbufr_adpupa.yaml"
                            gdas.t12z.adpupa.prepbufr.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_prepbufr_adpupa_api
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${PROJECT_SOURCE_DIR}/src/ncepbufr/bufr_ncep_prepbufr_adpupa.py
                            -d 202108010000
                            -i testinput/ADPUPA.prepbufr
                            -o testrun/prepbufr_adpupa_api.nc"
                            prepbufr_adpupa_api.nc ${IODA_CONV_COMP_TOL_ZERO} )


  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_adpupa2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_adpupa.yaml"
                            gdas.t12z.adpupa_bufr.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_mesonet2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_rtma_mesonet.yaml"
                            rtma_ru.t00z.msonet.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_rtma_adpsfc2ioda
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_rtma_adpsfc.yaml"
                            rtma_ru.t0000z.adpsfc_nc000101.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

 ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_sevcsr
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_sevcsr.yaml"
                            gdas.t00z.sevcsr.tm00.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_specific_subsets_by_query
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_specific_subsets_by_query.yaml"
                            bufr_specifying_subsets.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_specifying_subsets
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_specifying_subsets.yaml"
                            bufr_specifying_subsets.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_lgycld
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_lgycld_rrfs.yaml"
                            rrfs.t06z.lgycld.bufr.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_prepbufr_adpsfc_cloud
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_prepbufr_adpsfc_cloud_rrfs.yaml"
                            rrfs.t06z.adpsfc.prepbufr.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_long_strs
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_long_strs.yaml"
                            bufr_ncep_long_strs.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_rtma_aircft
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_rtma_aircft.yaml"
                            rtma_ru.t0000z.aircft.tm00_nc004006.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_rtma_aircft_NC004103
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_rtma_aircft_NC004103.yaml"
                            rtma_ru.t0000z.aircft.tm00_nc004103.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_rtma_aircar
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_rtma_aircar.yaml"
                            rtma_ru.t0000z.aircar_NC004004.tm00.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_rtma_prepbufr_adpupa
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_rtma_prepbufr_adpupa.yaml"
                            gdas.t00z.rtma_adpupa.prepbufr.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_ncep_rtma_adpupa
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_ncep_rtma_adpupa.yaml"
                            rtma.t00z.adpupa.tm00.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

#  FIXME: Greg Thompson
#  ecbuild_add_test( TARGET  test_iodaconv_bufr_aircar
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/aircar_BUFR2ioda.yaml"
#                            gdas.aircar.t00z.20210801.nc ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2ioda.x )
#
#  ecbuild_add_test( TARGET  test_iodaconv_bufr_airep_wmo_bufr
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/airep_wmoBUFR2ioda.yaml"
#                            airep_multi.nc ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2ioda.x )
#
  ecbuild_add_test( TARGET  test_iodaconv_bufr_wmo_amdar_multi
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_wmo_amdar_multi.yaml"
                            bufr_wmo_amdar_multi.nc ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2ioda.x )
#
#  ecbuild_add_test( TARGET  test_iodaconv_bufr_gnssro_wmo_bufr
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/gnssro_wmoBUFR2ioda.yaml"
#                            gnssro_2020-306-2358C2E6.nc ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_generic_gnssro_bufr
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${PROJECT_SOURCE_DIR}/src/gnssro/gnssro_bufr2ioda.py
                            -d 2021080212
                            -i testinput/bfrPrf_C2E6.2021.214.12.00.G16_0001.0001_bufr
                            -o testrun/gnssro_cosmic2_2021080212.nc4"
                            gnssro_cosmic2_2021080212.nc4 ${IODA_CONV_COMP_TOL})

#  FIXME: Greg Thompson
#  ecbuild_add_test( TARGET  test_iodaconv_bufr_buoy_wmo_bufr
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/buoy_wmoBUFR2ioda.yaml"
#                            buoy_wmo_multi.nc ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2ioda.x )

#  ecbuild_add_test( TARGET  test_iodaconv_bufr_rass_wmo_bufr
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/rass_wmoBUFR2ioda.yaml"
#                            rass_wmo_multi.nc ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2ioda.x )
#
#  ecbuild_add_test( TARGET  test_iodaconv_bufr_ship_wmo_bufr
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/ship_wmoBUFR2ioda.yaml"
#                            ship_wmo_multi.nc ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2ioda.x )
#
#  ecbuild_add_test( TARGET  test_iodaconv_bufr_synop_wmo_bufr
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/synop_wmoBUFR2ioda.yaml"
#                            synop_wmo_multi.nc ${IODA_CONV_COMP_TOL})
#
#  ecbuild_add_test( TARGET  test_iodaconv_bufr_satwind_EUMet_wmo_bufr
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/satwind_EUMet_wmoBUFR2ioda.yaml"
#                            satwind_EUMet.nc ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2ioda.x )
#
#  ecbuild_add_test( TARGET  test_iodaconv_bufr_satwind_Himawari_wmo_bufr
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/satwind_Himawari_wmoBUFR2ioda.yaml"
#                            satwind_Himawari.nc ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2ioda.x )
#
#  ecbuild_add_test( TARGET  test_iodaconv_bufr_satwind_Insat_wmo_bufr
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/satwind_Insat_wmoBUFR2ioda.yaml"
#                            satwind_Insat.nc ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2ioda.x )
#
#  ecbuild_add_test( TARGET  test_iodaconv_bufr_vadwinds_wmo_bufr
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/vadwinds_wmoBUFR2ioda.yaml"
#                            vadwinds_wmo_multi.nc ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2ioda.x )
endif()

if ( iodaconv_bufr_python_ENABLED )

    ecbuild_add_test( TARGET  test_iodaconv_bufr_query_python
                      TYPE    SCRIPT
                      ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                      COMMAND "${Python3_EXECUTABLE}"
                      ARGS    "${PROJECT_SOURCE_DIR}/test/testinput/bufr_query_python_test.py" )

    ecbuild_add_test( TARGET  test_iodaconv_bufr_query_python_to_ioda
                      TYPE    SCRIPT
                      ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                      COMMAND bash
                      ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                              netcdf
                              "${Python3_EXECUTABLE} ${PROJECT_SOURCE_DIR}/test/testinput/bufr_query_python_to_ioda_test.py"
                      bufr_query_python_to_ioda_test.nc ${IODA_CONV_COMP_TOL})

    ecbuild_add_test( TARGET  test_iodaconv_bufr_query_fieldname_validation
                      TYPE    SCRIPT
                      ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                      COMMAND "${Python3_EXECUTABLE}"
                      ARGS "${PROJECT_SOURCE_DIR}/test/testinput/bufr_query_fieldname_validation.py" )

endif()


#==============================================================================
# Converters for NRT ingest
#==============================================================================

ecbuild_add_test( TARGET  test_iodaconv_atms_nc4
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${PROJECT_SOURCE_DIR}/src/hdf5/atms_netcdf_hdf5_2ioda.py
                          -d 2022021600
                          -i testinput/sample.SNDR.SNPP.ATMS.2022021600.nc4
                          -o testrun/2022021600_atms_sdr.nc4" 2022021600_atms_sdr.nc4 ${IODA_CONV_COMP_TOL} )

ecbuild_add_test( TARGET  test_iodaconv_atms_BGRemap_nc4
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${PROJECT_SOURCE_DIR}/src/hdf5/atms_netcdf_hdf5_2ioda.py
                          -m BG
                          -d 2022021600
                          -i testinput/sample.SNDR.J1.ATMS.2022021600.nc4
                          -o testrun/2022021600_atms_BGRemap_sdr.nc4" 2022021600_atms_BGRemap_sdr.nc4 ${IODA_CONV_COMP_TOL} )

if( iodaconv_eccodes_ENABLED )

#==============================================================================
# Generic NRT Ingest tests
#==============================================================================

  ecbuild_add_test(TARGET  test_${PROJECT_NAME}_generic_bufr_raob
                   TYPE    SCRIPT
                   ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                   COMMAND bash
                   ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                           netcdf
                           "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/sonde_bufr2ioda.py
                           -i testinput/sonde_wmo_double.bufr
                           -o testrun/wmo_raob_double.nc4
                           -t share/raob_stations_small.json
                           --date 2021-02-01T12:00:00Z"
                           wmo_raob_double.nc4 ${IODA_CONV_COMP_TOL})

endif()

if( iodaconv_bufr_query_ENABLED )
#===============================================================================
# Conventional data, aircraft Obs - AMDAR BUFR (WMO) converter
#===============================================================================

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_amdar
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/amdar_bufr2ioda.py
                            -i testinput/amdar_wmo_multi.bufr
                            -o testrun/amdar_wmo_multi2.nc"
                            amdar_wmo_multi2.nc ${IODA_CONV_COMP_TOL})

#===============================================================================
# Conventional data, surface Obs - SYNOP, Buoy and Ship BUFR (WMO) converters
#===============================================================================

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_buoy
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/buoy_bufr2ioda.py
                            -i testinput/buoy_wmo_multi.bufr
                            -o testrun/buoy_wmo_multi2.nc"
                            buoy_wmo_multi2.nc ${IODA_CONV_COMP_TOL})

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ship
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/ship_bufr2ioda.py
                            -i testinput/ship_wmo_multi.bufr
                            -o testrun/ship_wmo_multi2.nc"
                            ship_wmo_multi2.nc ${IODA_CONV_COMP_TOL})

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_synop
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/synop_bufr2ioda.py
                            -i testinput/synop_wmo_multi.bufr
                            -o testrun/synop_wmo_multi2.nc"
                            synop_wmo_multi2.nc ${IODA_CONV_COMP_TOL})

endif()

#==============================================================================
# PrepBUFR and BUFR tests
#==============================================================================

if( iodaconv_pbfortran_ENABLED )

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_prepbufr_conv
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2nc_fortran.x
                            -i testinput -o testrun prepbufr.bufr"
                            radiosonde_obs_2020093018.nc4 ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2nc_fortran.x)

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_mhs_conv
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2nc_fortran.x
                            -i testinput -o testrun gdas.t18z.1bmhs.tm00.bufr_d"
                            mhs_metop-b_obs_2020101215.nc4 ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2nc_fortran.x)

#  FIXME: Emily Liu
#  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ears_mhs_conv
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2nc_fortran.x
#                            -i testinput -o testrun gdas.t12z.esmhs.tm00.bufr_d"
#                            mhs_n19_obs_2020110112.nc4 ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2nc_fortran.x)

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_amsua_conv
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2nc_fortran.x
                            -i testinput -o testrun gdas.t12z.1bamua.tm00.bufr_d"
                            amsua_metop-b_obs_2020110112.nc4 ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2nc_fortran.x)

#  FIXME: Emily Liu
#  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ears_amsua_conv
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2nc_fortran.x
#                            -i testinput -o testrun gdas.t12z.esamua.tm00.bufr_d"
#                            amsua_n18_obs_2020110112.nc4 ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2nc_fortran.x)

#  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_iasi_conv
#                    TYPE    SCRIPT
#                    COMMAND bash
#                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
#                            netcdf
#                            "${CMAKE_BINARY_DIR}/bin/bufr2nc_fortran.x
#                            -i testinput -o testrun gdas.t12z.mtiasi.tm00.bufr_d"
#                            iasi_metop-b_obs_2022021912.nc4 ${IODA_CONV_COMP_TOL}
#                    DEPENDS bufr2nc_fortran.x)

endif()


#==============================================================================
# SatBias tests
#==============================================================================

if( iodaconv_gsi_varbc_ENABLED )

  ecbuild_add_test( TARGET  test_iodaconv_satbias_amsua
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/satbias2ioda.x testinput/satbias_converter_amsua.yaml"
                            varbc/satbias_amsua_n18.nc4 ${IODA_CONV_COMP_TOL}
                    DEPENDS satbias2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_satbias_cris
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/satbias2ioda.x testinput/satbias_converter_cris.yaml"
                            varbc/satbias_cris_npp.nc4 ${IODA_CONV_COMP_TOL}
                    DEPENDS satbias2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_satbias_gmi
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/satbias2ioda.x testinput/satbias_converter_gmi_gpm.yaml"
                            varbc/satbias_gmi_gpm.nc4 ${IODA_CONV_COMP_TOL}

                    DEPENDS satbias2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_satbias_ssmis
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/satbias2ioda.x testinput/satbias_converter_ssmis.yaml"
                            varbc/satbias_ssmis_f16.nc4 ${IODA_CONV_COMP_TOL_ZERO}
                            satbias_ssmis_f16.nc4 ${IODA_CONV_COMP_TOL}
                    DEPENDS satbias2ioda.x )
  ecbuild_add_test( TARGET  test_iodaconv_acftbias_gsi
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/acftbias2ioda.x testinput/acftbias_converter.yaml"
                            varbc/acft_out.nc4 ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS acftbias2ioda.x )
endif()


#==============================================================================
# ObsError correlations tests
#==============================================================================

if( iodaconv_obserror_ENABLED )

  ecbuild_add_test( TARGET  test_iodaconv_obserror
                    TYPE    SCRIPT
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${CMAKE_BINARY_DIR}/bin/obserror2ioda.x testinput/Rcov_iasi_metop-b_sea.bin testrun/Rcov_iasi_metop-b_sea.nc4"
                            Rcov_iasi_metop-b_sea.nc4 ${IODA_CONV_COMP_TOL}
                    DEPENDS obserror2ioda.x )
endif()


# =============================================================================
# Land product converters
#==============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ghcn_snod
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/ghcn_snod2ioda.py
                          -i testinput/ghcn_20200228.csv
                          -o testrun/ghcn_snod_20200228.nc
                          -f ${PROJECT_BINARY_DIR}/doc/fix/ghcn-stations.txt
                          -d 2020022818
                          -m maskout"
                          ghcn_snod_20200228.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ionosonde_edp
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/ionosonde_ascii2ioda.py
                            -i testinput/AL945_2020277203000.EDP
                            -o testrun/ionosonde_edp_20201003T203000Z.nc4"
                            ionosonde_edp_20201003T203000Z.nc4 ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_smap_nrt_ssm
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/smap_ssm2ioda.py
                            -i testinput/SMAP_L2_SM_P_NRT_24342_A_20190822T224858_N16023_002.h5
                            -o testrun/smap_nrt_ssm.nc
                            --maskMissing"
                            smap_nrt_ssm.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_smos_ssm
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/smos_ssm2ioda.py
                            -i testinput/smos_l2nrt_ssm.nc
                            -o testrun/smos_ssm.nc
                            -m maskout"
                            smos_ssm.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ascat_ssm
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/ascat_ssm2ioda.py
                            -i testinput/ascat_ssm.nc
                            -o testrun/ascat_ssm.nc
                            -m maskout"
                            ascat_ssm.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_imsfv3_scf
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/imsfv3_scf2ioda.py
                            -i testinput/ascii_input_IMSscf.20230501.oro_C48.nc
                            -o testrun/imsfv3_scf.nc"
                            imsfv3_scf.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_smap_ssm
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/smap_ssm2ioda.py
                            -i testinput/SMAP_L2_SM_P_E_36365_D_20211122T013945_R18240_001.h5
                            -o testrun/smap_ssm.nc
                            --maskMissing"
                            smap_ssm.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_land_owp_snow_obs_dup_thin
                  TYPE    SCRIPT
                  ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                  COMMAND bash
                  ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                          netcdf
                          "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/owp_snow_obs.py
                          -i testinput/owp_snow_obs.csv
                          -o testrun/owp_snow_obs_dup_thin_err_fn.nc
                          --thin_swe 1 --thin_depth .5 --thin_random_seed 3  --err_fn dummy_err"
                          owp_snow_obs_dup_thin_err_fn.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_imsfv3grid_scf
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/imsfv3_scf2ioda.py
                            -i testinput/nc_input_IMSscf.20230501.oro_C48.nc
                            -o testrun/imsfv3grid_scf.nc"
                            imsfv3grid_scf.nc ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_madis_snod
                    TYPE    SCRIPT
                    ENVIRONMENT "PYTHONPATH=${IODACONV_PYTHONPATH}"
                    COMMAND bash
                    ARGS    ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                            netcdf
                            "${Python3_EXECUTABLE} ${CMAKE_BINARY_DIR}/bin/madis_snow2ioda.py
                            -i testinput/madis_2021010100.nc
                            -o testrun/madis_snod_2021010100.nc"
                            madis_snod_2021010100.nc ${IODA_CONV_COMP_TOL})

#######################################################
# Auto-generated tests for ioda file validation
#  For these tests we check the testoutput directory
#  since it already exists with valid data.

file( GLOB TESTOUTPUT_IODA_FILES testoutput/*.nc4 testoutput/*.nc testoutput/*.ioda )

foreach ( f ${TESTOUTPUT_IODA_FILES} )
    get_filename_component(filename ${f} NAME)

    # Note: IODA_YAML_ROOT is provided by find_package(ioda).
    # The ObsSpace.yaml file is *not* a test file. It always exists.
    ecbuild_add_test(
        TARGET ${PROJECT_NAME}_validate_testoutput_${filename}
        COMMAND ioda-validate.x
        LABELS ${PROJECT_NAME}_validate
        ENVIRONMENT "ECKIT_COLOUR_OUTPUT=1"
        ARGS "--ignore-warn"
             "--ignore-error"
             "${IODA_YAML_ROOT}/validation/ObsSpace.yaml"
             "${f}"
    )
endforeach()
