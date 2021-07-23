# (C) Copyright 2019-2021 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# IODA-CONVERTER tests
################################################################################

list( APPEND test_input
  testinput/gds2_sst_l2p.nc
  testinput/gds2_sst_l3u.nc
  testinput/giirs_fy4a-test.nc
  testinput/godae_prof.bin
  testinput/godae_ship.bin
  testinput/godae_trak.bin
  testinput/rads_adt.nc
  testinput/smap_sss_rss.nc
  testinput/hgodas_insitu.nc
  testinput/hgodas_sst.nc
  testinput/marineglider_AOML.nc
  testinput/ndbc_hfradar_in.nc
  testinput/gsidiag_conv_t_sfc_test.nc
  testinput/gsidiag_conv_uv_testinput.nc
  testinput/gsidiag_amsua_aqua_radiance_test.nc
  testinput/argoclim_test.nc
  testinput/cryosat2_L2_test.nc
  testinput/viirs_jpss1_oc_l2.nc
  testinput/modis_aqua_oc_l2.nc
  testinput/viirs_jpss1_oc_l3.nc
  testinput/sondes_obs_2018041500_m.nc4
  testinput/gnssro_obs_2018041500_s.nc4
  testinput/wrfdadiags_goes-16-abi_2018041500.nc
  testinput/b001xx007.20200310.bufr
  testinput/viirs_aod.nc
  testinput/tropomi_no2.nc
  testinput/modis_aod.nc
  testinput/imssnow_24km.grib2
  testinput/2020100106_metars_small.csv
  testinput/afwa_snod_24km.grib
  testinput/singleob.yaml
  testinput/ghcn_20200228.csv
  testinput/SMAP_L2_SM_P_NRT_24342_A_20190822T224858_N16023_002.h5
  testinput/smos_l2nrt_ssm.nc
  testinput/ascat_ssm.nc
  testinput/SM_REPR_MIR_OSUDP2_20100601T000131_20100601T001849_662_120_1.nc
  testinput/airnow_sites.dat
  testinput/airnow_2020081306.dat
)

list( APPEND test_output
  testoutput/gds2_sst_l2p.nc
  testoutput/gds2_sst_l3u.nc
  testoutput/giirs_fy4a_obs_2017030208.nc4
  testoutput/giirs_ssec_ioda.nc4
  testoutput/godae_prof.nc
  testoutput/godae_ship.nc
  testoutput/godae_trak.nc
  testoutput/rads_adt.nc
  testoutput/smap_sss_rss.nc
  testoutput/hgodas_insitu.nc
  testoutput/hgodas_sst.nc
  testoutput/test_glider.nc
  testoutput/ndbc_hfradar_out.nc
  testoutput/sfc_tv_obs_2018041500.nc4
  testoutput/satwind_obs_2018041500.nc4
  testoutput/amsua_aqua_obs_2018041500.nc4
  testoutput/argoclim.nc
  testoutput/cryosat2_L2.nc
  testoutput/viirs_jpss1_oc_l2.nc
  testoutput/modis_aqua_oc_l2.nc
  testoutput/viirs_jpss1_oc_l3.nc
  testoutput/aod_viirs_obs_2018041500_s.nc4
  testoutput/abi_g16_obs_2018041500.nc4
  testoutput/ioda.NC001007.2020031012.nc
  testoutput/viirs_aod.nc
  testoutput/tropomi_no2.nc
  testoutput/modis_aod.nc 
  testoutput/imssnow_scf.nc
  testoutput/2020100106_metars_small.nc
  testoutput/afwa_snod.nc
  testoutput/singleob.nc
  testoutput/ghcn_snod_20200228.nc
  testoutput/smap_ssm.nc
  testoutput/smos_ssm.nc
  testoutput/ascat_ssm.nc
  testoutput/smos_sss_l2.nc
  testoutput/airnow_2020081306.nc
)

if( iodaconv_gnssro_ENABLED )
  list( APPEND test_input
    testinput/gnssro_kompsat5_20180415_00Z.bufr
  )

  list( APPEND test_output
    testoutput/gnssro_kompsat5_2018041500.nc4
  )
endif()

if( iodaconv_bufr_ENABLED )
  list( APPEND test_input
    testinput/bufr_tables
    testinput/gdas.t18z.1bmhs.tm00.bufr_d
    testinput/gdas.t00z.1bhrs4.tm00.bufr_d
    testinput/gdas.t06z.adpsfc.tm00.bufr_d
    testinput/bufr_read_2_dim_blocks.bufr
    testinput/bufr_read_wmo_radiosonde.bufr
    testinput/bufr_satwnd_new_format.bufr
    testinput/bufr_satwnd_old_format.bufr
    testinput/bufr_mhs.yaml
    testinput/bufr_hrs.yaml
    testinput/bufr_filtering.yaml
    testinput/bufr_splitting.yaml
    testinput/bufr_filter_split.yaml
    testinput/bufr_adpsfc.yaml
    testinput/bufr_snow_adpsfc.yaml
    testinput/bufr_read_2_dim_blocks.yaml
    testinput/bufr_read_wmo_radiosonde.yaml
    testinput/bufr_satwnd_old_format.yaml
    testinput/bufr_satwnd_new_format.yaml
  )

  list( APPEND test_output
    testoutput/gdas.t18z.1bmhs.tm00.nc
    testoutput/gdas.t00z.1bhrs4.tm00.nc
    testoutput/gdas.t18z.1bmhs.tm00.filtering.nc
    testoutput/gdas.t18z.1bmhs.tm00.15.seven.split.nc
    testoutput/gdas.t18z.1bmhs.tm00.15.7.filter_split.nc
    testoutput/gdas.t06z.adpsfc.tm00.nc
    testoutput/gdas.t06z.adpsfc_snow.tm00.nc
    testoutput/bufr_read_2_dim_blocks.nc
    testoutput/bufr_read_wmo_radiosonde.nc
    testoutput/NC005031.nc
    testoutput/NC005066.nc
  )
endif()

if (iodaconv_satbias_ENABLED )
    list( APPEND test_input
    testinput/satbias_converter_amsua.yaml
    testinput/satbias_converter_cris.yaml
    testinput/satbias_converter_ssmis.yaml
    testinput/satbias_crtm_in
    testinput/satbias_crtm_pc
  )

  list( APPEND test_output
    testoutput/satbias_amsua_n18.nc4
    testoutput/satbias_cris_npp.nc4
    testoutput/satbias_ssmis_f16.nc4
  )
endif()


if( iodaconv_pbfortran_ENABLED )
  list( APPEND test_input
    testinput/gnssro_kompsat5_20180415_00Z.bufr
    testinput/prepbufr.bufr
    testinput/gdas.t18z.1bmhs.tm00.bufr_d
  )

  list( APPEND test_output
    testoutput/gnssro_obs_2018041500.nc4
    testoutput/sondes_obs_2020093018.nc4
    testoutput/mhs_metop-b_obs_2020101215.nc4
  )
endif()

# create test directories and make links to the input files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testoutput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testrun)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Data)
foreach(FILENAME ${test_input} ${test_output})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

#===============================================================================
# The following tests use odb compare or nccmp to check the difference between the output
#  file, and the reference copy of what the ouput file should look like.
# The first rgument to the script, is what the output file type is (netcdf or odb)
# The second rgument to the script, wrapped in quotes, is the command to execute
#  the ioda converter.
# The third argument is the name of the output file to compare (one copy to be
#  created in testrun/ by the converter, and one copy already present in in
#  testoutput/)
#===============================================================================

# Typically, a converter is simply doing a format change (ie, copying data directly
# from the input file). For these cases, use a tolerance of zero for the
# iodaconv_comp.sh test.
#
# For a converter that is generating new data, use a non-zero tolerance.
set(IODA_CONV_COMP_TOL_ZERO "0.0")
set(IODA_CONV_COMP_TOL "0.5e-4")

#===============================================================================
# Marine converters
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gds2_sst_l2p
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/gds2_sst2ioda.py
                          -i testinput/gds2_sst_l2p.nc
                          -o testrun/gds2_sst_l2p.nc
                          -d 2018041512
                          -t 0.5"
                          gds2_sst_l2p.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gds2_sst_l3u
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/gds2_sst2ioda.py
                          -i testinput/gds2_sst_l3u.nc
                          -o testrun/gds2_sst_l3u.nc
                          -d 2018041512
                          -t 0.5"
                          gds2_sst_l3u.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_smap_sss
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/smap_sss2ioda.py
                          -i testinput/smap_sss_rss.nc
                          -o testrun/smap_sss_rss.nc
                          -d 2018041512"
                          smap_sss_rss.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_rads_adt
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/rads_adt2ioda.py
                          -i testinput/rads_adt.nc
                          -o testrun/rads_adt.nc
                          -d 2018041512"
                          rads_adt.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_godae_prof
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/godae_profile2ioda.py
                          -i testinput/godae_prof.bin
                          -o testrun/godae_prof.nc
                          -d 1998092212"
                          godae_prof.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_godae_ship
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/godae_ship2ioda.py
                          -i testinput/godae_ship.bin
                          -o testrun/godae_ship.nc
                          -d 1998090112"
                          godae_ship.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_godae_trak
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/godae_trak2ioda.py
                          -i testinput/godae_trak.bin
                          -o testrun/godae_trak.nc
                          -d 2004070812"
                          godae_trak.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_hgodas_insitu
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/hgodas_insitu2ioda.py
                          -i testinput/hgodas_insitu.nc
                          -o testrun/hgodas_insitu.nc
                          -d 2018041512"
                          hgodas_insitu.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_hgodas_sst
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/hgodas_sst2ioda.py
                          -i testinput/hgodas_sst.nc
                          -o testrun/hgodas_sst.nc
                          -d 2018041512"
                          hgodas_sst.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_glider
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/glider2ioda.py
                          -i testinput/marineglider_AOML.nc
                          -o testrun/test_glider.nc
                          -d 2016080412"
                          test_glider.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_hfradar
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/ndbc_hfradar2ioda.py
                          -i testinput/ndbc_hfradar_in.nc
                          -o testrun/ndbc_hfradar_out.nc
                          -d 2020072012"
                          ndbc_hfradar_out.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_argoclim
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/argoClim2ioda.py
                          -i testinput/argoclim_test.nc
                          -o testrun/argoclim.nc
                          -d 2019101600"
                          argoclim.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_cryosat2
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/cryosat_ice2ioda.py
                          -i testinput/cryosat2_L2_test.nc
                          -o testrun/cryosat2_L2.nc
                          -d 2019092112"
                          cryosat2_L2.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_smos_sss
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/smos_sss2ioda.py
                          -i testinput/SM_REPR_MIR_OSUDP2_20100601T000131_20100601T001849_662_120_1.nc
                          -o testrun/smos_sss_l2.nc
                          -d 2010060100"
                          smos_sss_l2.nc ${IODA_CONV_COMP_TOL_ZERO})

#===============================================================================
# Conventional data, surface Obs - METAR converter
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_metar
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/metar_csv2ioda.py
                          -i testinput/2020100106_metars_small.csv
                          -o testrun/2020100106_metars_small.nc
                          -d 2020100106"
                          2020100106_metars_small.nc ${IODA_CONV_COMP_TOL_ZERO})

#===============================================================================
# MISC converters
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_viirs_jpss1_oc_l2
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/viirs_modis_l2_oc2ioda.py
                          -i testinput/viirs_jpss1_oc_l2.nc
                          -o testrun/viirs_jpss1_oc_l2.nc
                          -d 2018041512
                          -t 0.5"
                          viirs_jpss1_oc_l2.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_modis_aqua_oc_l2
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/viirs_modis_l2_oc2ioda.py
                          -i testinput/modis_aqua_oc_l2.nc
                          -o testrun/modis_aqua_oc_l2.nc
                          -d 2018041512
                          -t 0.5"
                          modis_aqua_oc_l2.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_viirs_jpss1_oc_l3
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/viirs_modis_l3_oc2ioda.py
                          -i testinput/viirs_jpss1_oc_l3.nc
                          -o testrun/viirs_jpss1_oc_l3.nc
                          -d 2018041512"
                          viirs_jpss1_oc_l3.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ncep_bufr
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/ncep_classes.py
                          -p testinput/
                          -i b001xx007.20200310.bufr
                          -o testrun/ioda.NC001007.2020031012.nc
                          -d 2020031012
                          -m 5
                          -l NC001007.yaml
                          -ot NC001007"
                          ioda.NC001007.2020031012.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gen_singleob
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/gen_single_ob.py
                          -y testinput/singleob.yaml"
                          singleob.nc ${IODA_CONV_COMP_TOL_ZERO})

#===============================================================================
# SSEC GIIRS converters
#===============================================================================

# use the non-zero tolerance for this test since the script is doing a variable change
ecbuild_add_test( TARGET  test_${PROJECT_NAME}_giirs_ssec2ioda
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/giirs_ssec2ioda.py
                          -i testinput/giirs_fy4a-test.nc
                          -o testrun/giirs_ssec_ioda.nc4
                          -d 2017030208"
                          giirs_ssec_ioda.nc4 ${IODA_CONV_COMP_TOL})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_giirs_lw
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/giirs_lw2ioda.py
                          -i testinput/giirs_fy4a-test.nc
                          -o testrun/giirs_fy4a_obs_2017030208.nc4
                          -d 2017030208"
                          giirs_fy4a_obs_2017030208.nc4 ${IODA_CONV_COMP_TOL_ZERO})

#===============================================================================
# GSI ncdiag converters
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gsidiag_conv_uv
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
                          -i testinput/gsidiag_conv_uv_testinput.nc
                          -o testrun/
                          -t conv
                          -p satwind"
                          satwind_obs_2018041500.nc4 ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gsidiag_conv
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
                          -i testinput/gsidiag_conv_t_sfc_test.nc
                          -o testrun/
                          -t conv
                          -p sfc"
                          sfc_tv_obs_2018041500.nc4 ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gsidiag_rad
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
                          -i testinput/gsidiag_amsua_aqua_radiance_test.nc
                          -o testrun/
                          -t rad"
                          amsua_aqua_obs_2018041500.nc4 ${IODA_CONV_COMP_TOL_ZERO})

#===============================================================================
# WRFDA ncdiag converter
#===============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_wrfdadiag_rad
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/test_wrfdadiag.py
                          -i testinput/wrfdadiags_goes-16-abi_2018041500.nc
                          -o testrun/
                          -t rad"
                          abi_g16_obs_2018041500.nc4 ${IODA_CONV_COMP_TOL_ZERO})

#===============================================================================
# GNSSRO BUFR converter
#===============================================================================

if( iodaconv_gnssro_ENABLED )
  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gnssro_bufr_conv
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/gnssro_bufr2ioda
                            2018041500
                            testinput/gnssro_kompsat5_20180415_00Z.bufr
                            testrun/gnssro_kompsat5_2018041500.nc4"
                            gnssro_kompsat5_2018041500.nc4 ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS gnssro_bufr2ioda)
endif()

#==============================================================================
# Atmospheric Composition converters
#==============================================================================

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_viirs_aod
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/viirs_aod2ioda.py
                          -i testinput/viirs_aod.nc
                          -o testrun/viirs_aod.nc
                          -m nesdis
                          -k maskout
                          -t 0.0"
                          viirs_aod.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_tropomi_no2
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/tropomi_no2_nc2ioda.py
                          -i testinput/tropomi_no2.nc
                          -o testrun/tropomi_no2.nc"
                          tropomi_no2.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_modis_aod
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/modis_aod2ioda.py
                          -i testinput/modis_aod.nc 
                          -t 2016060100 
                          -o testrun/modis_aod.nc"
                          modis_aod.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_airnow
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/airnow2ioda-nc.py
                          -i testinput/airnow_2020081306.dat
                          -s testinput/airnow_sites.dat
                          -o testrun/airnow_2020081306.nc"
                          airnow_2020081306.nc ${IODA_CONV_COMP_TOL_ZERO})

#==============================================================================
# Bufr Ingester tests
#==============================================================================

if(iodaconv_bufr_ENABLED)

  ecbuild_add_test( TARGET  test_iodaconv_bufr_bufrdescription
                    SOURCES bufr/TestBufrDescription.cpp
                    ARGS    testinput/bufr_mhs.yaml
                    LIBS    eckit oops iodaconv::ingester)

  ecbuild_add_test( TARGET  test_iodaconv_bufr_mhs2ioda
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_mhs.yaml"
                            gdas.t18z.1bmhs.tm00.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_hrs2ioda
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_hrs.yaml"
                            gdas.t00z.1bhrs4.tm00.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_filtering
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_filtering.yaml"
                            gdas.t18z.1bmhs.tm00.filtering.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_splitting
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_splitting.yaml"
                            gdas.t18z.1bmhs.tm00.15.seven.split.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_filter_split
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_filter_split.yaml"
                            gdas.t18z.1bmhs.tm00.15.7.filter_split.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_adpsfc2ioda
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_adpsfc.yaml"
                            gdas.t06z.adpsfc.tm00.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_snowadpsfc2ioda
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_snow_adpsfc.yaml"
                            gdas.t06z.adpsfc_snow.tm00.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )


  ecbuild_add_test( TARGET  test_iodaconv_bufr_read_2_dim_blocks
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_read_2_dim_blocks.yaml"
                            bufr_read_2_dim_blocks.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_read_wmo_radiosonde
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_read_wmo_radiosonde.yaml"
                            bufr_read_wmo_radiosonde.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_satwnd_old_format
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                    "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_satwnd_old_format.yaml"
                    NC005066.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_bufr_satwnd_new_format
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                    "${CMAKE_BINARY_DIR}/bin/bufr2ioda.x testinput/bufr_satwnd_new_format.yaml"
                    NC005031.nc ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2ioda.x )
endif()

#==============================================================================
# PrepBUFR and BUFR tests
#==============================================================================

if( iodaconv_pbfortran_ENABLED )

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_gnssro_conv
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2nc_fortran.x
                            -i testinput -o testrun gnssro_kompsat5_20180415_00Z.bufr"
                            gnssro_obs_2018041500.nc4 ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2nc_fortran.x)

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_prepbufr_conv
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2nc_fortran.x
                            -i testinput -o testrun prepbufr.bufr"
                            sondes_obs_2020093018.nc4 ${IODA_CONV_COMP_TOL}
                    DEPENDS bufr2nc_fortran.x)

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_mhs_conv
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/bufr2nc_fortran.x
                            -i testinput -o testrun gdas.t18z.1bmhs.tm00.bufr_d"
                            mhs_metop-b_obs_2020101215.nc4 ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS bufr2nc_fortran.x)

endif()


#==============================================================================
# SatBias tests
#==============================================================================

if( iodaconv_satbias_ENABLED )

  ecbuild_add_test( TARGET  test_iodaconv_satbias_amsua
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/satbias2ioda.x testinput/satbias_converter_amsua.yaml"
                            satbias_amsua_n18.nc4 ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS satbias2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_satbias_cris
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/satbias2ioda.x testinput/satbias_converter_cris.yaml"
                            satbias_cris_npp.nc4 ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS satbias2ioda.x )

  ecbuild_add_test( TARGET  test_iodaconv_satbias_ssmis
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/satbias2ioda.x testinput/satbias_converter_ssmis.yaml"
                            satbias_ssmis_f16.nc4 ${IODA_CONV_COMP_TOL_ZERO}
                    DEPENDS satbias2ioda.x )
endif()


# =============================================================================
# Land product converters
#==============================================================================

if( iodaconv_pygrib_ENABLED )
  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ims_scf
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/ims_scf2ioda.py
                            -i testinput/imssnow_24km.grib2
                            -o testrun/imssnow_scf.nc
                            -m maskout"
                            imssnow_scf.nc ${IODA_CONV_COMP_TOL_ZERO})

  ecbuild_add_test( TARGET  test_${PROJECT_NAME}_afwa_snod
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/afwa_snod2ioda.py
                            -i testinput/afwa_snod_24km.grib
                            -o testrun/afwa_snod.nc
                            -m maskout"
                            afwa_snod.nc ${IODA_CONV_COMP_TOL_ZERO})
endif()

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ghcn_snod
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                  ARGS    netcdf
                          "${CMAKE_BINARY_DIR}/bin/ghcn_snod2ioda.py
                          -i testinput/ghcn_20200228.csv
                          -o testrun/ghcn_snod_20200228.nc
                          -f ${PROJECT_BINARY_DIR}/doc/fix/ghcn-stations.txt
                          -d 20200228
                          -m maskout"
                          ghcn_snod_20200228.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_smap_ssm
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/smap_ssm2ioda.py
                            -i testinput/SMAP_L2_SM_P_NRT_24342_A_20190822T224858_N16023_002.h5
                            -o testrun/smap_ssm.nc
                            -m maskout"
                            smap_ssm.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_smos_ssm
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/smos_ssm2ioda.py
                            -i testinput/smos_l2nrt_ssm.nc
                            -o testrun/smos_ssm.nc
                            -m maskout"
                            smos_ssm.nc ${IODA_CONV_COMP_TOL_ZERO})

ecbuild_add_test( TARGET  test_${PROJECT_NAME}_ascat_ssm
                    TYPE    SCRIPT
                    COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh
                    ARGS    netcdf
                            "${CMAKE_BINARY_DIR}/bin/ascat_ssm2ioda.py
                            -i testinput/ascat_ssm.nc
                            -o testrun/ascat_ssm.nc
                            -m maskout"
                            ascat_ssm.nc ${IODA_CONV_COMP_TOL_ZERO})
