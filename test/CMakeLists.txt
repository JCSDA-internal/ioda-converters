# (C) Copyright 2019 UCAR.
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
  testinput/giirs_ssec.nc
  testinput/godae_prof.bin
  testinput/godae_ship.bin
  testinput/godae_trak.bin
  testinput/rads_adt.nc
  testinput/smap_sss_rss.nc
  testinput/hgodas_insitu.nc
  testinput/hgodas_sst.nc
  testinput/gsidiag_conv_t_sfc_test.nc
  testinput/gsidiag_conv_uv_testinput.nc
  testinput/gsidiag_amsua_aqua_radiance_test.nc
  testinput/gnssro_kompsat5_20180415_00Z.bufr
  testinput/argoclim_test.nc
  testinput/cryosat2_L2_test.nc
  testinput/viirs_jpss1_oc_l2.nc
  testinput/modis_aqua_oc_l2.nc
  testinput/sondes_obs_2018041500_m.nc4
  testinput/gnssro_obs_2018041500_s.nc4
  testinput/wrfdadiags_goes-16-abi_2018041500.nc
  testinput/b001xx007.20200310.bufr
  testinput/viirs_aod.nc
  testinput/tropomi_no2.nc
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
  testoutput/sfc_tv_obs_2018041500.nc4
  testoutput/satwind_obs_2018041500.nc4
  testoutput/amsua_aqua_obs_2018041500.nc4
  testoutput/gnssro_kompsat5_2018041500.nc4
  testoutput/argoclim.nc
  testoutput/cryosat2_L2.nc
  testoutput/viirs_jpss1_oc_l2.nc
  testoutput/modis_aqua_oc_l2.nc
  testoutput/aod_viirs_obs_2018041500_s.nc4
  testoutput/abi_g16_obs_2018041500.nc4
  testoutput/ioda.NC001007.2020031012.nc
  testoutput/viirs_aod.nc
  testoutput/tropomi_no2.nc
)

# create test directories and make links to the input files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testoutput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testrun)
foreach(FILENAME ${test_input} ${test_output})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

#===============================================================================
# The following tests use nccmp to check the difference between the output
#  file, and the reference copy of what the ouput file should look like.
# The first argument to the script, wrapped in quotes, is the command to execute
#  the ioda converter.
# The second argument is the name of the output file to compare (one copy to be
#  created in testrun/ by the converter, and one copy already present in in
#  testoutput/)
#===============================================================================

#===============================================================================
# Marine converters
#===============================================================================

add_test(NAME test_iodaconv_gds2_sst_l2p
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/gds2_sst2ioda.py
            -i testinput/gds2_sst_l2p.nc
            -o testrun/gds2_sst_l2p.nc
            -d 2018041512
            -t 0.5"
            gds2_sst_l2p.nc)

add_test(NAME test_iodaconv_gds2_sst_l3u
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/gds2_sst2ioda.py
            -i testinput/gds2_sst_l3u.nc
            -o testrun/gds2_sst_l3u.nc
            -d 2018041512
            -t 0.5"
            gds2_sst_l3u.nc)

add_test(NAME test_iodaconv_smap_sss
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/smap_sss2ioda.py
            -i testinput/smap_sss_rss.nc
            -o testrun/smap_sss_rss.nc
            -d 2018041512"
            smap_sss_rss.nc)

add_test(NAME test_iodaconv_rads_adt
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/rads_adt2ioda.py
            -i testinput/rads_adt.nc
            -o testrun/rads_adt.nc
            -d 2018041512"
            rads_adt.nc)

add_test(NAME test_iodaconv_godae_prof
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/godae_profile2ioda.py
            -i testinput/godae_prof.bin
            -o testrun/godae_prof.nc
            -d 1998092212"
            godae_prof.nc)

add_test(NAME test_iodaconv_godae_ship
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/godae_ship2ioda.py
            -i testinput/godae_ship.bin
            -o testrun/godae_ship.nc
            -d 1998090112"
            godae_ship.nc)

add_test(NAME test_iodaconv_godae_trak
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/godae_trak2ioda.py
            -i testinput/godae_trak.bin
            -o testrun/godae_trak.nc
            -d 2004070812"
            godae_trak.nc)

add_test(NAME test_iodaconv_hgodas_insitu
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/hgodas_insitu2ioda.py
            -i testinput/hgodas_insitu.nc
            -o testrun/hgodas_insitu.nc
            -d 2018041512"
            hgodas_insitu.nc)

add_test(NAME test_iodaconv_hgodas_sst
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/hgodas_sst2ioda.py
            -i testinput/hgodas_sst.nc
            -o testrun/hgodas_sst.nc
            -d 2018041512"
            hgodas_sst.nc)

add_test(NAME test_iodaconv_argoclim
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/argoClim2ioda.py
            -i testinput/argoclim_test.nc
            -o testrun/argoclim.nc
            -d 2019101600"
            argoclim.nc)

add_test(NAME test_iodaconv_cryosat2
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/cryosat_ice2ioda.py
            -i testinput/cryosat2_L2_test.nc
            -o testrun/cryosat2_L2.nc
            -d 2019092112"
            cryosat2_L2.nc)


#===============================================================================
# MISC converters
#===============================================================================

add_test(NAME test_iodaconv_viirs_jpss1_oc_l2
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/viirs_modis_oc2ioda.py
            -i testinput/viirs_jpss1_oc_l2.nc
            -o testrun/viirs_jpss1_oc_l2.nc
            -d 2018041512
            -t 0.5"
            viirs_jpss1_oc_l2.nc)

add_test(NAME test_iodaconv_modis_aqua_oc_l2
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/viirs_modis_oc2ioda.py
            -i testinput/modis_aqua_oc_l2.nc
            -o testrun/modis_aqua_oc_l2.nc
            -d 2018041512
            -t 0.5"
            modis_aqua_oc_l2.nc)

add_test(NAME test_iodaconv_ncep_bufr
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/ncep_classes.py
            -p testinput/
            -i b001xx007.20200310.bufr
            -o testrun/ioda.NC001007.2020031012.nc
            -d 2020031012
            -m 5
            -l NC001007.yaml
            -ot NC001007"
            ioda.NC001007.2020031012.nc)


#===============================================================================
# SSEC GIIRS converters
#===============================================================================

add_test( NAME test_iodaconv_giirs_tb
          COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
             "${CMAKE_BINARY_DIR}/bin/giirs_ssec2ioda.py
             -i testinput/giirs_ssec.nc
             -o testrun/giirs_ssec_ioda.nc4
             -d 2020080100"
             giirs_ssec_ioda.nc4)

add_test(NAME test_iodaconv_giirs_lw
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/giirs_lw2ioda.py
            -i testinput/giirs_fy4a-test.nc
            -o testrun/giirs_fy4a_obs_2017030208.nc4
            -d 2017030208"
            giirs_fy4a_obs_2017030208.nc4)

#===============================================================================
# GSI ncdiag converters
#===============================================================================

add_test(NAME test_iodaconv_gsidiag_conv_uv
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
            -i testinput/gsidiag_conv_uv_testinput.nc
            -o testrun/
            -t conv
            -p satwind"
            satwind_obs_2018041500.nc4)

add_test(NAME test_iodaconv_gsidiag_conv
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
            -i testinput/gsidiag_conv_t_sfc_test.nc
            -o testrun/
            -t conv
            -p sfc"
            sfc_tv_obs_2018041500.nc4)

add_test(NAME test_iodaconv_gsidiag_rad
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
            -i testinput/gsidiag_amsua_aqua_radiance_test.nc
            -o testrun/
            -t rad"
            amsua_aqua_obs_2018041500.nc4)


#===============================================================================
# WRFDA ncdiag converter
#===============================================================================

add_test(NAME test_iodaconv_wrfdadiag_rad
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/test_wrfdadiag.py
            -i testinput/wrfdadiags_goes-16-abi_2018041500.nc
            -o testrun/
            -t rad"
            abi_g16_obs_2018041500.nc4)


#===============================================================================
# GNSSRO BUFR converter
#===============================================================================

if( iodaconv_gnssro_ENABLED )
  add_test(NAME test_iodaconv_gnssro_bufr_conv
           COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
                "${CMAKE_BINARY_DIR}/bin/gnssro_bufr2ioda
                2018041500
                testinput/gnssro_kompsat5_20180415_00Z.bufr
                testrun/gnssro_kompsat5_2018041500.nc4"
                gnssro_kompsat5_2018041500.nc4 )
endif()


#==============================================================================
# Atmospheric Composition converters
#==============================================================================

add_test(NAME test_iodaconv_viirs_aod
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/viirs_aod2ioda.py
            -i testinput/viirs_aod.nc
            -o testrun/viirs_aod.nc
            -m nesdis
            -k maskout
            -t 0.0"
            viirs_aod.nc)

add_test(NAME test_iodaconv_tropomi_no2
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_comp.sh netcdf
            "${CMAKE_BINARY_DIR}/bin/tropomi_no2_nc2ioda.py
            -i testinput/tropomi_no2.nc
            -o testrun/tropomi_no2.nc"
            tropomi_no2.nc)
