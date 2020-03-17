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
  testinput/godae_prof.bin
  testinput/godae_ship.bin
  testinput/godae_trak.bin
  testinput/rads_adt.nc
  testinput/smap_sss_rss.nc
  testinput/hgodas_insitu.nc
  testinput/hgodas_sst.nc
  testinput/gsidiag_conv_t_sfc_test.nc
  testinput/gsidiag_amsua_aqua_radiance_test.nc
  testinput/odb_sonde_16019.odb
  testinput/gnssro_kompsat5_20180415_00Z.bufr
  testinput/argoclim_test.nc
  testinput/cryosat2_L2_test.nc
  testinput/CS_OFFL_SIR_GDR_2__20160229T132459_20160229T150412_C001.DBL
  testinput/sondes_obs_2018041500_m.nc4
  testinput/gnssro_obs_2018041500_s.nc4
  testinput/wrfdadiags_goes-16-abi_2018041500.nc
)

list( APPEND test_output
  testoutput/gds2_sst_l2p.nc
  testoutput/gds2_sst_l3u.nc
  testoutput/godae_prof.nc
  testoutput/godae_ship.nc
  testoutput/godae_trak.nc
  testoutput/rads_adt.nc
  testoutput/smap_sss_rss.nc
  testoutput/hgodas_insitu.nc
  testoutput/hgodas_sst.nc
  testoutput/sfc_tv_obs_2018041500.nc4
  testoutput/amsua_aqua_obs_2018041500.nc4
  testoutput/odb_sonde_16019.nc4
  testoutput/gnssro_kompsat5_2018041500.nc4
  testoutput/argoclim.nc
  testoutput/cryosat2_L2.nc
  testoutput/cryosat2_L2_DBL.nc
  testoutput/sondes_obs_2018041500_m.odb
  testoutput/gnssro_obs_2018041500_s.odb
  testoutput/abi_g16_obs_2018041500.nc4
)

# create test directories and make links to the input files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testoutput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testrun)
foreach(FILENAME ${test_input} ${test_output} iodaconv_comp.sh)
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

# create list of library paths for finding shared libraries
string( REPLACE ";" ":"
        CTEST_LIBRARY_PATH
        "${CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES}" )
message( STATUS "Library path for testing: ${CTEST_LIBRARY_PATH}" )

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

ecbuild_add_test( TARGET test_iodaconv_gds2_sst_l2p
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/gds2_sst2ioda.py
                       -i testinput/gds2_sst_l2p.nc
                       -o testrun/gds2_sst_l2p.nc
                       -d 2018041512
                       -t 0.5"
                      gds2_sst_l2p.nc)

ecbuild_add_test( TARGET test_iodaconv_gds2_sst_l3u
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/gds2_sst2ioda.py
                       -i testinput/gds2_sst_l3u.nc
                       -o testrun/gds2_sst_l3u.nc
                       -d 2018041512
                       -t 0.5"
                      gds2_sst_l3u.nc)

ecbuild_add_test( TARGET test_iodaconv_smap_sss
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/smap_sss2ioda.py
                       -i testinput/smap_sss_rss.nc
                       -o testrun/smap_sss_rss.nc
                       -d 2018041512"
                      smap_sss_rss.nc)

ecbuild_add_test( TARGET test_iodaconv_rads_adt
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/rads_adt2ioda.py
                       -i testinput/rads_adt.nc
                       -o testrun/rads_adt.nc
                       -d 2018041512"
                      rads_adt.nc)

ecbuild_add_test( TARGET test_iodaconv_godae_prof
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/godae_profile2ioda.py
                       -i testinput/godae_prof.bin
                       -o testrun/godae_prof.nc
                       -d 1998092212"
                      godae_prof.nc)

ecbuild_add_test( TARGET test_iodaconv_godae_ship
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/godae_ship2ioda.py
                       -i testinput/godae_ship.bin
                       -o testrun/godae_ship.nc
                       -d 1998090112"
                      godae_ship.nc)

ecbuild_add_test( TARGET test_iodaconv_godae_trak
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/godae_trak2ioda.py
                       -i testinput/godae_trak.bin
                       -o testrun/godae_trak.nc
                       -d 2004070812"
                      godae_trak.nc)

ecbuild_add_test( TARGET test_iodaconv_hgodas_insitu
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/hgodas_insitu2ioda.py
                       -i testinput/hgodas_insitu.nc
                       -o testrun/hgodas_insitu.nc
                       -d 2018041512"
                      hgodas_insitu.nc)

ecbuild_add_test( TARGET test_iodaconv_hgodas_sst
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/hgodas_sst2ioda.py
                       -i testinput/hgodas_sst.nc
                       -o testrun/hgodas_sst.nc
                       -d 2018041512"
                      hgodas_sst.nc)

ecbuild_add_test( TARGET test_iodaconv_argoclim
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/argoClim2ioda.py
                       -i testinput/argoclim_test.nc
                       -o testrun/argoclim.nc
                       -d 2019101600"
                      argoclim.nc)

ecbuild_add_test( TARGET test_iodaconv_cryosat2
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/cryosat_ice2ioda.py
                       -i testinput/cryosat2_L2_test.nc
                       -o testrun/cryosat2_L2.nc
                       -d 2019092112"
                      cryosat2_L2.nc)

ecbuild_add_test( TARGET test_iodaconv_cryosat2_DBL
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/cryosat_ice2ioda_DBL.py
                       -i testinput/CS_OFFL_SIR_GDR_2__20160229T132459_20160229T150412_C001.DBL
                       -o testrun/cryosat2_L2_DBL.nc
                       -d 2016022912"
                      cryosat2_L2_DBL.nc)

#===============================================================================
# GSI ncdiag converters
#===============================================================================

ecbuild_add_test( TARGET test_iodaconv_gsidiag_conv
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
                       -i testinput/gsidiag_conv_t_sfc_test.nc
                       -o testrun/
                       -t conv
                       -p sfc"
                      sfc_tv_obs_2018041500.nc4)

ecbuild_add_test( TARGET test_iodaconv_gsidiag_rad
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/test_gsidiag.py
                       -i testinput/gsidiag_amsua_aqua_radiance_test.nc
                       -o testrun/
                       -t rad"
                      amsua_aqua_obs_2018041500.nc4)

#===============================================================================
# WRFDA ncdiag converter
#===============================================================================

ecbuild_add_test( TARGET test_iodaconv_wrfdadiag_rad
                  TYPE SCRIPT
                  COMMAND iodaconv_comp.sh
                  ARGS netcdf
                       "${CMAKE_BINARY_DIR}/bin/test_wrfdadiag.py
                       -i testinput/wrfdadiags_goes-16-abi_2018041500.nc
                       -o testrun/
                       -t rad"
                      abi_g16_obs_2018041500.nc4)

#===============================================================================
# GNSSRO BUFR converter
#===============================================================================

if( HAVE_BUFRLIB_AND_NETCDF )
  ecbuild_add_test( TARGET test_iodaconv_gnssro_bufr_conv
                    TYPE SCRIPT
                    COMMAND iodaconv_comp.sh
                    ARGS netcdf
                         "${CMAKE_BINARY_DIR}/bin/gnssro_bufr2ioda
                         2018041500
                         testinput/gnssro_kompsat5_20180415_00Z.bufr
                         testrun/gnssro_kompsat5_2018041500.nc4"
                        gnssro_kompsat5_2018041500.nc4
                    ENVIRONMENT CTEST_LIBRARY_PATH=${CTEST_LIBRARY_PATH} )
endif()  
#===============================================================================
# ODB2 converter
#===============================================================================
if (ODB_API_HAVE_PYTHON AND PYTHON_VERSION_STRING VERSION_LESS 3)
  ecbuild_add_test( TARGET test_iodaconv_odb2_conv
                    TYPE SCRIPT
                    COMMAND iodaconv_comp.sh
                    ARGS netcdf
                         "${CMAKE_BINARY_DIR}/bin/odbapi2nc.py -c
                         testinput/odb_sonde_16019.odb
                         ${CMAKE_BINARY_DIR}/etc/pyiodaconv/MO_radiosonde_Definition.yaml
                         testrun/odb_sonde_16019.nc4"
                        odb_sonde_16019.nc4)
endif ()

#===============================================================================
# ODC API based ODB converter
#===============================================================================
if (odc_FOUND)
  ecbuild_add_test( TARGET test_iodaconv_nc2odb_tool_new_dt
                    TYPE SCRIPT
                    COMMAND iodaconv_comp.sh
                    ARGS odb
                         "${CMAKE_BINARY_DIR}/bin/nc2odb.py -c
                         testinput/sondes_obs_2018041500_m.nc4
                         testrun/sondes_obs_2018041500_m.odb"
                        sondes_obs_2018041500_m.odb)

  ecbuild_add_test( TARGET test_iodaconv_nc2odb_tool_old_dt
                    TYPE SCRIPT
                    COMMAND iodaconv_comp.sh
                    ARGS odb
                         "${CMAKE_BINARY_DIR}/bin/nc2odb.py -c
                         testinput/gnssro_obs_2018041500_s.nc4
                         testrun/gnssro_obs_2018041500_s.odb"
                        gnssro_obs_2018041500_s.odb)
endif ()
