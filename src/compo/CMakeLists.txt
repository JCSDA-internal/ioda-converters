list(APPEND programs
  ompsnm_o3_nc2ioda.py
  ompsnm_o3_nc2ioda.py
  omi_o3_nc2ioda.py
  mls_o3_nc2ioda.py
  viirs_aod2ioda.py
  tropomi_no2_co_nc2ioda.py
  tempo_nc2ioda.py
  mopitt_co_nc2ioda.py
  tropess_co_nc2ioda.py
  airnow2ioda_nc.py
  modis_aod2ioda.py
  aeronet_aod2ioda.py
  aeronet_aaod2ioda.py
  viirs_biaswriter.py
  omps_o3_nm_h52ioda.py
  viirs_l1bnc2ioda.py
)

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_compo_scripts_deps)
execute_process(COMMAND chmod +x ${bin_compo_scripts_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_compo_scripts_deps)

install( PROGRAMS ${install_bin_compo_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_compo_coding_norms
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS    ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
