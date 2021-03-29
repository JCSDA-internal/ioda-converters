list(APPEND programs
  cryosat_ice2ioda.py
  emc_ice2ioda.py
  gds2_sst2ioda.py
  gmao_obs2ioda.py
  godae_profile2ioda.py
  godae_ship2ioda.py
  godae_trak2ioda.py
  hgodas_adt2ioda.py
  hgodas_insitu2ioda.py
  hgodas_sst2ioda.py
  glider2ioda.py
  ndbc_hfradar2ioda.py
  rads_adt2ioda.py
  smap_sss2ioda.py
  argoClim2ioda.py
  viirs_modis_l2_oc2ioda.py
  viirs_modis_l3_oc2ioda.py
)

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_marine_scripts_deps)
execute_process(COMMAND chmod +x ${bin_marine_scripts_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${programs}"
            ${CMAKE_CURRENT_SOURCE_DIR}
            ${CMAKE_BINARY_DIR}/install-bin
            install_bin_marine_scripts_deps)

install( PROGRAMS ${install_bin_marine_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_marine_coding_norms
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS    ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
