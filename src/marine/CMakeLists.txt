list(APPEND programs
  cryosat_ice2ioda.py
  cryosat_ice2ioda_DBL.py
  emc_ice2ioda.py
  gds2_sst2ioda.py
  gmao_obs2ioda.py
  godae_profile2ioda.py
  godae_ship2ioda.py
  godae_trak2ioda.py
  hgodas_adt2ioda.py
  hgodas_insitu2ioda.py
  hgodas_sst2ioda.py
  rads_adt2ioda.py
  smap_sss2ioda.py
  argoClim2ioda.py
  viirs_modis_oc2ioda.py
)

set( SCRIPT_LIB_PATH ${PYIODACONV_BUILD_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_marine_scripts_deps)
add_custom_target( bin_marine_scripts ALL
    COMMAND chmod +x ${bin_marine_scripts_deps}
    DEPENDS ${bin_marine_scripts_deps} )

# Configure files for install prefix bin
set( SCRIPT_LIB_PATH ${PYIODACONV_INSTALL_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_marine_scripts_deps)
add_custom_target( install_bin_marine_scripts ALL
    COMMAND chmod +x ${install_bin_marine_scripts_deps}
    DEPENDS ${install_bin_marine_scripts_deps} )

install( PROGRAMS
  ${install_bin_marine_scripts_deps}
  DESTINATION bin
)

ecbuild_add_test( TARGET iodaconv_marine_coding_norms
                  TYPE
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
