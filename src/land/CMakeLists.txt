list(APPEND programs
  ghcn_snod2ioda.py
  smap_ssm2ioda.py
  smos_ssm2ioda.py
  ascat_ssm2ioda.py
  imsfv3_scf2ioda.py
  owp_snow_obs.py
  madis_snow2ioda.py
)

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_land_scripts_deps)
execute_process(COMMAND chmod +x ${bin_land_scripts_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_land_scripts_deps)

install( PROGRAMS ${install_bin_land_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

add_test(NAME iodaconv_land_coding_norms
         COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                 ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
