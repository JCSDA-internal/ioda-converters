list(APPEND programs
  owp_snow_obs_pkl_2_ioda.py           
)

set( SCRIPT_LIB_PATH ${PYIODACONV_BUILD_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_owp_scripts_deps)
add_custom_target( bin_owp_scripts ALL
    COMMAND chmod +x ${bin_owp_scripts_deps}
    DEPENDS ${bin_owp_scripts_deps} )

# Configure files for install prefix bin
set( SCRIPT_LIB_PATH ${PYIODACONV_INSTALL_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_owp_scripts_deps)
add_custom_target( install_bin_owp_scripts ALL
    COMMAND chmod +x ${install_bin_owp_scripts_deps}
    DEPENDS ${install_bin_owp_scripts_deps} )

install( PROGRAMS
  ${install_bin_owp_scripts_deps}
  DESTINATION bin
)

add_test( TARGET iodaconv_owp_coding_norms
                  TYPE
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
