list(APPEND programs
  giirs_lw2ioda.py
)

set( SCRIPT_LIB_PATH ${PYIODACONV_BUILD_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_nsmc_scripts_deps)
add_custom_target( bin_nsmc_scripts ALL
    COMMAND chmod +x ${bin_nsmc_scripts_deps}
    DEPENDS ${bin_nsmc_scripts_deps} )

# Configure files for install prefix bin
set( SCRIPT_LIB_PATH ${PYIODACONV_INSTALL_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_nsmc_scripts_deps)
add_custom_target( install_bin_nsmc_scripts ALL
    COMMAND chmod +x ${install_bin_nsmc_scripts_deps}
    DEPENDS ${install_bin_nsmc_scripts_deps} )

install( PROGRAMS
  ${install_bin_nsmc_scripts_deps}
  DESTINATION bin
)

ecbuild_add_test( TARGET iodaconv_nsmc_coding_norms
                  TYPE
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
