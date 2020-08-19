list(APPEND programs
  giirs_lw2ioda.py
)

set( SCRIPT_LIB_PATH ${PYIODACONV_BUILD_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_nsmc_scripts_deps)
execute_process(COMMAND chmod +x ${bin_nsmc_scripts_deps})

# Configure files for install prefix bin
file(RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_nsmc_scripts_deps)

install( PROGRAMS ${install_bin_nsmc_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

ecbuild_add_test( TARGET iodaconv_nsmc_coding_norms
                  TYPE
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
