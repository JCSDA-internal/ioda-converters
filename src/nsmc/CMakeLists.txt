list(APPEND programs
  giirs_lw2ioda.py
)

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_nsmc_scripts_deps)
execute_process(COMMAND chmod +x ${bin_nsmc_scripts_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_nsmc_scripts_deps)

install( PROGRAMS ${install_bin_nsmc_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

add_test(NAME iodaconv_nsmc_coding_norms
         COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_lint.sh
                 ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR})
