list(APPEND libs gsi_ncdiag.py)

list( APPEND programs
  proc_gsi_ncdiag.py
  test_gsidiag.py
  combine_conv.py
  subset_files.py)


SET_TARGETS_DEPS( "${libs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${PYIODACONV_BUILD_LIBDIR}
                   gsi_ncdiag_lib_deps )

set( SCRIPT_LIB_PATH ${PYIODACONV_BUILD_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   gsi_ncdiag_bin_deps)
execute_process(COMMAND chmod +x ${gsi_ncdiag_bin_deps})

# Configure files for install prefix bin
file(RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   gsi_ncdiag_bin_install_deps)

install( PROGRAMS ${gsi_ncdiag_bin_install_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

install( FILES ${gsi_ncdiag_lib_deps} DESTINATION ${CMAKE_INSTALL_LIBDIR}/pyiodaconv )

ecbuild_add_test( TARGET iodaconv_gsi-ncdiag_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
