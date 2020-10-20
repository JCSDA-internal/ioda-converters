list(APPEND libs gsi_ncdiag.py)

list( APPEND programs
  proc_gsi_ncdiag.py
  test_gsidiag.py
  combine_conv.py
  subset_files.py)


set_targets_deps( "${libs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${PYIODACONV_BUILD_LIBDIR}
                   gsi_ncdiag_lib_deps )

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   gsi_ncdiag_bin_deps)
execute_process(COMMAND chmod +x ${gsi_ncdiag_bin_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   gsi_ncdiag_bin_install_deps)

install( PROGRAMS ${gsi_ncdiag_bin_install_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

install( FILES ${gsi_ncdiag_lib_deps} DESTINATION ${CMAKE_INSTALL_LIBDIR}/pyiodaconv )

add_test(NAME iodaconv_gsi-ncdiag_coding_norms
         COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                 ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
