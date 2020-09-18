list( APPEND libs
  ioda_conv_ncio.py
  ioda_conv_util.py
  collect_sources.py
  orddicts.py
)

list( APPEND programs
  run-pyflakes.py
  run-mccabe.py
)

set_targets_deps( "${libs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${PYIODACONV_BUILD_LIBDIR}
                   lib_python_scripts_deps)

# Configure files for local bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_python_scripts_deps)
execute_process(COMMAND chmod +x ${bin_python_scripts_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_python_scripts_deps)

install( PROGRAMS ${install_bin_python_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

install( FILES ${lib_python_scripts_deps} DESTINATION ${CMAKE_INSTALL_LIBDIR}/pyiodaconv )

add_test(NAME iodaconv_lib-python_coding_norms
         COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                 ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR})
