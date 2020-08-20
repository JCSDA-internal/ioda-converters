list( APPEND libs
  bufr2ncCommon.py
  bufr2ncObsTypes.py
)

list( APPEND programs
  bufr2nc.py
  ncep_classes.py
)

set_targets_deps( "${libs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${PYIODACONV_BUILD_LIBDIR}
                   ncep_scripts_deps)

file(MAKE_DIRECTORY ${PYIODACONV_BUILD_LIBDIR})
execute_process(COMMAND cmake -E create_symlink ${CMAKE_CURRENT_SOURCE_DIR}/config ${PYIODACONV_BUILD_LIBDIR}/config)

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_ncep_scripts_deps)
execute_process(COMMAND chmod +x ${bin_ncep_scripts_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_ncep_scripts_deps)

install( PROGRAMS ${install_bin_ncep_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

install( FILES ${ncep_scripts_deps} DESTINATION ${CMAKE_INSTALL_LIBDIR}/pyiodaconv )

install( DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/config DESTINATION ${CMAKE_INSTALL_LIBDIR}/pyiodaconv )

add_test(NAME iodaconv_ncep_coding_norms
         COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                 ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR})
