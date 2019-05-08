list( APPEND libs
  ioda_conv_ncio.py
  collect_sources.py
)

list( APPEND programs
  run-pyflakes.py
  run-mccabe.py
)

SET_TARGETS_DEPS( "${libs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/lib
                   lib_python_scripts_deps)
add_custom_target( lib_python_scripts ALL DEPENDS ${lib_python_scripts_deps} )

# Configure files for local bin
set( SCRIPT_LIB_PATH "${CMAKE_BINARY_DIR}/lib" )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_python_scripts_deps)
add_custom_target( bin_python_scripts ALL
    COMMAND chmod +x ${bin_python_scripts_deps}
    DEPENDS ${bin_python_scripts_deps} )

# Configure files for install prefix bin
set( SCRIPT_LIB_PATH "${CMAKE_INSTALL_PREFIX}/lib" )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_python_scripts_deps)
add_custom_target( install_bin_python_scripts ALL
    COMMAND chmod +x ${install_bin_python_scripts_deps}
    DEPENDS ${install_bin_python_scripts_deps} )

install( PROGRAMS
  ${install_bin_python_scripts_deps}
  DESTINATION bin
)

install( FILES
  ioda_conv_ncio.py
  collect_sources.py
  DESTINATION lib
)

ecbuild_add_test( TARGET iodaconv_lib-python_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
