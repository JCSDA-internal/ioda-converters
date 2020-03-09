list( APPEND libs
  ioda_conv_ncio.py
  ioda_conv_util.py
  collect_sources.py
  orddicts.py
  read_cryosat_L2.py
)

list( APPEND programs
  run-pyflakes.py
  run-mccabe.py
)

SET_TARGETS_DEPS( "${libs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${PYIODACONV_BUILD_LIBDIR}
                   lib_python_scripts_deps)
add_custom_target( lib_python_scripts ALL DEPENDS ${lib_python_scripts_deps} )

# Configure files for local bin
set( SCRIPT_LIB_PATH ${PYIODACONV_BUILD_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_python_scripts_deps)
add_custom_target( bin_python_scripts ALL
    COMMAND chmod +x ${bin_python_scripts_deps}
    DEPENDS ${bin_python_scripts_deps} )

# Configure files for install prefix bin
set( SCRIPT_LIB_PATH ${PYIODACONV_INSTALL_LIBDIR} )
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
  ${lib_python_scripts_deps}
  DESTINATION lib/pyiodaconv
)

ecbuild_add_test( TARGET iodaconv_lib-python_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
