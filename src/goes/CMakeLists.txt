list(APPEND programs
  goes16_to_ioda.py
)

set( SCRIPT_LIB_PATH ${PYIODACONV_BUILD_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_goes16_scripts_deps)
add_custom_target( bin_goes16_scripts ALL
    COMMAND chmod +x ${bin_goes16_scripts_deps}
    DEPENDS ${bin_goes16_scripts_deps} )

# Configure files for install prefix bin
set( SCRIPT_LIB_PATH ${PYIODACONV_INSTALL_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_goes16_scripts_deps)
add_custom_target( install_bin_goes16_scripts ALL
    COMMAND chmod +x ${install_bin_goes16_scripts_deps}
    DEPENDS ${install_bin_goes16_scripts_deps} )

install( PROGRAMS
  ${install_bin_goes16_scripts_deps}
  DESTINATION bin
)

ecbuild_add_test( TARGET iodaconv_goes16_coding_norms
                  TYPE
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
