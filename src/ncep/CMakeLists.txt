list( APPEND libs
  bufr2ncCommon.py
  bufr2ncObsTypes.py
)

list( APPEND programs
  bufr2nc.py
  ncep_classes.py
)

SET_TARGETS_DEPS( "${libs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${PYIODACONV_BUILD_LIBDIR}
                   ncep_scripts_deps)
add_custom_target( ncep_scripts ALL DEPENDS ${ncep_scripts_deps} )

set( SCRIPT_LIB_PATH ${PYIODACONV_BUILD_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_ncep_scripts_deps)
add_custom_target( bin_ncep_scripts ALL
    COMMAND chmod +x ${bin_ncep_scripts_deps}
    DEPENDS ${bin_ncep_scripts_deps} )

# Configure files for install prefix bin
set( SCRIPT_LIB_PATH ${PYIODACONV_INSTALL_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_ncep_scripts_deps)
add_custom_target( install_bin_ncep_scripts ALL
    COMMAND chmod +x ${install_bin_ncep_scripts_deps}
    DEPENDS ${install_bin_ncep_scripts_deps} )

install( PROGRAMS
  ${install_bin_ncep_scripts_deps}
  DESTINATION bin
)

install( FILES
  ${ncep_scripts_deps}
  DESTINATION lib/pyiodaconv
)

ecbuild_add_test( TARGET iodaconv_ncep_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
