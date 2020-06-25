list( APPEND libs
  bufr2ncCommon.py
  bufr2ncObsTypes.py
)

list( APPEND programs
  bufr2nc.py
  ncep_classes.py
)

list( APPEND config
  bufr2ioda.yaml
  NC001001.yaml
  NC001002.yaml
  NC001003.yaml
  NC001007.yaml
  NC001013.yaml
  NC001101.yaml
  NC001102.yaml
  NC001103.yaml
  NC001113.yaml
  NC031117.yaml
  NC031118.yaml
  NC031119.yaml
  NC031120.yaml
  NC031121.yaml
  NC031122.yaml
  NC031123.yaml
  NC031124.yaml
  NC031125.yaml
  NC031126.yaml
  NC031127.yaml
  NC031128.yaml
  NC031129.yaml
  NC031130.yaml
)

SET_TARGETS_DEPS( "${libs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${PYIODACONV_BUILD_LIBDIR}
                   ncep_scripts_deps)
add_custom_target( ncep_scripts ALL DEPENDS ${ncep_scripts_deps} )

SET_TARGETS_DEPS( "${config}"
                   ${CMAKE_CURRENT_SOURCE_DIR}/config
                   ${PYIODACONV_BUILD_LIBDIR}/config
                   config_ncep_scripts_deps)
add_custom_target( config_ncep_scripts ALL DEPENDS ${config_ncep_scripts_deps} )

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

install( FILES
  ${config_ncep_scripts_deps}
  DESTINATION lib/pyiodaconv/config
)

ecbuild_add_test( TARGET iodaconv_ncep_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
