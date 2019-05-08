list( APPEND programs
  bufr2nc.py
  bufr2ncCommon.py
  bufr2ncObsTypes.py
  ncep_classes.py
)

SET_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}
                   ncep_scripts_deps)
add_custom_target( ncep_scripts ALL DEPENDS ${ncep_scripts_deps} )

install (PROGRAMS
  ${programs}
  DESTINATION bin)

ecbuild_add_test( TARGET iodaconv_ncep_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
