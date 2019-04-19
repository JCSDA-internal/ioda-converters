list( APPEND programs
  odbapi2nc.py
  odbapi2json.py
  var_convert.py)


SET_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}
                   odb2_scripts_deps)
add_custom_target( odb2_scripts ALL DEPENDS ${odb2_scripts_deps} )

install (PROGRAMS
  odbapi2nc.py
  odbapi2json.py
  var_convert.py
  DESTINATION bin)

ecbuild_add_test( TARGET iodaconv_met-office_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR} )
