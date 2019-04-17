list( APPEND programs
  odbapi2nc.py
  odbapi2json.py
  var_convert.py
  DESTINATION bin)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  odbapi2nc.py
  odbapi2json.py
  var_convert.py
  DESTINATION bin)

ecbuild_add_test( TARGET iodaconv_met-office_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR} )
