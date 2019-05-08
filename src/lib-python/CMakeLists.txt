list( APPEND programs
  ioda_conv_ncio.py
  ioda_conv_util.py
  collect_sources.py
  run-pyflakes.py
  run-mccabe.py
  orddicts.py
)

SET_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}
                   lib_python_scripts_deps)
add_custom_target( lib_python_scripts ALL DEPENDS ${lib_python_scripts_deps} )

install (PROGRAMS
  ${programs}
  DESTINATION bin)

ecbuild_add_test( TARGET iodaconv_lib-python_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
