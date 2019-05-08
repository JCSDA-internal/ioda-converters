list(APPEND programs
  ioda_conv_util.py
)

SET_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}
                   utils_scripts_deps)
add_custom_target( utils_scripts ALL DEPENDS ${utils_scripts_deps} )

install (PROGRAMS
  ${programs}
  DESTINATION bin)


ecbuild_add_test( TARGET iodaconv_utils_coding_norms
                  TYPE
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
