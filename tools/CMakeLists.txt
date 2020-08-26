list( APPEND programs
    ${PROJECT_NAME}_lint.sh
    ${PROJECT_NAME}_cpplint.py
)

SET_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   tools_scripts_deps)
add_custom_target( tools_scripts ALL DEPENDS ${tools_scripts_deps} )

install (PROGRAMS
  ${programs}
  DESTINATION bin)
