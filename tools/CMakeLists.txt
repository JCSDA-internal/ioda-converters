list( APPEND programs
    lint.sh
)

SET_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}
                   tools_scripts_deps)
add_custom_target( tools_scripts ALL DEPENDS ${tools_scripts_deps} )

install (PROGRAMS
  ${programs}
  DESTINATION bin)
